# Do invertebrate responses to grazer exclusion track the plant-biomass response?
# Run from the repo root:  Rscript R/02_plant_biomass_link.R
#
# Uses studies that report both above-ground plant biomass (Ecosystem_functions
# sheet) and an invertebrate response. Three response sets: Overall (abundance +
# richness pooled), Abundance, Richness.
#
# 1. Plant lnRR (exclosure / grazed) per row, aggregated to one value per study
#    (inverse-variance weighted, compound symmetry with r = 0.5 among a study's
#    effects).
# 2. Linear correlation: Pearson r between study-level plant lnRR and study-level
#    invertebrate lnRR (invertebrate effects aggregated the same way), one point
#    per study, unweighted. Spearman rho is reported alongside as a check that a
#    few studies do not drive r.
# 3. Meta-regression: study-level plant lnRR as a moderator of every invertebrate
#    effect size in the multilevel model of R/01 (location / study / effect size,
#    shared-control covariance), with cluster-robust (CR2) inference. In the
#    Overall set the model also has a separate intercept for abundance and
#    richness, so the slope is the common plant-invertebrate relationship.

source("R/00_functions.R")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

PLANT_GROUP <- "Plant biomass - above-ground"
SETS <- list(Overall = c("Abundance", "Richness"), Abundance = "Abundance", Richness = "Richness")
RHO <- 0.5
MIN_STUDIES <- 5

# ------------------------------------------------------------------ plant lnRR
raw <- read_excel(DATA_FILE, sheet = "Ecosystem_functions", .name_repair = "unique_quiet")
ef <- tibble(
  Study_ID = trimws(raw$Study_ID), Author_Year = trimws(raw$Author_Year), Function_group = raw$Function_group,
  variation = raw$variation,
  m_g = as.numeric(raw$Mean_Grazed),  se_g = as.numeric(raw$SD_or_SE_Grazed),  n_g = as.numeric(raw$n_Grazed),
  m_c = as.numeric(raw$Mean_Control), se_c = as.numeric(raw$SD_or_SE_Control), n_c = as.numeric(raw$n_Control)
) |>
  filter(!is.na(Study_ID), Function_group %in% PLANT_GROUP)
n0 <- nrow(ef)
ef <- ef |> filter(!is.na(n_g), !is.na(n_c), !is.na(se_g), !is.na(se_c), !is.na(variation), m_g > 0, m_c > 0) |>
  mutate(sd_g = se_to_sd(se_g, n_g, variation), sd_c = se_to_sd(se_c, n_c, variation))
message(sprintf("Plant biomass: %d of %d rows usable (%d studies)", nrow(ef), n0, n_distinct(ef$Study_ID)))
ef <- escalc("ROM", m1i = m_c, sd1i = sd_c, n1i = n_c, m2i = m_g, sd2i = sd_g, n2i = n_g, data = ef)
write.csv(ef, "output/tables/effect_sizes_plant_biomass.csv", row.names = FALSE)

pa <- aggregate(ef, cluster = Study_ID, struct = "CS", rho = RHO)
plant <- data.frame(Study_ID = pa$Study_ID, plant_lnRR = as.numeric(pa$yi), plant_vi = as.numeric(pa$vi),
                    plant_k = as.integer(table(ef$Study_ID)[pa$Study_ID]))
# context: mean plant-biomass response (random-effects model on the study-level values)
pm <- rma(plant_lnRR, plant_vi, data = plant, test = "knha")
plant_summary <- data.frame(n_studies = nrow(plant), k = nrow(ef), estimate = round(pm$b[1], 3),
                            ci_lb = round(pm$ci.lb, 3), ci_ub = round(pm$ci.ub, 3), p = signif(pm$pval, 3),
                            pct_change = pct(pm$b[1]), pct_ci_lb = pct(pm$ci.lb), pct_ci_ub = pct(pm$ci.ub),
                            min_lnRR = round(min(plant$plant_lnRR), 3), max_lnRR = round(max(plant$plant_lnRR), 3))
write.csv(plant_summary, "output/tables/plant_biomass_overall.csv", row.names = FALSE)

# ------------------------------------------------------------------ invertebrates
inv <- load_inverts() |> compute_lnrr()

cor_rows <- list(); mr_rows <- list(); pts <- list(); fit <- list()
for (set in names(SETS)) {
  d <- inv |> filter(Response_group %in% SETS[[set]]) |> inner_join(plant, by = "Study_ID")
  d$es_id <- seq_len(nrow(d))
  ns <- n_distinct(d$Study_ID)
  if (ns < MIN_STUDIES) { message(sprintf("skip %s: only %d studies", set, ns)); next }

  # one invertebrate value per study
  ia <- aggregate(escalc(yi = d$yi, vi = d$vi, data = d[, c("Study_ID", "plant_lnRR")]),
                  cluster = Study_ID, struct = "CS", rho = RHO)
  sp <- data.frame(set = set, Study_ID = ia$Study_ID, inv_lnRR = as.numeric(ia$yi), inv_vi = as.numeric(ia$vi),
                   plant_lnRR = ia$plant_lnRR,
                   inv_k = as.integer(table(d$Study_ID)[ia$Study_ID]),
                   responses = sapply(ia$Study_ID, function(s)
                     paste(intersect(RESP_GROUPS, d$Response_group[d$Study_ID == s]), collapse = "+")))
  pts[[set]] <- sp

  # (a) linear correlation, one point per study
  ct <- cor.test(sp$plant_lnRR, sp$inv_lnRR, method = "pearson")
  st <- suppressWarnings(cor.test(sp$plant_lnRR, sp$inv_lnRR, method = "spearman"))
  ols <- lm(inv_lnRR ~ plant_lnRR, data = sp)
  cor_rows[[set]] <- data.frame(set = set, n_studies = ns,
    pearson_r = round(unname(ct$estimate), 3), r_ci_lb = round(ct$conf.int[1], 3), r_ci_ub = round(ct$conf.int[2], 3),
    p = signif(ct$p.value, 3), r2_pct = round(100 * unname(ct$estimate)^2, 1),
    spearman_rho = round(unname(st$estimate), 3), spearman_p = signif(st$p.value, 3),
    ols_slope = round(coef(ols)[2], 3), ols_intercept = round(coef(ols)[1], 3))
  xs <- seq(min(sp$plant_lnRR), max(sp$plant_lnRR), length.out = 60)
  pr <- predict(ols, newdata = data.frame(plant_lnRR = xs), interval = "confidence")
  fit[[set]] <- data.frame(set = set, x = xs, pred = pr[, "fit"], lb = pr[, "lwr"], ub = pr[, "upr"])

  # (b) multilevel meta-regression on all effect sizes
  mods <- if (length(SETS[[set]]) > 1) ~ plant_lnRR + Response_group else ~ plant_lnRR
  m <- fit_mlma(d, mods); mr <- robustify(m, d)
  tf <- tidy_fit(m, mr, d, "Invert lnRR ~ plant biomass lnRR", set) |> filter(term == "plant_lnRR")
  tf$R2_het_pct <- het_explained(m, d)
  mr_rows[[set]] <- tf
}

cors <- bind_rows(cor_rows); mreg <- bind_rows(mr_rows)
write.csv(cors, "output/tables/plant_link_correlations.csv", row.names = FALSE)
write.csv(mreg, "output/tables/plant_link_metaregression.csv", row.names = FALSE)
write.csv(bind_rows(pts), "output/tables/plant_link_points.csv", row.names = FALSE)
write.csv(bind_rows(fit), "output/tables/plant_link_fit.csv", row.names = FALSE)
write.csv(plant, "output/tables/plant_lnRR_by_study.csv", row.names = FALSE)

print(plant_summary, row.names = FALSE)
print(cors, row.names = FALSE)
print(mreg[, c("response", "estimate", "ci_lb", "ci_ub", "p", "robust_p", "k", "n_studies", "n_locations", "R2_het_pct")],
      row.names = FALSE)
