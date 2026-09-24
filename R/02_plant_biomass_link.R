# Do invertebrate responses to grazer exclusion track plant-biomass responses?
# Run from the repo root:  Rscript R/02_plant_biomass_link.R
#
# 1. lnRR (exclosure / grazed) for plant biomass from the Ecosystem_functions sheet.
# 2. Plant lnRR is aggregated to one value per study (inverse-variance weighted,
#    assuming r = 0.5 among a study's plant effects).
# 3. Study-level plant lnRR is used as a moderator of the invertebrate lnRR in
#    the same multilevel model as R/01 (location / study / effect size),
#    for studies that report both.

source("R/00_functions.R")
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

raw <- read_excel(DATA_FILE, sheet = "Ecosystem_functions", .name_repair = "unique_quiet")
ef <- tibble(
  Study_ID = trimws(raw$Study_ID), Function_group = raw$Function_group,
  variation = ifelse(is.na(raw$variation), "SE", raw$variation),  # #1503 (respiration) unlabelled; not used here
  m_g = as.numeric(raw$Mean_Grazed),  se_g = as.numeric(raw$SD_or_SE_Grazed),  n_g = as.numeric(raw$n_Grazed),
  m_c = as.numeric(raw$Mean_Control), se_c = as.numeric(raw$SD_or_SE_Control), n_c = as.numeric(raw$n_Control)
) |>
  filter(!is.na(Study_ID), !is.na(Function_group), !is.na(n_g), !is.na(n_c)) |>
  mutate(sd_g = se_to_sd(se_g, n_g, variation), sd_c = se_to_sd(se_c, n_c, variation))
ef <- escalc("ROM", m1i = m_c, sd1i = sd_c, n1i = n_c, m2i = m_g, sd2i = sd_g, n2i = n_g, data = ef)
write.csv(ef, "results/tables/effect_sizes_ecosystem_functions.csv", row.names = FALSE)

inv <- load_inverts() |> compute_lnrr()

plant_by_study <- function(group) {
  e <- ef |> filter(Function_group == group)
  if (!nrow(e)) return(NULL)
  a <- aggregate(e, cluster = Study_ID, struct = "CS", rho = 0.5)
  data.frame(Study_ID = a$Study_ID, plant_lnRR = as.numeric(a$yi), plant_vi = as.numeric(a$vi),
             plant_k = as.numeric(table(e$Study_ID)[a$Study_ID]))
}

res <- list(); summ_rows <- list(); scat <- list()
for (grp in c("Plant biomass - above-ground", "Plant biomass - below-ground")) {
  pb <- plant_by_study(grp)
  write.csv(pb, sprintf("results/tables/plant_lnRR_by_study_%s.csv",
                        ifelse(grepl("above", grp), "above", "below")), row.names = FALSE)
  for (rg in RESP_GROUPS) {
    d <- inv |> filter(Response_group == rg) |> inner_join(pb, by = "Study_ID")
    ns <- n_distinct(d$Study_ID)
    summ_rows[[length(summ_rows) + 1]] <- data.frame(plant_measure = grp, response = rg,
                                                     k = nrow(d), n_studies = ns)
    if (ns < 5) { message(sprintf("skip %s ~ %s: only %d studies", rg, grp, ns)); next }
    m <- fit_mlma(d, ~ plant_lnRR); mr <- robustify(m, d)
    res[[length(res) + 1]] <- cbind(plant_measure = grp, tidy_fit(m, mr, d, "Invert lnRR ~ plant biomass lnRR", rg))
    # study-level means of the invertebrate response, for plotting
    agg <- aggregate(escalc(yi = d$yi, vi = d$vi, data = d), cluster = Study_ID, V = build_V(d))
    scat[[length(scat) + 1]] <- list(
      pts = data.frame(plant_measure = grp, response = rg, Study_ID = agg$Study_ID,
                       inv_lnRR = as.numeric(agg$yi), inv_vi = as.numeric(agg$vi),
                       plant_lnRR = agg$plant_lnRR),
      line = { x <- seq(min(d$plant_lnRR), max(d$plant_lnRR), length.out = 50)
               p <- predict(m, newmods = x)
               data.frame(plant_measure = grp, response = rg, x = x, pred = p$pred, lb = p$ci.lb, ub = p$ci.ub) })
  }
}

write.csv(bind_rows(summ_rows), "results/tables/plant_link_sample_sizes.csv", row.names = FALSE)
est <- bind_rows(res)
write.csv(est, "results/tables/plant_link_estimates.csv", row.names = FALSE)
print(est[, c("plant_measure", "response", "term", "estimate", "ci_lb", "ci_ub", "p", "robust_p", "k", "n_studies")])

if (length(scat)) {
  pts <- bind_rows(lapply(scat, `[[`, "pts")); ln <- bind_rows(lapply(scat, `[[`, "line"))
  g <- ggplot() +
    geom_hline(yintercept = 0, colour = INK2, linetype = "dashed", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = INK2, linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(data = ln, aes(x = x, ymin = lb, ymax = ub), fill = "#cde2fb") +
    geom_line(data = ln, aes(x = x, y = pred), colour = INK, linewidth = 0.8) +
    geom_point(data = pts, aes(x = plant_lnRR, y = inv_lnRR, size = 1 / sqrt(inv_vi)),
               shape = 21, fill = SERIES, colour = "white", alpha = 0.8, stroke = 0.6) +
    facet_grid(response ~ plant_measure, scales = "free") +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    labs(x = "Plant biomass lnRR (exclosure / grazed)", y = "Invertebrate lnRR (exclosure / grazed)",
         title = "Invertebrate vs plant-biomass responses to grazer exclusion",
         subtitle = "One point per study (size = precision); line and band: meta-regression fit and 95% CI") +
    theme_meta() + theme(panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3))
  ggsave("results/figures/plant_biomass_link.png", g, width = 8, height = 7.5, dpi = 250, bg = "white")
}
