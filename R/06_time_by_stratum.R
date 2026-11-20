# Does the change in grazer-exclusion effects over time differ among invertebrate strata?
# Run from the repo root:  Rscript R/06_time_by_stratum.R
#
# Extends H5 of R/01 (time since exclusion, ln[years + 1]) with a Stratum x time
# interaction, fitted separately for each response in the same multilevel model
# (location / study / effect size, shared-control covariance).
#   - Interaction test: model ~ Stratum * log_years; omnibus F-test of the
#     Stratum:log_years terms ("do the time slopes differ among strata?"),
#     model-based and cluster-robust (CR2, clustered by location).
#   - Stratum-specific intercepts and slopes from the equivalent
#     ~ Stratum + Stratum:log_years - 1 parameterisation.
#   - Pairwise differences in slope between strata.
#   - Share of heterogeneity explained by the interaction model, and by adding
#     the interaction to the additive Stratum + log_years model.
# Strata with fewer than 3 studies in a response are dropped (as elsewhere).

source("R/00_functions.R")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

es <- load_inverts() |> compute_lnrr() |>
  filter(!is.na(Years_since_exclusion), !is.na(Stratum)) |>
  mutate(log_years = log(Years_since_exclusion + 1))

est <- list(); tests <- list(); pairs <- list(); fits <- list(); sizes <- list()
for (rg in RESP_GROUPS) {
  message("== ", rg)
  d <- es |> filter(Response_group == rg) |> keep_levels("Stratum")
  d$es_id <- seq_len(nrow(d))
  sizes[[rg]] <- d |> group_by(Stratum) |>
    summarise(response = rg, k = n(), n_studies = n_distinct(Study_ID), n_locations = n_distinct(Location_ID),
              years_min = min(Years_since_exclusion), years_median = median(Years_since_exclusion),
              years_max = max(Years_since_exclusion), .groups = "drop")

  # interaction test
  mi <- fit_mlma(d, ~ Stratum * log_years); mir <- robustify(mi, d)
  int_terms <- grep(":", rownames(mi$b))
  ai <- anova(mi, btt = int_terms); air <- anova(mir, btt = int_terms)
  # additive model, for the gain in explained heterogeneity from the interaction
  ma <- fit_mlma(d, ~ Stratum + log_years)
  tests[[rg]] <- data.frame(response = rg, test = "Stratum x time interaction (slopes differ among strata)",
    F = round(ai$QM, 3), df1 = ai$QMdf[1], df2 = ai$QMdf[2], p = signif(ai$QMp, 3),
    robust_F = round(air$QM, 3), robust_p = signif(air$QMp, 3),
    R2_het_pct_full_model = het_explained(mi, d),
    R2_het_pct_additive = het_explained(ma, d),
    k = mi$k, n_studies = n_distinct(d$Study_ID), n_locations = n_distinct(d$Location_ID))

  # stratum-specific intercepts and slopes
  ms <- fit_mlma(d, ~ Stratum + Stratum:log_years - 1); msr <- robustify(ms, d)
  tf <- tidy_fit(ms, msr, d, "Time since exclusion by stratum", rg)
  tf$Stratum <- sub("^Stratum", "", sub(":log_years$", "", tf$term))
  tf$parameter <- ifelse(grepl(":log_years$", tf$term), "slope (per ln[years+1])", "intercept (year 0)")
  tf <- tf |> select(-k, -n_studies, -n_locations) |>
    left_join(sizes[[rg]] |> select(Stratum, k, n_studies, n_locations), by = "Stratum")
  est[[rg]] <- tf

  # pairwise slope differences
  sl <- grep(":log_years$", rownames(ms$b)); lv <- levels(d$Stratum)
  for (i in seq_along(lv)[-length(lv)]) for (j in (i + 1):length(lv)) {
    L <- rep(0, nrow(ms$b)); L[sl[j]] <- 1; L[sl[i]] <- -1
    a <- anova(ms, X = L); ar <- anova(msr, X = L)
    pairs[[length(pairs) + 1]] <- data.frame(response = rg, contrast = paste(lv[j], "-", lv[i]),
      estimate = round(a$Xb[1], 3), se = round(a$se[1], 3), p = signif(a$pval[1], 3), robust_p = signif(ar$pval[1], 3))
  }

  # fitted lines (each stratum over its own observed range) for the figure
  for (s in lv) {
    xs <- seq(min(d$log_years[d$Stratum == s]), max(d$log_years[d$Stratum == s]), length.out = 60)
    X <- matrix(0, length(xs), nrow(ms$b)); colnames(X) <- rownames(ms$b)
    X[, paste0("Stratum", s)] <- 1; X[, paste0("Stratum", s, ":log_years")] <- xs
    p <- predict(ms, newmods = X)
    fits[[length(fits) + 1]] <- data.frame(response = rg, Stratum = s, log_years = xs,
                                           pred = p$pred, ci_lb = p$ci.lb, ci_ub = p$ci.ub)
  }
}

write.csv(bind_rows(sizes), "output/tables/time_by_stratum_sample_sizes.csv", row.names = FALSE)
write.csv(bind_rows(tests), "output/tables/time_by_stratum_tests.csv", row.names = FALSE)
write.csv(bind_rows(est), "output/tables/time_by_stratum_estimates.csv", row.names = FALSE)
write.csv(bind_rows(pairs), "output/tables/time_by_stratum_slope_contrasts.csv", row.names = FALSE)
write.csv(bind_rows(fits), "output/tables/time_by_stratum_fit.csv", row.names = FALSE)

print(bind_rows(tests), row.names = FALSE)
print(bind_rows(est) |> filter(grepl("slope", parameter)) |>
        select(response, Stratum, estimate, ci_lb, ci_ub, p, robust_p, k, n_studies), row.names = FALSE)
print(bind_rows(pairs), row.names = FALSE)
