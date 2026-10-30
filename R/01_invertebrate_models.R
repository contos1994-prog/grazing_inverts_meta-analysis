# Grazer exclusion effects on invertebrates: multilevel meta-analysis.
# Run from the repo root:  Rscript R/01_invertebrate_models.R
#
# Each response type (abundance, richness, diversity) is analysed separately.
# Random effects: location / study / effect size. Every sampling year is kept
# as its own effect size; time since exclusion is tested as a moderator.
# Inference: t-tests with containment df; CR2 cluster-robust results (clustered
# by location) are reported alongside.

source("R/00_functions.R")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

es <- load_inverts() |> compute_lnrr()
write.csv(es, "output/tables/effect_sizes_invertebrates.csv", row.names = FALSE)

all_est <- list(); het <- list(); tests <- list(); time_fits <- list()

record_test <- function(m, mr, analysis, response, what, r2 = NA) {
  tests[[length(tests) + 1]] <<- data.frame(
    analysis = analysis, response = response, test = what,
    F = round(m$QM, 3), df1 = m$QMdf[1], df2 = m$QMdf[2], p = signif(m$QMp, 3),
    robust_F = round(mr$QM, 3), robust_p = signif(mr$QMp, 3), R2_het_pct = r2, R2_marginal_pct = r2_marginal(m))
}

# categorical moderator: cell-means model for level estimates + intercept
# model for the omnibus "do levels differ?" test
run_categorical <- function(d, var, analysis, response) {
  d <- keep_levels(d, var)
  if (nlevels(d[[var]]) < 2) { message("  skipped ", analysis, " (fewer than 2 levels)"); return(invisible()) }
  f_cells <- as.formula(paste("~", var, "- 1"))
  f_diff  <- as.formula(paste("~", var))
  m  <- fit_mlma(d, f_cells); mr <- robustify(m, d)
  all_est[[length(all_est) + 1]] <<- tidy_fit(m, mr, d, analysis, response, var)
  m2 <- fit_mlma(d, f_diff); mr2 <- robustify(m2, d)
  record_test(m2, mr2, analysis, response, paste("difference among", var, "levels"), het_explained(m2, d))
}

for (rg in RESP_GROUPS) {
  message("== ", rg)
  d <- es |> filter(Response_group == rg)

  # ---- H1: overall effect of excluding grazers
  m0 <- fit_mlma(d); mr0 <- robustify(m0, d)
  all_est[[length(all_est) + 1]] <- tidy_fit(m0, mr0, d, "Overall", rg)
  het[[rg]] <- data.frame(response = rg, k = m0$k, n_studies = n_distinct(d$Study_ID),
                          n_locations = n_distinct(d$Location_ID),
                          sigma2_location = m0$sigma2[1], sigma2_study = m0$sigma2[2],
                          sigma2_within = m0$sigma2[3], Q = round(m0$QE, 1), Q_p = signif(m0$QEp, 3),
                          t(i2_ml(m0)))
  # 95% prediction interval: range of true effects expected at a new site
  p0 <- predict(m0)
  het[[rg]]$pi_lb <- round(p0$pi.lb, 3); het[[rg]]$pi_ub <- round(p0$pi.ub, 3)
  het[[rg]]$pi_lb_pct <- pct(p0$pi.lb); het[[rg]]$pi_ub_pct <- pct(p0$pi.ub)
  # expected share of sites with a negative true effect (normal distribution of true effects)
  het[[rg]]$pct_sites_negative <- round(100 * pnorm(0, coef(m0)[1], sqrt(sum(m0$sigma2))), 1)

  # ---- H2: above- vs ground/litter- vs below-ground
  run_categorical(d |> filter(!is.na(Stratum)), "Stratum", "Stratum", rg)

  # ---- H3: grazer size (largest size class excluded)
  run_categorical(d |> filter(!is.na(Max_size_class)), "Max_size_class", "Grazer size (largest class excluded)", rg)
  # linear trend across ordered size classes (1 df)
  ds <- d |> filter(!is.na(Max_size_class)) |> mutate(size_rank = as.numeric(Max_size_class))
  mt <- fit_mlma(ds, ~ size_rank); mtr <- robustify(mt, ds)
  all_est[[length(all_est) + 1]] <- tidy_fit(mt, mtr, ds, "Grazer size trend (per class step)", rg)
  record_test(mt, mtr, "Grazer size trend (per class step)", rg, "linear trend", het_explained(mt, ds))

  # ---- H4: native vs domestic grazers
  dn <- d |> filter(Herbivore_origin %in% c("Native", "Domestic")) |>
    mutate(Herbivore_origin = factor(Herbivore_origin, levels = c("Native", "Domestic")))
  run_categorical(dn, "Herbivore_origin", "Herbivore origin", rg)

  # ---- H5: time since exclusion, ln(years + 1)
  dt <- d |> filter(!is.na(Years_since_exclusion)) |> mutate(log_years = log(Years_since_exclusion + 1))
  mtime <- fit_mlma(dt, ~ log_years); mtimer <- robustify(mtime, dt)
  all_est[[length(all_est) + 1]] <- tidy_fit(mtime, mtimer, dt, "Time since exclusion (ln years+1)", rg)
  record_test(mtime, mtimer, "Time since exclusion (ln years+1)", rg, "slope", het_explained(mtime, dt))
  # fitted line + 95% CI for plotting (R/05_figures.R)
  xs <- seq(0, max(dt$log_years), length.out = 100); pt <- predict(mtime, newmods = xs)
  time_fits[[rg]] <- data.frame(response = rg, log_years = xs, pred = pt$pred, ci_lb = pt$ci.lb, ci_ub = pt$ci.ub)

  # ---- sensitivity analyses
  run_categorical(d, "Design", "Sensitivity: study design", rg)
  dz <- d |> filter(!zero_adjusted)
  if (nrow(dz) < nrow(d)) {
    mz <- fit_mlma(dz); mzr <- robustify(mz, dz)
    all_est[[length(all_est) + 1]] <- tidy_fit(mz, mzr, dz, "Sensitivity: overall, zero-mean rows removed", rg)
  }
  # small-study / publication bias: multilevel Egger test (SE as moderator)
  d$sei <- sqrt(d$vi)
  me <- fit_mlma(d, ~ sei); mer <- robustify(me, d)
  all_est[[length(all_est) + 1]] <- tidy_fit(me, mer, d, "Publication bias: Egger (SE as moderator)", rg)
  # leave-one-study-out range for the overall estimate
  loo <- sapply(unique(d$Study_ID), function(s) coef(fit_mlma(d |> filter(Study_ID != s)))[1])
  het[[rg]]$loo_min <- round(min(loo), 3); het[[rg]]$loo_max <- round(max(loo), 3)
  het[[rg]]$loo_most_influential <- sub("\\.intrcpt$", "", names(loo)[which.max(abs(loo - coef(m0)[1]))])

}

est_tab <- bind_rows(all_est)
write.csv(est_tab, "output/tables/model_estimates.csv", row.names = FALSE)
write.csv(bind_rows(tests), "output/tables/moderator_tests.csv", row.names = FALSE)
write.csv(bind_rows(het), "output/tables/heterogeneity_overall.csv", row.names = FALSE)
write.csv(bind_rows(time_fits), "output/tables/time_since_exclusion_fit.csv", row.names = FALSE)

message("\nDone. Tables in output/tables; figures are drawn by R/05_figures.R.")
