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

all_est <- list(); het <- list(); tests <- list(); plot_rows <- list()

add_plot_rows <- function(m, d, analysis, response, level_var = NULL) {
  if (is.null(level_var)) {
    p <- predict(m)
    s <- data.frame(level = "Overall", estimate = p$pred[1], ci_lb = p$ci.lb[1], ci_ub = p$ci.ub[1],
                    pi_lb = p$pi.lb[1], pi_ub = p$pi.ub[1])
    cnt <- data.frame(level = "Overall", k = nrow(d), ns = n_distinct(d$Study_ID))
  } else {
    lv <- levels(d[[level_var]])
    X <- diag(length(lv))
    p <- predict(m, newmods = X)
    s <- data.frame(level = lv, estimate = p$pred, ci_lb = p$ci.lb, ci_ub = p$ci.ub,
                    pi_lb = p$pi.lb, pi_ub = p$pi.ub)
    cnt <- d |> group_by(level = as.character(.data[[level_var]])) |>
      summarise(k = n(), ns = n_distinct(Study_ID), .groups = "drop")
  }
  s <- left_join(s, cnt, by = "level")
  s$lab <- sprintf("k = %d (%d studies)", s$k, s$ns)
  s$response <- response; s$analysis <- analysis
  eff <- data.frame(level = if (is.null(level_var)) "Overall" else as.character(d[[level_var]]),
                    yi = d$yi, vi = d$vi, response = response, analysis = analysis)
  plot_rows[[length(plot_rows) + 1]] <<- list(summ = s, eff = eff)
}

record_test <- function(m, mr, analysis, response, what) {
  tests[[length(tests) + 1]] <<- data.frame(
    analysis = analysis, response = response, test = what,
    F = round(m$QM, 3), df1 = m$QMdf[1], df2 = m$QMdf[2], p = signif(m$QMp, 3),
    robust_F = round(mr$QM, 3), robust_p = signif(mr$QMp, 3))
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
  record_test(m2, mr2, analysis, response, paste("difference among", var, "levels"))
  add_plot_rows(m, d, analysis, response, var)
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
  add_plot_rows(m0, d, "Overall", rg)

  # ---- H2: above- vs ground/litter- vs below-ground
  run_categorical(d |> filter(!is.na(Stratum)), "Stratum", "Stratum", rg)

  # ---- H3: grazer size (largest size class excluded)
  run_categorical(d |> filter(!is.na(Max_size_class)), "Max_size_class", "Grazer size (largest class excluded)", rg)
  # linear trend across ordered size classes (1 df)
  ds <- d |> filter(!is.na(Max_size_class)) |> mutate(size_rank = as.numeric(Max_size_class))
  mt <- fit_mlma(ds, ~ size_rank); mtr <- robustify(mt, ds)
  all_est[[length(all_est) + 1]] <- tidy_fit(mt, mtr, ds, "Grazer size trend (per class step)", rg)
  # sensitivity: studies excluding a single size class only
  run_categorical(d |> filter(!is.na(Single_size_class)), "Single_size_class", "Grazer size (single-class exclosures only)", rg)

  # ---- H4: native vs domestic grazers
  dn <- d |> filter(Herbivore_origin %in% c("Native", "Domestic")) |>
    mutate(Herbivore_origin = factor(Herbivore_origin, levels = c("Native", "Domestic")))
  run_categorical(dn, "Herbivore_origin", "Herbivore origin", rg)

  # ---- H5: time since exclusion, ln(years + 1)
  dt <- d |> filter(!is.na(Years_since_exclusion)) |> mutate(log_years = log(Years_since_exclusion + 1))
  mtime <- fit_mlma(dt, ~ log_years); mtimer <- robustify(mtime, dt)
  all_est[[length(all_est) + 1]] <- tidy_fit(mtime, mtimer, dt, "Time since exclusion (ln years+1)", rg)
  png(sprintf("output/figures/time_since_exclusion_%s.png", tolower(rg)), width = 1800, height = 1300, res = 250)
  regplot(mtime, mod = "log_years", xlab = "Years since exclusion (log scale, ln[years + 1])",
          ylab = "lnRR (exclosure / grazed)", pi = TRUE, shade = "#cde2fb", bg = "#2a78d655",
          col = "#2a78d6", main = paste(rg, "- time since grazer exclusion"), xvals = seq(0, 4.1, length.out = 100))
  abline(h = 0, lty = 2, col = "#52514e")
  dev.off()

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
  het[[rg]]$loo_most_influential <- names(loo)[which.max(abs(loo - coef(m0)[1]))]

  png(sprintf("output/figures/funnel_%s.png", tolower(rg)), width = 1600, height = 1400, res = 250)
  funnel(m0, xlab = "Residual lnRR", back = "#f0efec", shade = "white", hlines = "#e4e3df",
         pch = 21, bg = "#2a78d655", col = "#2a78d6", main = paste(rg, "- funnel plot"))
  dev.off()
}

est_tab <- bind_rows(all_est)
write.csv(est_tab, "output/tables/model_estimates.csv", row.names = FALSE)
write.csv(bind_rows(tests), "output/tables/moderator_tests.csv", row.names = FALSE)
write.csv(bind_rows(het), "output/tables/heterogeneity_overall.csv", row.names = FALSE)

# ---------------------------------------------------------------- figures
plot_analysis <- function(analysis, file, level_lab, title) {
  pr <- Filter(function(x) x$summ$analysis[1] == analysis, plot_rows)
  if (!length(pr)) return(invisible())
  summ <- bind_rows(lapply(pr, `[[`, "summ")); eff <- bind_rows(lapply(pr, `[[`, "eff"))
  lv <- rev(unique(summ$level))
  summ$level <- factor(summ$level, lv); eff$level <- factor(eff$level, lv)
  summ$response <- factor(summ$response, RESP_GROUPS); eff$response <- factor(eff$response, RESP_GROUPS)
  h <- 1.2 + 0.55 * nrow(summ) + 0.6 * length(unique(summ$response))
  g <- orchard(eff, summ, level_lab, title,
               "Points: individual effect sizes (size = precision). Bar: 95% CI; line: 95% prediction interval.")
  ggsave(file, g, width = 7.5, height = h, dpi = 250, bg = "white")
}
plot_analysis("Overall", "output/figures/overall.png", NULL, "Overall effect of excluding grazers")
plot_analysis("Stratum", "output/figures/stratum.png", NULL, "Effect of grazer exclusion by invertebrate stratum")
plot_analysis("Grazer size (largest class excluded)", "output/figures/grazer_size.png",
              "Largest grazer size class excluded", "Effect of grazer exclusion by grazer size")
plot_analysis("Grazer size (single-class exclosures only)", "output/figures/grazer_size_single_class.png",
              "Grazer size class", "Grazer size - single-class exclosures only")
plot_analysis("Herbivore origin", "output/figures/herbivore_origin.png", NULL,
              "Effect of excluding native vs domestic grazers")

message("\nDone. Tables in output/tables, figures in output/figures.")
