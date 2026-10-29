# Does the effect of grazer exclusion differ among invertebrate trophic levels,
# and does that depend on whether native or domestic grazers were excluded?
# Run from the repo root:  Rscript R/03_trophic_level.R
#
# Same effect size and multilevel model as R/01 (location / study / effect size,
# t-tests with containment df, CR2 robust results alongside). Each response
# type is analysed separately; trophic levels with < 3 studies are dropped
# from a model (keep_levels), so sparse responses may yield few or no tests.
#
# Trophic levels: the cleaned Trophic_Level labels from v2 are used as they are
# (no merging into broader guilds, e.g. Pollinator is not folded into Herbivore,
# nor Bacterivore/Fungivore/Algivore into a microbivore group). Rows are
# excluded where the trophic level is absent or unclear: blank, "Total"
# (whole-community counts) and "Herbivore/detritivore" (mixed guild).
#
# H7a  Trophic level, all exclusions (all herbivore origins, incl. mixed/invasive).
# H7b  Native vs domestic exclusion by trophic level:
#      - trophic-level estimates fitted separately within native and within
#        domestic exclusions;
#      - a joint Trophic x Origin model on trophic levels with >= 3 studies in
#        BOTH origins: an omnibus test of the interaction (does the native vs
#        domestic difference vary among trophic levels?) and, per trophic
#        level, the Domestic - Native contrast.

source("R/00_functions.R")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

UNCLEAR_TROPHIC <- c("Total", "Herbivore/detritivore")

es <- load_inverts() |> compute_lnrr()
n_all <- nrow(es)
es <- es |> filter(!is.na(Trophic_Level), !Trophic_Level %in% UNCLEAR_TROPHIC)
message(sprintf("Trophic analyses: dropped %d rows with blank or unclear trophic level (%s)",
                n_all - nrow(es), paste(UNCLEAR_TROPHIC, collapse = ", ")))

all_est <- list(); tests <- list(); contrasts <- list()

try_fit <- function(d, mods, what) {
  tryCatch(fit_mlma(d, mods), error = function(e) { message("  ", what, " failed: ", conditionMessage(e)); NULL })
}

record_test <- function(m, mr, analysis, response, what, btt = NULL) {
  a  <- if (is.null(btt)) m  else anova(m, btt = btt)
  ar <- if (is.null(btt)) mr else anova(mr, btt = btt)
  tests[[length(tests) + 1]] <<- data.frame(
    analysis = analysis, response = response, test = what,
    F = round(a$QM, 3), df1 = a$QMdf[1], df2 = a$QMdf[2], p = signif(a$QMp, 3),
    # the CR2 (HTZ) Wald test cannot be computed when the number of levels is
    # large relative to the number of locations; report NA rather than 0
    robust_F = if (is.na(ar$QMp)) NA else round(ar$QM, 3), robust_p = signif(ar$QMp, 3))
}

# categorical moderator: cell-means model for level estimates + intercept
# model for the omnibus "do levels differ?" test (as in R/01)
run_categorical <- function(d, var, analysis, response) {
  d <- keep_levels(d, var)
  if (nlevels(d[[var]]) < 2) { message("  skipped ", analysis, " (fewer than 2 levels)"); return(invisible()) }
  m <- try_fit(d, as.formula(paste("~", var, "- 1")), analysis)
  if (is.null(m)) return(invisible())
  mr <- robustify(m, d)
  all_est[[length(all_est) + 1]] <<- tidy_fit(m, mr, d, analysis, response, var)
  m2 <- try_fit(d, as.formula(paste("~", var)), analysis)
  if (!is.null(m2)) record_test(m2, robustify(m2, d), analysis, response, paste("difference among", var, "levels"))
}

for (rg in RESP_GROUPS) {
  message("== ", rg)
  d <- es |> filter(Response_group == rg)

  # ---- H7a: trophic level, all exclusions
  run_categorical(d, "Trophic_Level", "Trophic level", rg)

  # ---- H7b: native vs domestic exclusion by trophic level
  dn <- d |> filter(Herbivore_origin %in% c("Native", "Domestic"))
  for (o in c("Native", "Domestic"))
    run_categorical(dn |> filter(Herbivore_origin == o), "Trophic_Level",
                    sprintf("Trophic level (%s exclusion)", tolower(o)), rg)

  # joint model: trophic levels with >= 3 studies under both origins
  ok <- dn |> group_by(Trophic_Level, Herbivore_origin) |>
    summarise(ns = n_distinct(Study_ID), .groups = "drop") |>
    filter(ns >= MIN_STUDIES_PER_LEVEL) |> count(Trophic_Level) |>
    filter(n == 2) |> pull(Trophic_Level)
  if (!length(ok)) { message("  skipped Trophic x Origin (no trophic level with >= 3 studies in both origins)"); next }
  message("  Trophic x Origin: levels shared by both origins: ", paste(ok, collapse = ", "))
  dj <- dn |> filter(Trophic_Level %in% ok) |>
    mutate(Trophic_Level = factor(Trophic_Level, levels = sort(ok)),
           Herbivore_origin = factor(Herbivore_origin, levels = c("Native", "Domestic")),
           Cell = factor(paste(Trophic_Level, Herbivore_origin, sep = " | "),
                         levels = paste(rep(sort(ok), each = 2), c("Native", "Domestic"), sep = " | ")))
  an <- "Trophic level x origin"

  # cell means (one estimate per trophic level x origin) and per-level contrasts
  mc <- try_fit(dj, ~ Cell - 1, an)
  if (is.null(mc)) next
  mcr <- robustify(mc, dj)
  all_est[[length(all_est) + 1]] <- tidy_fit(mc, mcr, dj, an, rg, "Cell")
  X <- t(sapply(seq_along(ok), function(i) { x <- numeric(2 * length(ok)); x[2 * i] <- 1; x[2 * i - 1] <- -1; x }))
  if (length(ok) == 1) X <- matrix(X, nrow = 1)
  ct <- anova(mc, X = X); ctr <- anova(mcr, X = X)
  contrasts[[length(contrasts) + 1]] <- data.frame(
    response = rg, trophic_level = sort(ok), contrast = "Domestic - Native",
    estimate = round(as.numeric(ct$Xb), 3), ci_lb = round(as.numeric(ct$Xb) - qt(0.975, ct$ddf) * ct$se, 3),
    ci_ub = round(as.numeric(ct$Xb) + qt(0.975, ct$ddf) * ct$se, 3), p = signif(ct$pval, 3),
    robust_p = signif(ctr$pval, 3))

  # omnibus interaction test (only meaningful with >= 2 shared trophic levels)
  if (length(ok) >= 2) {
    mi <- try_fit(dj, ~ Trophic_Level * Herbivore_origin, an)
    if (!is.null(mi)) {
      record_test(mi, robustify(mi, dj), an, rg,
                  "Trophic_Level x Herbivore_origin interaction (does the origin effect differ among trophic levels?)",
                  btt = grep(":", rownames(mi$b)))
      record_test(mi, robustify(mi, dj), an, rg,
                  "Herbivore_origin main effect (Domestic - Native, reference trophic level)",
                  btt = grep("^Herbivore_origin", rownames(mi$b)))
    }
  }
}

write.csv(bind_rows(all_est), "output/tables/trophic_estimates.csv", row.names = FALSE)
write.csv(bind_rows(tests), "output/tables/trophic_tests.csv", row.names = FALSE)
write.csv(bind_rows(contrasts), "output/tables/trophic_origin_contrasts.csv", row.names = FALSE)

message("\nDone. Trophic-level tables in output/tables (trophic_*.csv); figures are drawn by R/05_figures.R.")
