# Does the effect of grazer exclusion differ among invertebrate trophic levels?
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

source("R/00_functions.R")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

UNCLEAR_TROPHIC <- c("Total", "Herbivore/detritivore")

es <- load_inverts() |> compute_lnrr()
n_all <- nrow(es)
es <- es |> filter(!is.na(Trophic_Level), !Trophic_Level %in% UNCLEAR_TROPHIC)
message(sprintf("Trophic analyses: dropped %d rows with blank or unclear trophic level (%s)",
                n_all - nrow(es), paste(UNCLEAR_TROPHIC, collapse = ", ")))

all_est <- list(); tests <- list()

try_fit <- function(d, mods, what) {
  tryCatch(fit_mlma(d, mods), error = function(e) { message("  ", what, " failed: ", conditionMessage(e)); NULL })
}

record_test <- function(m, mr, analysis, response, what) {
  a <- m; ar <- mr
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

  run_categorical(d, "Trophic_Level", "Trophic level", rg)
}

write.csv(bind_rows(all_est), "output/tables/trophic_estimates.csv", row.names = FALSE)
write.csv(bind_rows(tests), "output/tables/trophic_tests.csv", row.names = FALSE)

message("\nDone. Trophic-level tables in output/tables (trophic_*.csv); figures are drawn by R/05_figures.R.")
