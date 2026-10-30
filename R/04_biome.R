# Does the effect of grazer exclusion differ among biomes?
# Run from the repo root:  Rscript R/04_biome.R
#
# Biome = RESOLVE Ecoregions 2017 biome (Dinerstein et al. 2017) at each study
# site, extracted by scripts/01_extract_biomes.R into data/study_biomes.csv.
# Rows are matched on Study_ID + coordinates; studies without coordinates in the
# data were located approximately from their site description (flagged in
# data/study_biomes.csv, column coord_source = "manual").
#
# Same effect size and multilevel model as R/01 (location / study / effect size,
# t-tests with containment df, CR2 robust results alongside). Biomes with < 3
# studies in a response are dropped from that model (keep_levels).

source("R/00_functions.R")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

es <- load_inverts() |> compute_lnrr() |> add_biome()
message(sprintf("Biome assigned to %d of %d effect sizes", sum(!is.na(es$Biome)), nrow(es)))

# effect sizes and studies per biome x response (all biomes, before dropping)
counts <- es |> filter(!is.na(Biome)) |> group_by(Biome, Response_group) |>
  summarise(k = n(), n_studies = n_distinct(Study_ID), .groups = "drop") |>
  arrange(Response_group, desc(n_studies))
write.csv(counts, "output/tables/biome_sample_sizes.csv", row.names = FALSE)

all_est <- list(); tests <- list()

for (rg in RESP_GROUPS) {
  message("== ", rg)
  d <- es |> filter(Response_group == rg, !is.na(Biome)) |> keep_levels("Biome")
  if (nlevels(d$Biome) < 2) { message("  skipped (fewer than 2 biomes)"); next }
  m <- fit_mlma(d, ~ Biome - 1); mr <- robustify(m, d)
  all_est[[length(all_est) + 1]] <- tidy_fit(m, mr, d, "Biome", rg, "Biome")
  m2 <- fit_mlma(d, ~ Biome); mr2 <- robustify(m2, d)
  tests[[length(tests) + 1]] <- data.frame(
    analysis = "Biome", response = rg, test = "difference among Biome levels",
    F = round(m2$QM, 3), df1 = m2$QMdf[1], df2 = m2$QMdf[2], p = signif(m2$QMp, 3),
    robust_F = if (is.na(mr2$QMp)) NA else round(mr2$QM, 3), robust_p = signif(mr2$QMp, 3),
    R2_het_pct = het_explained(m2, d), R2_marginal_pct = r2_marginal(m2))

}

write.csv(bind_rows(all_est), "output/tables/biome_estimates.csv", row.names = FALSE)
write.csv(bind_rows(tests), "output/tables/biome_tests.csv", row.names = FALSE)

message("\nDone. Biome tables in output/tables (biome_*.csv); figures are drawn by R/05_figures.R.")
