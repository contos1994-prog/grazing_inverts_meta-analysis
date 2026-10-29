# Does the effect of grazer exclusion differ among biomes?
# Run from the repo root:  Rscript R/04_biome.R
#
# Biome = RESOLVE Ecoregions 2017 biome (Dinerstein et al. 2017) at each study
# site, extracted by scripts/01_extract_biomes.py into data/study_biomes.csv.
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

# short labels for plotting
BIOME_LABELS <- c(
  "Boreal Forests/Taiga" = "Boreal forest",
  "Deserts & Xeric Shrublands" = "Desert & xeric shrubland",
  "Flooded Grasslands & Savannas" = "Flooded grassland",
  "Mediterranean Forests, Woodlands & Scrub" = "Mediterranean",
  "Montane Grasslands & Shrublands" = "Montane grassland",
  "Temperate Broadleaf & Mixed Forests" = "Temperate broadleaf forest",
  "Temperate Conifer Forests" = "Temperate conifer forest",
  "Temperate Grasslands, Savannas & Shrublands" = "Temperate grassland",
  "Tropical & Subtropical Coniferous Forests" = "Tropical conifer forest",
  "Tropical & Subtropical Dry Broadleaf Forests" = "Tropical dry forest",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "Tropical savanna",
  "Tropical & Subtropical Moist Broadleaf Forests" = "Tropical moist forest",
  "Tundra" = "Tundra")

bio <- read.csv("data/study_biomes.csv") |>
  transmute(Study_ID, lat_key = round(Latitude_decimal, 5), lon_key = round(Longitude_decimal, 5),
            Biome = unname(BIOME_LABELS[BIOME_NAME]))

es <- load_inverts() |> compute_lnrr() |>
  mutate(lat_key = round(Latitude, 5), lon_key = round(Longitude, 5)) |>
  left_join(bio, by = c("Study_ID", "lat_key", "lon_key"))
message(sprintf("Biome assigned to %d of %d effect sizes", sum(!is.na(es$Biome)), nrow(es)))

# effect sizes and studies per biome x response (all biomes, before dropping)
counts <- es |> filter(!is.na(Biome)) |> group_by(Biome, Response_group) |>
  summarise(k = n(), n_studies = n_distinct(Study_ID), .groups = "drop") |>
  arrange(Response_group, desc(n_studies))
write.csv(counts, "output/tables/biome_sample_sizes.csv", row.names = FALSE)

all_est <- list(); tests <- list(); plot_rows <- list()

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
    robust_F = if (is.na(mr2$QMp)) NA else round(mr2$QM, 3), robust_p = signif(mr2$QMp, 3))

  lv <- levels(d$Biome)
  p <- predict(m, newmods = diag(length(lv)))
  cnt <- d |> group_by(level = as.character(Biome)) |> summarise(k = n(), ns = n_distinct(Study_ID), .groups = "drop")
  s <- data.frame(level = lv, estimate = p$pred, ci_lb = p$ci.lb, ci_ub = p$ci.ub, pi_lb = p$pi.lb, pi_ub = p$pi.ub) |>
    left_join(cnt, by = "level")
  s$lab <- sprintf("k = %d (%d studies)", s$k, s$ns); s$response <- rg
  plot_rows[[length(plot_rows) + 1]] <- list(
    summ = s, eff = data.frame(level = as.character(d$Biome), yi = d$yi, vi = d$vi, response = rg))
}

write.csv(bind_rows(all_est), "output/tables/biome_estimates.csv", row.names = FALSE)
write.csv(bind_rows(tests), "output/tables/biome_tests.csv", row.names = FALSE)

if (length(plot_rows)) {
  summ <- bind_rows(lapply(plot_rows, `[[`, "summ")); eff <- bind_rows(lapply(plot_rows, `[[`, "eff"))
  lv <- rev(sort(unique(summ$level)))
  summ$level <- factor(summ$level, lv); eff$level <- factor(eff$level, lv)
  rs <- intersect(RESP_GROUPS, summ$response)
  summ$response <- factor(summ$response, rs); eff$response <- factor(eff$response, rs)
  g <- orchard(eff, summ, NULL, "Effect of grazer exclusion by biome (RESOLVE 2017)",
               "Points: effect sizes (size = precision)\nThick bar: 95% CI; thin line: 95% prediction interval")
  ggsave("output/figures/biome.png", g, width = 7.5, height = 1.2 + 0.55 * nrow(summ) + 0.6 * length(rs),
         dpi = 250, bg = "white")
}

message("\nDone. Biome tables in output/tables (biome_*.csv), figure output/figures/biome.png.")
