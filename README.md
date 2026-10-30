# Grazer exclusion and invertebrates: meta-analysis

Effects of excluding grazers on invertebrate abundance, richness and diversity,
and whether these depend on stratum, grazer size, grazer origin (native vs
domestic), time since exclusion, invertebrate trophic level and biome.

## Files

| File | What it is |
|---|---|
| `MetaAnalysis_data.xlsx` | Original data (unchanged) |
| `MetaAnalysis_data_v2.xlsx` | Cleaned data used by the analysis. Yellow cells = edited, blue columns = derived; every edit is listed in the `Cleaning_log` sheet |
| `scripts/01_extract_biomes.R` | Assigns a RESOLVE Ecoregions 2017 biome to each study site -> `data/study_biomes.csv` |
| `data/study_biomes.csv` | Biome and ecoregion per study site, with coordinate source and match notes |
| `scripts/00_make_v2.py` | Builds v2 from the original (`python3 scripts/00_make_v2.py`, needs `openpyxl`) |
| `R/00_functions.R` | Shared helpers: effect sizes, covariance matrix, model fitting, plots |
| `R/01_invertebrate_models.R` | Overall effect + all moderator analyses + sensitivity checks |
| `R/03_trophic_level.R` | Effect by invertebrate trophic level |
| `R/04_biome.R` | Effect by biome |
| `R/05_figures.R` | All publication figures (PDF + 600 dpi PNG, 183 mm wide) from the model tables |
| `docs/Methods_Results.docx` | Draft Methods and Results with tables and figures (`docs/build_methods_results.js` builds it) |
| `output/tables/` | CSV outputs (effect sizes, estimates, moderator tests, heterogeneity) |
| `output/figures/` | Fig1 site map; Fig2 overall + grazer origin; Fig3 stratum + grazer size; Fig4 time since exclusion; Fig5 trophic level; FigS1 biome; FigS2 funnel plots |

Run from the repo root:

```sh
Rscript R/01_invertebrate_models.R   # ~10 min (leave-one-study-out refits)
Rscript R/03_trophic_level.R         # ~30 s
Rscript R/04_biome.R
Rscript R/05_figures.R               # needs patchwork, sf, maps, ragg
```

`data/study_biomes.csv` only needs regenerating if site coordinates change:
`Rscript scripts/01_extract_biomes.R` (downloads the ~150 MB RESOLVE shapefile; needs `sf`).

R packages: `metafor`, `clubSandwich`, `readxl`, `dplyr`, `tidyr`, `ggplot2`.

If you edit `MetaAnalysis_data_v2.xlsx` directly (e.g. back-filling missing n),
just re-run the R scripts. Only re-run `00_make_v2.py` if you change the
original file; it overwrites v2.

## Methods

**Effect size.** lnRR = ln(mean exclosure / mean grazed) (`escalc(measure = "ROM")`).
Positive values mean the metric is higher where grazers were excluded. SEs are
converted to SDs with SD = SE * sqrt(n). `% change = 100 * (exp(lnRR) - 1)`.

**Model.** Multilevel random-effects model, `rma.mv` with random effects
`~ 1 | Location_ID / Study_ID / es_id`, so studies at the same location
(within 100 km) and effect sizes within a study are not treated as independent.
When several comparisons in a study share the same exclosure (or grazed)
group, their sampling covariance is included in V. Tests use t-distributions
with containment df; cluster-robust (CR2, clustered by location) CIs and
p-values are reported alongside as a check.

**Responses** are analysed separately: Abundance (abundance + density),
Richness, Diversity (Shannon and other diversity indices).

**Moderators** (each fitted separately within each response):

| Hypothesis | Coding |
|---|---|
| Stratum | Above-ground / Ground & litter-dwelling (soil/litter, ground-dwelling and the 3 mixed rows) / Below-ground |
| Grazer size | Largest size class excluded (Small < Medium < Large < Mega); categorical + linear trend. "NR" rows excluded |
| Origin | Native vs Domestic, tested first; not significant for any response, so other moderators pool across origins. Mixed assemblages and invasive grazers excluded from this test |
| Time since exclusion | ln(years + 1), where years = last sampling year - exclusion year, or the stated duration. Ranges use the midpoint; open-ended values (">40 years") use the bound. Every sampling year is kept as its own effect size |

| Trophic level (`R/03`) | Cleaned `Trophic_Level` labels used as they are (no merging into broader guilds). Rows with a blank, "Total" or "Herbivore/detritivore" trophic level are excluded |
| Biome (`R/04`) | RESOLVE Ecoregions 2017 biome (Dinerstein et al. 2017; the layer served on ArcGIS Online as "Biomes") at each site, by point-in-polygon; sites just off a polygon edge take the nearest ecoregion within 25 km. 13 studies without coordinates were placed from their site description (`coord_source = manual`; #1609 and #1825 are ambiguous). RESOLVE biomes describe potential natural vegetation, so many grazed grasslands fall in forest biomes |

Levels with fewer than 3 studies are dropped from that moderator's model.
The omnibus F-test for "do levels differ?" is in `moderator_tests.csv`.
Trophic-level outputs are in `trophic_estimates.csv` and `trophic_tests.csv`. The CR2 robust omnibus test cannot be computed when
a model has many levels relative to the number of locations; it is reported as NA.

**Sensitivity / bias.** Design (exclosure vs natural grazed-vs-ungrazed);
removing rows with a zero mean; multilevel Egger test (SE as a moderator);
leave-one-study-out range of the overall estimate; funnel plots.

## Data cleaning (v2)

See the `Cleaning_log` sheet for the full list. In summary:

- Drag-fill (auto-increment) errors reset to the first row of the block:
  #1013 (exclusion year, sampling date), #2260 (sampling date), #1589 (exclusion
  year, sampling date), #1289 and #2492 (Author_Year).
- #1796 exclusion year was stored as an Excel date serial (shown as 1905) -> 1999.
- Zero means (#1624, #2025_3): a constant c = half the smallest non-zero mean in
  the study is added to both group means; the zero SE is set to c.
- Zero SE with a non-zero mean (#1508, #1125): imputed from the study's mean
  coefficient of variation (a tiny constant SE would give these rows enormous weight).
- #3610 control SE was a broken formula (`=AG491-AG490`, pointing at another
  study) -> blanked; the row drops until re-extracted.
- Whitespace in Study_ID / Author_Year stripped; a number stored as text converted.
- #2025_1 partial exclosures (livestock + rabbits, rows 521 and 523) were coded Native (wild);
  recoded to "Domestic and invasive" (Mixed), so they leave the native vs domestic test.
- **Order** standardised (81 cells): misspellings (Aranaea, Hymenpotera, Homeptera) and
  "Spider" corrected; Homoptera merged into Hemiptera; families entered as orders moved to
  their order (Formicidae -> Hymenoptera, Lumbricidae -> Crassiclitellata, Enchytraeidae ->
  Enchytraeida, Oribatida -> Sarcoptiformes), with the family written into a blank Family
  cell (18 cells); earthworms and *Helminthoglypta* moved from class to order; blank Orders
  filled where the common name identifies the taxon (spiders, ground beetles, plant hoppers,
  nematodes, mites, enchytraeids, molluscs). Higher ranks are kept where the order is unknown
  (Acari, Collembola, Gastropoda, Nematoda).
- **Trophic_Level** standardised (248 cells) to singular labels: Herbivore (incl. phytophagous,
  plant feeders), Predator (incl. carnivore, zoophagous, predatory), Detritivore (incl.
  decomposers), Bacterivore, Fungivore, Algivore (nematode feeding groups), Omnivore,
  Parasitoid, Pollinator, Scavenger, Herbivore/detritivore, Total. Blank trophic level set
  to Predator for spiders.
- Order is not yet used in the models. Trophic_Level is used only by `R/03_trophic_level.R`.
  Remaining taxonomic queries (e.g. #1097 "Total" rows, mismatched common names) are flagged
  at the bottom of `Cleaning_log`.

**Rows currently dropped:** missing n (#2260 x6, #2115 x1), missing SE
(#3610 x1), no response type (#1022 nematodes x5, one "Functional richness" row).
