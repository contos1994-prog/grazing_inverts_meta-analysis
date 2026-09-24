# Grazer exclusion and invertebrates: meta-analysis

Effects of excluding grazers on invertebrate abundance, richness and diversity,
and whether these depend on stratum, grazer size, grazer origin (native vs
domestic) and time since exclusion. Also tests whether invertebrate responses
track plant-biomass responses in studies that measured both.

## Files

| File | What it is |
|---|---|
| `MetaAnalysis_data.xlsx` | Original data (unchanged) |
| `MetaAnalysis_data_v2.xlsx` | Cleaned data used by the analysis. Yellow cells = edited, blue columns = derived; every edit is listed in the `Cleaning_log` sheet |
| `scripts/00_make_v2.py` | Builds v2 from the original (`python3 scripts/00_make_v2.py`, needs `openpyxl`) |
| `R/00_functions.R` | Shared helpers: effect sizes, covariance matrix, model fitting, plots |
| `R/01_invertebrate_models.R` | Overall effect + all moderator analyses + sensitivity checks |
| `R/02_plant_biomass_link.R` | Invertebrate lnRR vs plant-biomass lnRR |
| `output/tables/` | CSV outputs (effect sizes, estimates, moderator tests, heterogeneity) |
| `output/figures/` | Orchard plots, time-since-exclusion bubble plots, funnel plots, plant link |

Run from the repo root:

```sh
Rscript R/01_invertebrate_models.R   # ~10 min (leave-one-study-out refits)
Rscript R/02_plant_biomass_link.R
```

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
| Grazer size | Largest size class excluded (Small < Medium < Large < Mega); categorical + linear trend. Sensitivity: single-class exclosures only. "NR" rows excluded |
| Origin | Native vs Domestic. Mixed assemblages (22 rows) and invasive (3 rows) excluded for now |
| Time since exclusion | ln(years + 1), where years = last sampling year - exclusion year, or the stated duration. Ranges use the midpoint; open-ended values (">40 years") use the bound. Every sampling year is kept as its own effect size |

Levels with fewer than 3 studies are dropped from that moderator's model.
The omnibus F-test for "do levels differ?" is in `moderator_tests.csv`.

**Sensitivity / bias.** Design (exclosure vs natural grazed-vs-ungrazed);
removing rows with a zero mean; multilevel Egger test (SE as a moderator);
leave-one-study-out range of the overall estimate; funnel plots.

**Plant biomass link.** Plant-biomass lnRR from the `Ecosystem_functions`
sheet is averaged to one value per study (inverse-variance, r = 0.5 among a
study's plant effects) and used as a moderator of invertebrate lnRR in studies
reporting both. Only above-ground biomass has enough overlapping studies (>= 5).

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

**Rows currently dropped:** missing n (#2260 x6, #2115 x1), missing SE
(#3610 x1), no response type (#1022 nematodes x5, one "Functional richness" row).
