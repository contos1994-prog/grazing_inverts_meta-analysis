# Publication figures. Run after R/01-R/04:  Rscript R/05_figures.R
#
# Reads the model tables in output/tables and the effect sizes, and writes each
# figure as vector PDF and 600 dpi PNG to output/figures. Sized for a
# double-column width of 183 mm. Orchard-style panels show individual effect
# sizes (point size = precision), the model estimate and its 95% CI.

source("R/00_functions.R")
suppressPackageStartupMessages({ library(patchwork); library(sf); library(maps) })
sf_use_s2(FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
set.seed(1)

FONT <- "Liberation Sans"            # metric-compatible with Arial / Helvetica
RESP_COL <- c(Abundance = "#0072B2", Richness = "#D55E00", Diversity = "#009E73")   # Okabe-Ito
ORIGIN_COL <- c(Native = "#E69F00", Domestic = "#56B4E9", Mixed = "#999999")
TAB <- "output/tables/"

theme_pub <- function() {
  theme_classic(base_size = 7, base_family = FONT) +
    theme(axis.line = element_line(linewidth = 0.3), axis.ticks = element_line(linewidth = 0.3),
          axis.ticks.length = unit(1.2, "mm"),
          axis.text = element_text(colour = "black", size = 6), axis.title = element_text(size = 7),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 7),
          panel.spacing.x = unit(5, "mm"), panel.spacing.y = unit(3, "mm"),
          legend.title = element_text(size = 6.5), legend.text = element_text(size = 6),
          legend.key.size = unit(3, "mm"), legend.background = element_blank(),
          plot.tag = element_text(face = "bold", size = 8, family = FONT),
          plot.margin = margin(2, 4, 2, 2, "mm"))
}

save_fig <- function(p, name, w = 183, h = 120) {
  ggsave(file.path("output/figures", paste0(name, ".pdf")), p, width = w, height = h, units = "mm",
         device = cairo_pdf, bg = "white")
  ggsave(file.path("output/figures", paste0(name, ".png")), p, width = w, height = h, units = "mm",
         dpi = 600, device = ragg::agg_png, bg = "white")
  message("wrote output/figures/", name, ".pdf/.png")
}

# % change on the top axis; breaks chosen so labels never crowd (>= 15% of the lnRR range apart)
pct_breaks <- function(lims) {
  r <- log(1 + lims / 100); span <- diff(r)
  cand <- c(0, -25, 25, -50, 50, 100, -75, 200, -90, 500, 1000, -95, 2000, 5000, 10000)
  keep <- c()
  for (b in cand) {
    x <- log(1 + b / 100)
    if (x >= r[1] && x <= r[2] && all(abs(x - log(1 + keep / 100)) >= 0.15 * span)) keep <- c(keep, b)
  }
  sort(keep)
}
pct_axis <- sec_axis(~ 100 * (exp(.) - 1), name = "Change in exclosures (%)", breaks = pct_breaks)
X_LNRR <- "Effect size (lnRR, exclosure / grazed)"

# ------------------------------------------------------------------ data
es <- load_inverts() |> compute_lnrr() |> add_biome()

est_of <- function(file, analysis_name, prefix) {
  read.csv(paste0(TAB, file)) |> filter(analysis == analysis_name) |>
    mutate(level = sub(paste0("^", prefix), "", term))
}
# raw effect sizes for the levels that were modelled
pts_of <- function(est, var, data = es) {
  data |> mutate(level = as.character(.data[[var]]), response = Response_group) |>
    semi_join(est, by = c("response", "level")) |> select(response, level, yi, vi)
}

# ------------------------------------------------------------------ orchard panel
orchard_panel <- function(est, pts, levels, col_by = "response", cols = RESP_COL, show_n = TRUE) {
  rs <- intersect(RESP_GROUPS, unique(est$response))
  prep <- function(df) {
    df$response <- factor(df$response, rs)
    df$y <- match(df$level, rev(levels)); df
  }
  est <- prep(est); pts <- prep(pts)
  pts$y <- pts$y + runif(nrow(pts), -0.2, 0.2)
  est$lab <- sprintf("%d (%d)", est$k, est$n_studies)
  g <- ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey45") +
    geom_point(data = pts, aes(x = yi, y = y, size = 1 / sqrt(vi), colour = .data[[col_by]]),
               alpha = 0.3, stroke = 0) +
    geom_errorbarh(data = est, aes(y = y, xmin = ci_lb, xmax = ci_ub), height = 0, linewidth = 0.8,
                   colour = "black") +
    geom_point(data = est, aes(x = estimate, y = y, fill = .data[[col_by]]), shape = 21, size = 2.2,
               stroke = 0.4, colour = "black") +
    facet_grid(. ~ response, scales = "free_x") +
    scale_y_continuous(breaks = seq_along(levels), labels = rev(levels), expand = expansion(add = 0.6)) +
    scale_x_continuous(sec.axis = pct_axis) +
    scale_size_continuous(range = c(0.4, 2.8), guide = "none") +
    scale_colour_manual(values = cols, guide = "none") + scale_fill_manual(values = cols, guide = "none") +
    labs(x = X_LNRR, y = NULL) + theme_pub()
  if (show_n) g <- g + geom_text(data = est, aes(x = Inf, y = y + 0.36, label = lab), hjust = 1, size = 1.9,
                                 colour = "grey30", family = FONT)
  g
}

# ------------------------------------------------------------------ Figure 1: map
sites <- read.csv("data/study_biomes.csv")
site_k <- es |> mutate(lat_key = round(Latitude, 5), lon_key = round(Longitude, 5)) |>
  count(Study_ID, lat_key, lon_key, name = "k")
origin_site <- es |> mutate(lat_key = round(Latitude, 5), lon_key = round(Longitude, 5)) |>
  group_by(Study_ID, lat_key, lon_key) |>
  summarise(origin = { o <- unique(na.omit(Herbivore_origin)); o <- o[o %in% c("Native", "Domestic")]
                       if (length(o) == 1) o else "Mixed" }, .groups = "drop")
map_pts <- sites |> mutate(lat_key = round(Latitude_decimal, 5), lon_key = round(Longitude_decimal, 5)) |>
  inner_join(site_k, by = c("Study_ID", "lat_key", "lon_key")) |>
  left_join(origin_site, by = c("Study_ID", "lat_key", "lon_key")) |>
  group_by(query_lat, query_lon) |>
  summarise(k = sum(k), n_studies = n_distinct(Study_ID),
            origin = if (n_distinct(origin) == 1) first(origin) else "Mixed", .groups = "drop") |>
  mutate(origin = factor(origin, c("Native", "Domestic", "Mixed"),
                         c("Native", "Domestic", "Native and domestic / mixed")))
PROJ <- "+proj=robin"
world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE, wrap = c(-180, 180))) |>
  filter(ID != "Antarctica") |> st_make_valid() |>
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) |> st_transform(PROJ)
pts_sf <- st_as_sf(map_pts, coords = c("query_lon", "query_lat"), crs = 4326) |> st_transform(PROJ) |>
  arrange(desc(k))
fig1 <- ggplot() +
  geom_sf(data = world, fill = "grey88", colour = NA) +
  geom_sf(data = pts_sf, aes(size = k, fill = origin), shape = 21, colour = "black", stroke = 0.3, alpha = 0.85) +
  scale_fill_manual(values = setNames(unname(ORIGIN_COL), levels(map_pts$origin)), name = "Grazers excluded") +
  scale_size_area(max_size = 6, breaks = c(5, 20, 50, 100), name = "Effect sizes") +
  coord_sf(crs = PROJ, expand = FALSE, datum = NA) +
  guides(fill = guide_legend(override.aes = list(size = 2.5), order = 1)) +
  theme_void(base_size = 7, base_family = FONT) +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(size = 6.5),
        legend.text = element_text(size = 6), plot.margin = margin(2, 2, 2, 2, "mm"))
save_fig(fig1, "Fig1_study_sites", h = 105)

# ------------------------------------------------------------------ Figure 2: overall + stratum
ov <- est_of("model_estimates.csv", "Overall", "") |> mutate(level = response)
ov_pts <- es |> transmute(response = Response_group, level = Response_group, yi, vi) |> filter(!is.na(response))
p_ov <- {
  e <- ov; p <- ov_pts
  e$y <- match(e$level, rev(RESP_GROUPS)); p$y <- match(p$level, rev(RESP_GROUPS)) + runif(nrow(p), -0.2, 0.2)
  e$lab <- sprintf("%d (%d)", e$k, e$n_studies)
  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey45") +
    geom_point(data = p, aes(yi, y, size = 1 / sqrt(vi), colour = level), alpha = 0.3, stroke = 0) +
    geom_errorbarh(data = e, aes(y = y, xmin = ci_lb, xmax = ci_ub), height = 0, linewidth = 0.8) +
    geom_point(data = e, aes(estimate, y, fill = level), shape = 21, size = 2.4, stroke = 0.4) +
    geom_text(data = e, aes(x = Inf, y = y + 0.36, label = lab), hjust = 1, size = 1.9, colour = "grey30", family = FONT) +
    scale_y_continuous(breaks = 1:3, labels = rev(RESP_GROUPS), expand = expansion(add = 0.6)) +
    scale_x_continuous(sec.axis = pct_axis) +
    scale_size_continuous(range = c(0.4, 2.8), guide = "none") +
    scale_colour_manual(values = RESP_COL, guide = "none") + scale_fill_manual(values = RESP_COL, guide = "none") +
    labs(x = X_LNRR, y = NULL) + theme_pub()
}
st <- est_of("model_estimates.csv", "Stratum", "Stratum")
p_st <- orchard_panel(st, pts_of(st, "Stratum"), c("Above-ground", "Ground & litter-dwelling", "Below-ground"))
fig2 <- (p_ov / p_st) + plot_layout(heights = c(1, 1.1)) + plot_annotation(tag_levels = "a")
save_fig(fig2, "Fig2_overall_stratum", h = 120)

# ------------------------------------------------------------------ Figure 3: grazer size
SIZE_LV <- c("Small", "Medium", "Large", "Mega")
sz <- est_of("model_estimates.csv", "Grazer size (largest class excluded)", "Max_size_class")
ss <- est_of("model_estimates.csv", "Grazer size (single-class exclosures only)", "Single_size_class")
fig3 <- (orchard_panel(sz, pts_of(sz, "Max_size_class"), SIZE_LV) /
         orchard_panel(ss, pts_of(ss, "Single_size_class"), SIZE_LV)) + plot_annotation(tag_levels = "a")
save_fig(fig3, "Fig3_grazer_size", h = 140)

# ------------------------------------------------------------------ Figure 4: origin + time since exclusion
og <- est_of("model_estimates.csv", "Herbivore origin", "Herbivore_origin")
p_og <- orchard_panel(og, pts_of(og, "Herbivore_origin"), c("Native", "Domestic"))
tf <- read.csv(paste0(TAB, "time_since_exclusion_fit.csv"))
tp <- es |> filter(!is.na(Years_since_exclusion), !is.na(Response_group)) |>
  transmute(response = Response_group, log_years = log(Years_since_exclusion + 1), yi, vi)
tf$response <- factor(tf$response, RESP_GROUPS); tp$response <- factor(tp$response, RESP_GROUPS)
yrs <- c(0, 1, 5, 10, 25, 50, 100)
p_t <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey45") +
  geom_point(data = tp, aes(log_years, yi, size = 1 / sqrt(vi), colour = response), alpha = 0.3, stroke = 0) +
  geom_ribbon(data = tf, aes(log_years, ymin = ci_lb, ymax = ci_ub, fill = response), alpha = 0.2) +
  geom_line(data = tf, aes(log_years, pred, colour = response), linewidth = 0.7) +
  facet_wrap(~ response, scales = "free_y") +
  scale_x_continuous(breaks = log(yrs + 1), labels = yrs) +
  scale_size_continuous(range = c(0.4, 2.8), guide = "none") +
  scale_colour_manual(values = RESP_COL, guide = "none") + scale_fill_manual(values = RESP_COL, guide = "none") +
  labs(x = "Years since grazer exclusion (log scale)", y = "Effect size (lnRR)") + theme_pub()
fig4 <- (p_og / p_t) + plot_layout(heights = c(0.8, 1)) + plot_annotation(tag_levels = "a")
save_fig(fig4, "Fig4_origin_time", h = 120)

# ------------------------------------------------------------------ Figure 5: trophic level
TROPH_LV <- c("Herbivore", "Pollinator", "Omnivore", "Predator", "Parasitoid", "Detritivore",
              "Bacterivore", "Fungivore", "Scavenger", "Algivore")
es_t <- es |> filter(!is.na(Trophic_Level), !Trophic_Level %in% c("Total", "Herbivore/detritivore"))
tr <- est_of("trophic_estimates.csv", "Trophic level", "Trophic_Level")
p_tr <- orchard_panel(tr, pts_of(tr, "Trophic_Level", es_t), intersect(TROPH_LV, tr$level))

# within-origin trophic estimates, native and domestic side by side
to <- bind_rows(
  est_of("trophic_estimates.csv", "Trophic level (native exclusion)", "Trophic_Level") |> mutate(origin = "Native"),
  est_of("trophic_estimates.csv", "Trophic level (domestic exclusion)", "Trophic_Level") |> mutate(origin = "Domestic"))
to_pts <- es_t |> filter(Herbivore_origin %in% c("Native", "Domestic")) |>
  transmute(response = Response_group, level = Trophic_Level, origin = Herbivore_origin, yi, vi) |>
  semi_join(to, by = c("response", "level", "origin"))
lv <- intersect(TROPH_LV, to$level)
off <- c(Native = 0.17, Domestic = -0.17)
rs <- intersect(RESP_GROUPS, to$response)
to <- to |> mutate(response = factor(response, rs), y = match(level, rev(lv)) + off[origin],
                   origin = factor(origin, c("Native", "Domestic")))
to_pts <- to_pts |> mutate(response = factor(response, rs),
                           y = match(level, rev(lv)) + off[origin] + runif(n(), -0.08, 0.08),
                           origin = factor(origin, c("Native", "Domestic")))
p_to <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey45") +
  geom_point(data = to_pts, aes(yi, y, size = 1 / sqrt(vi), colour = origin), alpha = 0.35, stroke = 0) +
  geom_errorbarh(data = to, aes(y = y, xmin = ci_lb, xmax = ci_ub), height = 0, linewidth = 0.7) +
  geom_point(data = to, aes(estimate, y, fill = origin), shape = 21, size = 2, stroke = 0.4) +
  facet_grid(. ~ response, scales = "free_x") +
  scale_y_continuous(breaks = seq_along(lv), labels = rev(lv), expand = expansion(add = 0.6)) +
  scale_x_continuous(sec.axis = pct_axis) +
  scale_size_continuous(range = c(0.4, 2.6), guide = "none") +
  scale_colour_manual(values = ORIGIN_COL, guide = "none") +
  scale_fill_manual(values = ORIGIN_COL, name = "Grazers excluded") +
  labs(x = X_LNRR, y = NULL) + theme_pub() + theme(legend.position = "bottom")
fig5 <- (p_tr / p_to) + plot_layout(heights = c(1.15, 1)) + plot_annotation(tag_levels = "a")
save_fig(fig5, "Fig5_trophic_level", h = 165)

# ------------------------------------------------------------------ Figure 6: biome + plant biomass
bi <- read.csv(paste0(TAB, "biome_estimates.csv")) |> mutate(level = sub("^Biome", "", term))
p_bi <- orchard_panel(bi, pts_of(bi, "Biome"), sort(unique(bi$level)))
lp <- read.csv(paste0(TAB, "plant_link_points.csv")) |> filter(grepl("above", plant_measure))
lf <- read.csv(paste0(TAB, "plant_link_fit.csv")) |> filter(grepl("above", plant_measure))
lp$response <- factor(lp$response, RESP_GROUPS); lf$response <- factor(lf$response, RESP_GROUPS)
p_pl <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey45") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey45") +
  geom_ribbon(data = lf, aes(x, ymin = lb, ymax = ub, fill = response), alpha = 0.2) +
  geom_line(data = lf, aes(x, pred, colour = response), linewidth = 0.7) +
  geom_point(data = lp, aes(plant_lnRR, inv_lnRR, size = 1 / sqrt(inv_vi), fill = response),
             shape = 21, colour = "black", stroke = 0.3, alpha = 0.8) +
  facet_grid(. ~ response, scales = "free") +
  scale_size_continuous(range = c(1, 3.5), guide = "none") +
  scale_colour_manual(values = RESP_COL, guide = "none") + scale_fill_manual(values = RESP_COL, guide = "none") +
  labs(x = "Above-ground plant biomass effect size (lnRR)", y = "Invertebrate effect size (lnRR)") + theme_pub()
fig6 <- (p_bi / (p_pl + plot_spacer() + plot_layout(widths = c(2, 1)))) +
  plot_layout(heights = c(1.5, 1)) + plot_annotation(tag_levels = "a")
save_fig(fig6, "Fig6_biome_plant_biomass", h = 150)

# ------------------------------------------------------------------ Figure S1: funnel plots
# precision (1/SE) on the y-axis; shaded region = overall estimate +/- 1.96 SE
ov_est <- setNames(ov$estimate, ov$response)
fp <- es |> filter(!is.na(Response_group)) |> mutate(response = factor(Response_group, RESP_GROUPS), prec = 1 / sqrt(vi))
reg <- bind_rows(lapply(RESP_GROUPS, function(r) {
  pr <- exp(seq(log(min(fp$prec[fp$Response_group == r])), log(max(fp$prec[fp$Response_group == r])), length.out = 200))
  b <- ov_est[[r]]
  data.frame(response = factor(r, RESP_GROUPS), prec = c(pr, rev(pr)), x = c(b - 1.96 / pr, rev(b + 1.96 / pr)))
}))
xl <- fp |> group_by(response) |> summarise(lo = min(yi), hi = max(yi))
reg <- reg |> left_join(xl, by = "response") |> mutate(x = pmin(pmax(x, lo - 0.3), hi + 0.3))
figS1 <- ggplot() +
  geom_polygon(data = reg, aes(x, prec), fill = "grey92") +
  geom_vline(data = data.frame(response = factor(RESP_GROUPS, RESP_GROUPS), b = ov_est[RESP_GROUPS]),
             aes(xintercept = b), linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey45") +
  geom_point(data = fp, aes(yi, prec, colour = response), alpha = 0.45, size = 0.9, stroke = 0) +
  facet_wrap(~ response, scales = "free") + scale_y_log10(labels = function(x) format(x, drop0trailing = TRUE, trim = TRUE, scientific = FALSE)) +
  scale_colour_manual(values = RESP_COL, guide = "none") +
  labs(x = X_LNRR, y = "Precision (1/SE, log scale)") + theme_pub()
save_fig(figS1, "FigS1_funnel", h = 65)

message("\nDone.")
