# Shared helpers for the grazing-exclusion invertebrate meta-analysis.
# Effect size: lnRR = ln(mean exclosure / mean grazed)
#   > 0  -> invertebrate metric is higher where grazers were excluded
#   < 0  -> metric is lower where grazers were excluded

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(metafor)
  library(clubSandwich)
  library(ggplot2)
})

DATA_FILE <- "MetaAnalysis_data_v2.xlsx"
RESP_GROUPS <- c("Abundance", "Richness", "Diversity")
MIN_STUDIES_PER_LEVEL <- 3   # moderator levels with fewer studies are dropped from that model

# ------------------------------------------------------------------ data
col_like <- function(df, prefix) {
  hit <- names(df)[startsWith(names(df), prefix)]
  stopifnot(length(hit) == 1)
  df[[hit]]
}

se_to_sd <- function(x, n, type) ifelse(toupper(type) == "SD", x, x * sqrt(n))

load_inverts <- function(path = DATA_FILE) {
  raw <- read_excel(path, sheet = "Invertebrates", guess_max = 5000, .name_repair = "unique_quiet")
  d <- tibble(
    excel_row     = seq_len(nrow(raw)) + 1,
    Study_ID      = trimws(raw$Study_ID),
    Author_Year   = trimws(raw$Author_Year),
    Location_ID   = col_like(raw, "Location_ID"),
    Measured      = col_like(raw, "Measured"),
    Order         = raw$Order,
    Family        = raw$Family,
    Common_name   = raw$`Common name`,
    Trophic_Level = raw$Trophic_Level,
    Sampling_date = as.character(raw$Sampling_date),
    variation     = raw$variation,
    m_g = as.numeric(raw$Mean_Grazed),  se_g = as.numeric(raw$SD_or_SE_Grazed),  n_g = as.numeric(raw$n_Grazed),
    m_c = as.numeric(raw$Mean_Control), se_c = as.numeric(raw$SD_or_SE_Control), n_c = as.numeric(raw$n_Control),
    Stratum          = factor(raw$Stratum, levels = c("Above-ground", "Ground & litter-dwelling", "Below-ground")),
    Herbivore_origin = raw$Herbivore_origin,
    Max_size_class   = factor(raw$Max_size_class, levels = c("Small", "Medium", "Large", "Mega")),
    Single_size_class = factor(raw$Single_size_class, levels = c("Small", "Medium", "Large", "Mega")),
    Response_group   = raw$Response_group,
    Design           = raw$Design,
    Years_since_exclusion = as.numeric(raw$Years_since_exclusion)
  ) |>
    filter(!is.na(Study_ID))

  # rows whose zero means were offset by a constant in v2 (for sensitivity analysis)
  log <- read_excel(path, sheet = "Cleaning_log")
  zero_rows <- unique(log$Excel_row[grepl("^zero mean", log$Reason %||% "") & log$Sheet == "Invertebrates"])
  d$zero_adjusted <- d$excel_row %in% zero_rows

  d |>
    mutate(sd_g = se_to_sd(se_g, n_g, variation),
           sd_c = se_to_sd(se_c, n_c, variation))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

compute_lnrr <- function(d) {
  n_before <- nrow(d)
  d <- d |> filter(!is.na(n_g), !is.na(n_c), !is.na(sd_g), !is.na(sd_c), !is.na(Response_group))
  message(sprintf("Dropped %d rows with missing n, SE or response type", n_before - nrow(d)))
  d <- escalc("ROM", m1i = m_c, sd1i = sd_c, n1i = n_c,
                     m2i = m_g, sd2i = sd_g, n2i = n_g, data = d)
  d$es_id <- seq_len(nrow(d))
  d
}

# Variance-covariance matrix accounting for shared groups: when several
# comparisons within a study re-use the same exclosure (or grazed) group,
# their sampling errors covary by that group's contribution sd^2 / (n m^2).
build_V <- function(d) {
  key <- function(m, s, n) paste(d$Study_ID, d$Response_group, d$Order, d$Family, d$Common_name,
                                 d$Trophic_Level, d$Stratum, d$Sampling_date,
                                 signif(m, 8), signif(s, 8), n, sep = "|")
  kc <- key(d$m_c, d$sd_c, d$n_c)
  kg <- key(d$m_g, d$sd_g, d$n_g)
  vc <- d$sd_c^2 / (d$n_c * d$m_c^2)
  vg <- d$sd_g^2 / (d$n_g * d$m_g^2)
  V <- (outer(kc, kc, "==") * outer(sqrt(vc), sqrt(vc))) +
       (outer(kg, kg, "==") * outer(sqrt(vg), sqrt(vg)))
  diag(V) <- d$vi
  V
}

# ------------------------------------------------------------------ models
fit_mlma <- function(d, mods = ~ 1) {
  V <- build_V(d)
  rma.mv(yi, V, mods = mods, data = d,
         random = ~ 1 | Location_ID / Study_ID / es_id,
         method = "REML", test = "t", dfs = "contain", sparse = TRUE)
}

# cluster-robust (CR2) inference, clustered at the top level (location)
robustify <- function(m, d) robust(m, cluster = d$Location_ID, clubSandwich = TRUE)

# multilevel I^2 (Nakagawa & Santos 2012)
i2_ml <- function(m) {
  W <- solve(m$V)
  X <- model.matrix(m)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  typical_v <- (m$k - m$p) / sum(diag(P))
  s <- m$sigma2
  out <- 100 * c(s, sum(s)) / (sum(s) + typical_v)
  setNames(round(out, 1), c("I2_location", "I2_study", "I2_within", "I2_total"))
}

pct <- function(x) round(100 * (exp(x) - 1), 1)

# tidy table of coefficient estimates with model-based and robust inference
tidy_fit <- function(m, mr, d, analysis, response, level_var = NULL) {
  est <- data.frame(
    analysis = analysis, response = response, term = rownames(m$b),
    estimate = round(m$b[, 1], 3), ci_lb = round(m$ci.lb, 3), ci_ub = round(m$ci.ub, 3),
    p = signif(m$pval, 3),
    robust_ci_lb = round(mr$ci.lb, 3), robust_ci_ub = round(mr$ci.ub, 3), robust_p = signif(mr$pval, 3),
    pct_change = pct(m$b[, 1]), pct_ci_lb = pct(m$ci.lb), pct_ci_ub = pct(m$ci.ub),
    row.names = NULL
  )
  if (!is.null(level_var)) {
    cnt <- d |> group_by(level = .data[[level_var]]) |>
      summarise(k = n(), n_studies = n_distinct(Study_ID), n_locations = n_distinct(Location_ID), .groups = "drop")
    est$level <- sub(paste0("^", level_var), "", est$term)
    est <- left_join(est, cnt, by = "level") |> select(-level)
  } else {
    est$k <- m$k; est$n_studies <- n_distinct(d$Study_ID); est$n_locations <- n_distinct(d$Location_ID)
  }
  est
}

# drop moderator levels backed by too few studies
keep_levels <- function(d, var, min_studies = MIN_STUDIES_PER_LEVEL) {
  ok <- d |> filter(!is.na(.data[[var]])) |>
    group_by(.data[[var]]) |> summarise(ns = n_distinct(Study_ID), .groups = "drop") |>
    filter(ns >= min_studies) |> pull(1)
  dropped <- setdiff(unique(as.character(na.omit(d[[var]]))), as.character(ok))
  if (length(dropped)) message(sprintf("  %s: dropped level(s) with < %d studies: %s",
                                       var, min_studies, paste(dropped, collapse = ", ")))
  out <- d |> filter(.data[[var]] %in% ok)
  out[[var]] <- droplevels(factor(out[[var]], levels = intersect(levels(factor(d[[var]])), as.character(ok))))
  out
}

# ------------------------------------------------------------------ plots
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e4e3df"; SERIES <- "#2a78d6"

theme_meta <- function() {
  theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = GRID, linewidth = 0.3),
          axis.text = element_text(colour = INK2), axis.title = element_text(colour = INK2),
          strip.text = element_text(colour = INK, face = "bold", hjust = 0),
          plot.title = element_text(colour = INK, face = "bold"),
          plot.subtitle = element_text(colour = INK2),
          legend.position = "bottom", legend.text = element_text(colour = INK2),
          legend.title = element_text(colour = INK2))
}

# orchard-style plot: raw effects (sized by precision), pooled estimate,
# 95% CI (thick) and 95% prediction interval (thin)
orchard <- function(eff, summ, level_lab, title, subtitle = NULL) {
  ggplot() +
    geom_vline(xintercept = 0, colour = INK2, linetype = "dashed", linewidth = 0.4) +
    geom_jitter(data = eff, aes(x = yi, y = level, size = 1 / sqrt(vi)),
                colour = SERIES, alpha = 0.3, height = 0.18, width = 0, stroke = 0) +
    geom_errorbarh(data = summ, aes(y = level, xmin = pi_lb, xmax = pi_ub), height = 0,
                   colour = INK, linewidth = 0.5) +
    geom_errorbarh(data = summ, aes(y = level, xmin = ci_lb, xmax = ci_ub), height = 0,
                   colour = INK, linewidth = 1.6) +
    geom_point(data = summ, aes(x = estimate, y = level), shape = 21, fill = "white",
               colour = INK, size = 3, stroke = 1) +
    geom_text(data = summ, aes(x = Inf, y = level, label = lab), hjust = 1.05, vjust = -0.9,
              size = 3, colour = INK2) +
    facet_wrap(~ response, ncol = 1, scales = "free_y") +
    scale_size_continuous(range = c(1, 5), guide = "none") +
    labs(x = "lnRR  (ln exclosure / grazed; > 0 = higher without grazers)", y = level_lab,
         title = title, subtitle = subtitle) +
    theme_meta()
}
