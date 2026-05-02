setwd("/Volumes/SSD/climate-risk/")

library(tidyverse)
library(arrow)
library(fixest)
library(broom)
library(sf)
library(tigris)
library(scales)
library(patchwork)
library(ggspatial)

for (pkg in c("biscale", "cowplot")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cran.rstudio.com/")
}
library(biscale)
library(cowplot)

options(tigris_use_cache = TRUE)
dir.create("figures/paper", showWarnings = FALSE, recursive = TRUE)
paper_dir <- "figures/paper"

# ── Shared constants (mirrors 11_eda-viz-paper.r) ─────────────────────────────
FLOOD_COLORS <- c("High" = "#d73027", "Moderate" = "#fc8d59", "Low" = "#4dac26")
SHORE_FIPS   <- c("001", "009", "025", "029")
COUNTY_COLORS <- c(
  "ATLANTIC" = "#E69F00", "CAPE MAY" = "#56B4E9",
  "MONMOUTH" = "#009E73", "OCEAN"    = "#CC79A7"
)

nj_counties <- counties("NJ", cb = TRUE, year = 2023) |> st_transform(4326)
shore_co    <- nj_counties |> filter(COUNTYFP %in% SHORE_FIPS)
tracts_sf   <- tracts("NJ", year = 2023) |>
                 st_transform(4326) |> filter(COUNTYFP %in% SHORE_FIPS)

cafra_mask <- arrow::read_parquet("data/shore_mask.parquet") |>
  mutate(geometry = sf::st_as_sfc(geometry, crs = 3424)) |>
  sf::st_as_sf(crs = 3424) |>
  sf::st_transform(4326)
SHORE_XLIM <- st_bbox(cafra_mask)[c("xmin", "xmax")]
SHORE_YLIM <- st_bbox(cafra_mask)[c("ymin", "ymax")]
SCALE_BAR  <- annotation_scale(
  location = "br", width_hint = 0.25,
  style = "ticks", unit_category = "imperial",
  text_cex = 0.8, line_col = "grey30", text_col = "grey30"
)

# Shared ggplot2 theme for non-map figures
theme_paper <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")

# ── Load and prepare modeling data ────────────────────────────────────────────
message("Loading data...")
df_raw <- read_parquet("data/parcels_shore_clean.parquet")

df_model <- df_raw |>
  filter(
    sale_price_2020 > 0,
    as.integer(sale_year_1) >= 2000 & as.integer(sale_year_1) <= 2025,
    as.numeric(dist_to_ocean_mi) > 0,
    as.numeric(land_value) > 0,
    as.numeric(improvement_value) > 0,
    !is.na(pct_seasonal),
    !is.na(census_tract_geoid),
    !is.na(flood_risk) & flood_risk != "Undetermined",
    !is.na(year_constructed)
  ) |>
  mutate(
    sale_price_2020  = as.numeric(sale_price_2020),
    dist_to_ocean_mi = as.numeric(dist_to_ocean_mi),
    land_value       = as.numeric(land_value),
    improvement_value = as.numeric(improvement_value),
    pct_seasonal     = as.numeric(pct_seasonal),
    sale_year_1      = as.integer(sale_year_1),
    year_constructed = as.integer(year_constructed),

    log_price   = log(sale_price_2020),
    log_dist    = log(dist_to_ocean_mi),
    log_land    = log(land_value),
    log_improve = log(improvement_value),
    age         = 2026L - year_constructed,

    flood_risk = relevel(factor(flood_risk), ref = "Low"),
    year_fe    = factor(sale_year_1),
    tract_fe   = factor(census_tract_geoid),
    county_fe  = factor(mod_iv_county_name)
  ) |>
  filter(age >= 0, age <= 300)

cat(sprintf("Modeling sample: %s observations\n", format(nrow(df_model), big.mark = ",")))

# ── Fit models ─────────────────────────────────────────────────────────────────
message("Fitting models...")

m1 <- feols(log_price ~ log_dist + flood_risk + pct_seasonal +
              log_dist:pct_seasonal + flood_risk:pct_seasonal +
              age + log_land + log_improve,
            data = df_model)

m2 <- feols(log_price ~ log_dist + flood_risk + pct_seasonal +
              log_dist:pct_seasonal + flood_risk:pct_seasonal +
              age + log_land + log_improve | year_fe,
            cluster = ~mod_iv_county_name, data = df_model)

m3 <- feols(log_price ~ log_dist + flood_risk + pct_seasonal +
              log_dist:pct_seasonal + flood_risk:pct_seasonal +
              age + log_land + log_improve | year_fe + county_fe,
            cluster = ~mod_iv_county_name, data = df_model)

m4 <- feols(log_price ~ log_dist + flood_risk + age + log_land + log_improve |
              year_fe + tract_fe,
            cluster = ~tract_fe, data = df_model)

message("Models fitted.")

# ── Fig 5: Coefficient plot ────────────────────────────────────────────────────
message("Building fig5: coefficient plot...")

KEY_VARS <- c(
  "log_dist"                        = "log(Distance to Ocean)",
  "flood_riskHigh"                  = "Flood Risk: High",
  "flood_riskModerate"              = "Flood Risk: Moderate",
  "pct_seasonal"                    = "% Seasonal Units",
  "log_dist:pct_seasonal"           = "log(Distance) × % Seasonal",
  "flood_riskHigh:pct_seasonal"     = "High Risk × % Seasonal",
  "flood_riskModerate:pct_seasonal" = "Moderate Risk × % Seasonal"
)

extract_coefs <- function(model, label) {
  tidy(model, conf.int = TRUE) |>
    filter(term %in% names(KEY_VARS)) |>
    mutate(term_label = KEY_VARS[term], model_label = label)
}

coef_df <- bind_rows(
  extract_coefs(m1, "OLS"),
  extract_coefs(m2, "Year FE"),
  extract_coefs(m3, "Year + County FE")
) |>
  mutate(
    term_label  = factor(term_label, levels = rev(unname(KEY_VARS))),
    model_label = factor(model_label, levels = c("OLS", "Year FE", "Year + County FE"))
  )

fig5_coefplot <- ggplot(coef_df,
    aes(x = estimate, y = term_label,
        color = model_label, shape = model_label)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.65),
                  linewidth = 0.7, size = 0.5) +
  scale_color_manual(
    values = c("OLS" = "#aaaaaa",
               "Year FE" = "#4393c3",
               "Year + County FE" = "#2166ac"),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("OLS" = 16, "Year FE" = 17, "Year + County FE" = 15),
    name = NULL
  ) +
  labs(x = "Coefficient (log sale price, 2020 USD)", y = NULL) +
  theme_paper +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(color = "#f5f5f5"))

ggsave(file.path(paper_dir, "fig5_coefplot.png"),
       fig5_coefplot, width = 9, height = 6, dpi = 300, bg = "white")
message("Saved fig5_coefplot.png")

# ── Fig 6: Predicted log-price gradient — flood risk × seasonality ─────────────
# Uses delta-method on M3 coefficients to show how the flood-risk discount
# grows or shrinks across the seasonality spectrum.
message("Building fig6: margins/interaction plot...")

b     <- coef(m3)
V     <- as.matrix(vcov(m3))
x_seq <- seq(0, 0.80, by = 0.005)

# Effect = change in predicted log price relative to (flood_risk=Low, pct_seasonal=0).
# Low:  Δ = β_seasonal * x
# High: Δ = β_High + (β_seasonal + β_High:seasonal) * x
# Moderate: Δ = β_Mod + (β_seasonal + β_Mod:seasonal) * x

delta_ci <- function(flood, x) {
  if (flood == "Low") {
    est <- b["pct_seasonal"] * x
    grad <- matrix(0, nrow = length(x), ncol = length(b))
    colnames(grad) <- names(b)
    grad[, "pct_seasonal"] <- x
  } else {
    bname <- paste0("flood_risk", flood)
    iname <- paste0("flood_risk", flood, ":pct_seasonal")
    est   <- b[bname] + (b["pct_seasonal"] + b[iname]) * x
    grad  <- matrix(0, nrow = length(x), ncol = length(b))
    colnames(grad) <- names(b)
    grad[, bname]           <- 1
    grad[, "pct_seasonal"]  <- x
    grad[, iname]           <- x
  }
  vars <- diag(grad %*% V %*% t(grad))
  tibble(
    pct_seasonal = x,
    flood_risk   = flood,
    estimate     = est,
    conf.low     = est - 1.96 * sqrt(pmax(vars, 0)),
    conf.high    = est + 1.96 * sqrt(pmax(vars, 0))
  )
}

pred_df <- bind_rows(
  delta_ci("Low",      x_seq),
  delta_ci("High",     x_seq),
  delta_ci("Moderate", x_seq)
) |>
  mutate(flood_risk = factor(flood_risk, levels = c("Low", "Moderate", "High")))

fig6_margins <- ggplot(pred_df,
    aes(x = pct_seasonal, y = estimate,
        color = flood_risk, fill = flood_risk)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 0.8, 0.2)) +
  scale_color_manual(values = FLOOD_COLORS,
                     breaks = c("High", "Moderate", "Low"),
                     name = "Flood risk") +
  scale_fill_manual(values = FLOOD_COLORS,
                    breaks = c("High", "Moderate", "Low"),
                    name = "Flood risk") +
  labs(
    x = "Share of seasonal housing units (census tract)",
    y = "Predicted change in log sale price\n(relative to low-risk, non-seasonal baseline)"
  ) +
  theme_paper

ggsave(file.path(paper_dir, "fig6_margins_interaction.png"),
       fig6_margins, width = 8, height = 5, dpi = 300, bg = "white")
message("Saved fig6_margins_interaction.png")

# ── Fig 7: Time trend — median real sale price by flood risk ──────────────────
# Deduplicate so frozen prices aren't counted multiple times:
# keep one row per unique (parcel, sale_year_1) event.
message("Building fig7: time trend...")

time_id_col <- if ("gis_pin" %in% names(df_model)) "gis_pin" else "census_tract_geoid"

time_trend <- df_model |>
  distinct(across(all_of(c(time_id_col, "sale_year_1"))), .keep_all = TRUE) |>
  filter(sale_year_1 >= 2015, sale_year_1 <= 2025,
         flood_risk %in% c("Low", "Moderate", "High")) |>
  group_by(sale_year_1, flood_risk) |>
  summarise(
    median_price = median(sale_price_2020, na.rm = TRUE),
    n            = n(),
    .groups = "drop"
  ) |>
  mutate(flood_risk = factor(flood_risk, levels = c("Low", "Moderate", "High")))

fig7_trend <- ggplot(time_trend,
    aes(x = sale_year_1, y = median_price,
        color = flood_risk, group = flood_risk)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = n), alpha = 0.85) +
  scale_x_continuous(breaks = 2015:2025, minor_breaks = NULL) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale()),
                     limits = c(0, NA)) +
  scale_color_manual(values = FLOOD_COLORS,
                     breaks = c("High", "Moderate", "Low"),
                     name = "Flood risk") +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  labs(x = "Sale year",
       y = "Median sale price (2020 USD)") +
  theme_paper

ggsave(file.path(paper_dir, "fig7_time_trend.png"),
       fig7_trend, width = 9, height = 5, dpi = 300, bg = "white")
message("Saved fig7_time_trend.png")

# ── Fig 8: Violin — sale price by flood risk × seasonality quartile ───────────
message("Building fig8: violin plot...")

seasonal_breaks <- quantile(df_model$pct_seasonal,
                            probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
seasonal_labels <- c("Q1\n(low seasonal)", "Q2", "Q3", "Q4\n(high seasonal)")

fig8_data <- df_model |>
  filter(flood_risk %in% c("Low", "High")) |>
  mutate(
    seasonal_q = cut(pct_seasonal,
                     breaks  = seasonal_breaks,
                     labels  = seasonal_labels,
                     include.lowest = TRUE),
    flood_risk = factor(flood_risk, levels = c("Low", "High"))
  ) |>
  filter(!is.na(seasonal_q))

fig8_violin <- ggplot(fig8_data,
    aes(x = seasonal_q, y = log_price, fill = flood_risk)) +
  geom_violin(alpha = 0.5, color = NA, scale = "width", trim = TRUE,
              position = position_dodge(width = 0.85)) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "grey30",
               linewidth = 0.4,
               position = position_dodge(width = 0.85)) +
  scale_fill_manual(
    values = c("Low" = "#4dac26", "High" = "#d73027"),
    name = "Flood risk"
  ) +
  scale_y_continuous(
    breaks = log(c(25000, 75000, 200000, 500000, 1500000)),
    labels = c("$25K", "$75K", "$200K", "$500K", "$1.5M")
  ) +
  labs(
    x = "Seasonal housing share quartile (census tract)",
    y = "Sale price (2020 USD, log scale)"
  ) +
  theme_paper

ggsave(file.path(paper_dir, "fig8_price_flood_seasonal.png"),
       fig8_violin, width = 9, height = 5, dpi = 300, bg = "white")
message("Saved fig8_price_flood_seasonal.png")

# ── Fig 9: Scatter — tract % seasonal vs % high-risk parcels ─────────────────
message("Building fig9: scatter plot...")

tract_scatter <- df_model |>
  filter(!is.na(census_tract_geoid)) |>
  group_by(census_tract_geoid, mod_iv_county_name) |>
  summarise(
    pct_seasonal   = first(pct_seasonal),
    pct_high_flood = mean(flood_risk == "High", na.rm = TRUE),
    n_parcels      = n(),
    .groups = "drop"
  )

fig9_scatter <- ggplot(tract_scatter,
    aes(x = pct_seasonal, y = pct_high_flood)) +
  geom_point(aes(color = mod_iv_county_name, size = n_parcels), alpha = 0.65) +
  geom_smooth(method = "lm", se = TRUE,
              color = "black", fill = "grey80",
              linewidth = 0.8, alpha = 0.25) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_manual(values = COUNTY_COLORS, name = "County") +
  scale_size_continuous(range = c(1.5, 6), guide = "none") +
  labs(
    x = "Share of seasonal housing units (census tract)",
    y = "Share of high-risk parcels (census tract)"
  ) +
  theme_paper

ggsave(file.path(paper_dir, "fig9_scatter_seasonal_flood.png"),
       fig9_scatter, width = 8, height = 5, dpi = 300, bg = "white")
message("Saved fig9_scatter_seasonal_flood.png")

# ── Fig 10: Bivariate map — flood risk + seasonality ─────────────────────────
message("Building fig10: bivariate map...")

tract_bi <- df_model |>
  group_by(census_tract_geoid) |>
  summarise(
    pct_seasonal   = first(pct_seasonal),
    pct_high_flood = mean(flood_risk == "High", na.rm = TRUE),
    .groups = "drop"
  )

tract_bi_sf <- tracts_sf |>
  left_join(tract_bi, by = c("GEOID" = "census_tract_geoid")) |>
  filter(!is.na(pct_seasonal), !is.na(pct_high_flood)) |>
  bi_class(x = pct_seasonal, y = pct_high_flood,
           style = "quantile", dim = 3)

map_bi <- ggplot() +
  geom_sf(data = nj_counties, fill = "#e0e0e0", color = "#888888",
          linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = tract_bi_sf, aes(fill = bi_class),
          color = "white", linewidth = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  geom_sf(data = shore_co, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(xlim = SHORE_XLIM, ylim = SHORE_YLIM, expand = FALSE) +
  SCALE_BAR +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

legend_bi <- bi_legend(pal = "DkBlue", dim = 3,
                        xlab = "More seasonal →",
                        ylab = "More flood risk →",
                        size = 8)

fig10_bivariate <- ggdraw(map_bi) +
  draw_plot(legend_bi, x = 0.02, y = 0.02, width = 0.30, height = 0.22)

ggsave(file.path(paper_dir, "fig10_bivariate_map.png"),
       fig10_bivariate, width = 6, height = 9, dpi = 300, bg = "white")
message("Saved fig10_bivariate_map.png")

message("\nAll new figures saved to figures/paper/")
