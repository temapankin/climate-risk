setwd("/Volumes/SSD/climate-risk/")

library(tidyverse)
library(sf)
library(tigris)
library(arrow)
library(scales)
library(patchwork)
library(ggspatial)

options(tigris_use_cache = TRUE)
dir.create("figures", showWarnings = FALSE)

paper_dir <- "figures/paper"
dir.create(paper_dir, showWarnings = FALSE)

SHORE_FIPS <- c("001", "009", "025", "029")

nj_counties <- counties("NJ", cb = TRUE, year = 2023) |> st_transform(4326)
shore_co    <- nj_counties |> filter(COUNTYFP %in% SHORE_FIPS)
cousub      <- county_subdivisions("NJ", year = 2023) |>
                 st_transform(4326) |> filter(COUNTYFP %in% SHORE_FIPS)
tracts_sf   <- tracts("NJ", year = 2023) |>
                 st_transform(4326) |> filter(COUNTYFP %in% SHORE_FIPS)

parcels <- open_dataset("data/parcels_shore_clean.parquet") |>
  filter(mod_iv_year == 2023) |>
  select(sale_price, flood_risk,
         dist_to_ocean_mi, pct_seasonal, census_tract_geoid) |>
  collect() |>
  mutate(across(c(sale_price, dist_to_ocean_mi, pct_seasonal), as.numeric))

# ── Figure 1: Study area — shore parcels coloured by county ─────────────────

cafra_mask <- arrow::read_parquet("data/shore_mask.parquet") |>
  mutate(geometry = sf::st_as_sfc(geometry, crs = 3424)) |>
  sf::st_as_sf(crs = 3424) |>
  sf::st_transform(4326) |>
  mutate(layer = "Study area (CAFRA)")

# Shared geographic extent for all paper maps (consistent crop)
PADDING <- 0.1  # degrees; increase to show more area
SHORE_XLIM <- st_bbox(cafra_mask)[c("xmin", "xmax")] + c(-PADDING, PADDING)
SHORE_YLIM <- st_bbox(cafra_mask)[c("ymin", "ymax")] + c(-PADDING, PADDING)

# Common scale bar style — 
SCALE_BAR <- annotation_scale(
  location = "br", width_hint = 0.22,
  style = "ticks", unit_category = "imperial",
  text_cex = 0.75,
  #pad_x = unit(0.1, "cm"), pad_y = unit(0.25, "cm"),
  line_col = "grey30", text_col = "grey30"
)

# 2023 shore parcels with geometry (~400 k polygons, loads in ~1 min)
parcels_geo <- open_dataset("data/parcels_shore.parquet") |>
  filter(mod_iv_year == 2023) |>
  select(mod_iv_county_name, geometry) |>
  collect() |>
  mutate(geometry = sf::st_as_sfc(geometry, crs = 3424)) |>
  sf::st_as_sf(crs = 3424) |>
  sf::st_transform(4326)

COUNTY_COLORS <- c(
  "ATLANTIC" = "#E69F00",
  "CAPE MAY" = "#56B4E9",
  "MONMOUTH" = "#009E73",
  "OCEAN"    = "#CC79A7"
)

fig1 <- ggplot() +
  geom_sf(data = nj_counties, fill = "#e0e0e0", color = "#888888",
          linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = cafra_mask, fill = "lightyellow", aes(color = layer),
          linewidth = 0.8, alpha = 0.35) +
  geom_sf(data = parcels_geo, aes(fill = mod_iv_county_name),
          color = NA, alpha = 0.55) +
  scale_fill_manual(values = COUNTY_COLORS, name = "County") +
  scale_color_manual(
    values = c("Study area (CAFRA)" = "#DC143C"),
    name   = NULL,
    guide  = guide_legend(
      override.aes = list(fill = "lightyellow", alpha = 0.5, linewidth = 1)
    )
  ) +
  coord_sf(
    xlim = st_bbox(cafra_mask)[c("xmin", "xmax")],
    ylim = st_bbox(cafra_mask)[c("ymin", "ymax")]
  ) +
 theme_void() +
  labs(title = NULL) +
  coord_sf(xlim = SHORE_XLIM, ylim = SHORE_YLIM, expand = FALSE) +
  SCALE_BAR +
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))

ggsave(file.path(paper_dir, "fig1_parcels_county.png"), fig1, width = 5, height = 10, dpi = 300, bg = "white")



# Figure 2a: Median Sale Price by Census Tract (2023)
tract_stats <- parcels |>
  filter(sale_price > 1000, !is.na(census_tract_geoid)) |>
  group_by(census_tract_geoid) |>
  summarise(median_price = median(sale_price, na.rm = TRUE), .groups = "drop")

tract_sales <- tracts_sf |>
  left_join(tract_stats, by = c("GEOID" = "census_tract_geoid"))

# Bin tract medians into groups that mirror the choropleth color breaks
PRICE_BREAKS  <- c(0, 1e5, 3e5, 6e5, 1e6, 2e6, Inf)
PRICE_LABELS  <- c("<$100K", "$100K–$300K", "$300K–$600K",
                   "$600K–$1M", "$1M–$2M", ">$2M")
BLUE_COLORS <- viridisLite::viridis(6, option = "mako", begin = 0.15, end = 0.95)

price_bins <- tract_stats |>
  filter(!is.na(median_price)) |>
  mutate(bin = cut(median_price, breaks = PRICE_BREAKS, labels = PRICE_LABELS,
                   include.lowest = TRUE)) |>
  count(bin) |>
  filter(!is.na(bin)) |>
  mutate(
    pct         = n / sum(n),
    label_hjust = if_else(pct > 0.10, 1.15, -0.15),
    label_color = if_else(pct > 0.10, "white", "black")
  )

bar_inset <- ggplot(price_bins, aes(x = bin, y = n, fill = bin)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(pct, accuracy = 1),
                hjust = label_hjust, color = label_color),
            size = 3) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(BLUE_COLORS, PRICE_LABELS)) +
  scale_color_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  guides(fill = "none") +
  labs(title = "Tracts by median price", x = NULL, y = NULL) +
  theme_void(base_size = 7) +
  theme(
    plot.background = element_blank(),
    plot.title      = element_text(hjust = 0, face = "plain", margin = margin(0, 0, 4, 0), size = 8),
    axis.text.y     = element_text(size = 6, hjust = 1, margin = margin(r = 5)),
    axis.line.y     = element_line(color = "black", linewidth = 0.4),
    plot.margin     = margin(4, 6, 4, 6)
  )

map_base <- ggplot(tract_sales) +
  geom_sf(aes(fill = median_price), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "mako", begin = 0.15, end = 0.95,
    trans = "log10", na.value = "transparent",
    name   = "Median\nsale price",
    labels = label_dollar(scale_cut = cut_short_scale()),
    breaks = c(1e5, 3e5, 6e5, 1e6, 2e6)
  ) +
  labs(title = "Median Sale Price by Census Tract (2023)",
       x = NULL, y = NULL, fontsize = 15, ) +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.title.position = "plot",
    legend.position = "right"
  )

# ── Paper version (NJ background, coord crop, scale bar, no title) ───────────

# Mirrored bar inset: bars go right→left so labels sit on the right edge
price_bins_r <- price_bins |>
  mutate(
    label_hjust_r = if_else(pct > 0.10, -0.15, 1.15),
    label_color   = if_else(pct > 0.10, "white", "black")
  )

bar_inset_right <- ggplot(price_bins_r, aes(x = bin, y = n, fill = bin)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(pct, accuracy = 1),
                hjust = label_hjust_r, color = label_color),
            size = 3) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(BLUE_COLORS, PRICE_LABELS)) +
  scale_color_identity() +
  scale_y_reverse(expand = expansion(mult = c(0.22, 0))) +
  scale_x_discrete(position = "top") +
  guides(fill = "none") +
  labs(title = "Tracts by median price", x = NULL, y = NULL) +
  theme_void(base_size = 7) +
  theme(
    plot.background   = element_blank(),
    plot.title        = element_text(hjust = 1, face = "plain",
                                     margin = margin(0, 0, 4, 0), size = 8),
    axis.text.y.right = element_text(size = 6, hjust = 0, margin = margin(l = 4)),
    axis.line.y.right = element_line(color = "black", linewidth = 0.4),
    plot.margin       = margin(4, 2, 4, 6)
  )

map_paper_2a <- ggplot() +
  geom_sf(data = nj_counties, fill = "#e0e0e0", color = "#888888",
          linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = tract_sales, aes(fill = median_price),
          color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "mako", begin = 0.15, end = 0.95,
    trans = "log10", na.value = "transparent",
    name   = "Median\nsale price",
    labels = label_dollar(scale_cut = cut_short_scale()),
    breaks = c(1e5, 3e5, 6e5, 1e6, 2e6)
  ) +
  coord_sf(xlim = SHORE_XLIM, ylim = SHORE_YLIM, expand = FALSE) +
  SCALE_BAR +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position      = c(1.1, 0.6),
        legend.justification = c(1, 0.8),
        plot.margin = margin(0, 0, 0, -50),
        legend.box.margin = margin(0, 0, 0, 0)
)

# bar_inset_right placed flush at the far right; bars extend leftward into map
fig2a_paper <- map_paper_2a +
  inset_element(bar_inset_right, left = 0.5, bottom = 0.05, right = 1.1, top = 0.35)

ggsave(file.path(paper_dir, "fig2a_sales_map.png"),
       fig2a_paper, width = 5.8, height = 9, dpi = 300, bg = "white")

# ── Figure 2b: Sale Price Distribution (log scale) ───────────────────────────
sales <- parcels |> filter(sale_price > 1000)

fig2b <- ggplot(sales, aes(x = log10(sale_price))) +
  geom_histogram(bins = 60, fill = "#2166ac", color = NA, alpha = 0.85) +
  scale_x_continuous(
    breaks = 3:7,
    labels = c("$1K", "$10K", "$100K", "$1M", "$10M")
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Sale Price Distribution (2023, arms-length sales)",
       x = "Sale price (log scale)", y = "Count")

ggsave("figures/fig2b_sales_hist.png", fig2b, width = 8, height = 5, dpi = 300, bg = "white")

# ── Paper version (no title) ─────────────────────────────────────────────────
fig2b_paper <- fig2b + labs(title = NULL)
ggsave(file.path(paper_dir, "fig2b_sales_hist.png"),
       fig2b_paper, width = 8, height = 5, dpi = 300, bg = "white")

# ── Figure 2c: Parcels by Flood Risk Category ─────────────────────────────────
FLOOD_COLORS <- c("High" = "#d73027", "Moderate" = "#fc8d59",
                  "Low" = "#4dac26", "Undetermined" = "grey60")

flood_counts <- parcels |>
  mutate(flood_risk = if_else(is.na(flood_risk) | !flood_risk %in% names(FLOOD_COLORS),
                              "Undetermined", flood_risk)) |>
  count(flood_risk) |>
  mutate(flood_risk = factor(flood_risk, levels = names(FLOOD_COLORS)))

fig2c <- ggplot(flood_counts, aes(x = flood_risk, y = n, fill = flood_risk)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(n / sum(n), accuracy = 0.1)),
            vjust = -0.5, size = 3.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = FLOOD_COLORS) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  guides(fill = "none") +
  labs(title = "Parcels by Flood Risk Category (2023)",
       x = NULL, y = "Parcel count")

ggsave("figures/fig2c_flood_bar.png", fig2c, width = 8, height = 5, dpi = 300, bg = "white")

# ── Paper version (no title) ─────────────────────────────────────────────────
fig2c_paper <- fig2c + labs(title = NULL)
ggsave(file.path(paper_dir, "fig2c_flood_bar.png"),
       fig2c_paper, width = 8, height = 5, dpi = 300, bg = "white")

# ── Figure 3: Distance to Ocean by Flood Risk ─────────────────────────────────
dist_data <- parcels |>
  filter(flood_risk %in% c("High", "Moderate", "Low"), !is.na(dist_to_ocean_mi))

fig3 <- ggplot(dist_data, aes(x = dist_to_ocean_mi, fill = flood_risk)) +
  geom_density(alpha = 0.45, color = NA) +
  scale_fill_manual(
    values = c("High" = "#d73027", "Moderate" = "#fc8d59", "Low" = "#4dac26"),
    name   = "Flood risk"
  ) +
  labs(title = "Distance to Ocean by Flood Risk Category (2023)",
       x = "Distance to ocean (miles)", y = "Density")

ggsave("figures/fig3_flood_distance.png", fig3, width = 8, height = 5, dpi = 300, bg = "white")

# ── Paper version (no title, legend in risk-severity order) ──────────────────
fig3_paper <- fig3 +
  labs(title = NULL) +
  scale_fill_manual(
    values = c("High" = "#d73027", "Moderate" = "#fc8d59", "Low" = "#4dac26"),
    breaks = c("High", "Moderate", "Low"),
    name   = "Flood risk"
  )
ggsave(file.path(paper_dir, "fig3_flood_distance.png"),
       fig3_paper, width = 8, height = 5, dpi = 300, bg = "white")

# ── Figure 4: Seasonal Housing Share by Census Tract ─────────────────────────
tract_seasonal <- tracts_sf |>
  left_join(
    parcels |> distinct(census_tract_geoid, pct_seasonal),
    by = c("GEOID" = "census_tract_geoid")
  )

fig4 <- ggplot() +
  geom_sf(data = tract_seasonal, aes(fill = pct_seasonal),
          color = "transparent", linewidth = 0.05) +
  geom_sf(data = shore_co, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_distiller(
    palette  = "YlGnBu", direction = 1, na.value = "transparent",
    name     = "% Seasonal\nhousing units",
    labels   = label_percent(accuracy = 1)
  ) +
  labs(title = "Share of Seasonal Housing Units by Census Tract (2023 ACS)",
       x = NULL, y = NULL,) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.title.position = "plot",
    legend.position = "right"
  )

ggsave("figures/fig4_seasonal.png", fig4, width = 7, height = 9, dpi = 300, bg = "white")

# ── Paper version (NJ background, coord crop, scale bar, no title) ───────────
fig4_paper <- ggplot() +
  geom_sf(data = nj_counties, fill = "#e0e0e0", color = "#888888",
          linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = tract_seasonal, aes(fill = pct_seasonal),
          color = "transparent", linewidth = 0.05) +
 # geom_sf(data = shore_co, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_distiller(
    palette = "YlGnBu", direction = 1, na.value = "transparent",
    name    = "% Seasonal\nhousing units",
    labels  = label_percent(accuracy = 1)
  ) +
  coord_sf(xlim = SHORE_XLIM, ylim = SHORE_YLIM, expand = FALSE) +
  SCALE_BAR +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "right",
        plot.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))
ggsave(file.path(paper_dir, "fig4_seasonal.png"),
       fig4_paper, width = 5.5, height = 8, dpi = 300, bg = "white")

# ── Model variable distributions ─────────────────────────────────────────────
# Loads the same cleaned sample used in 20_models_hedonic.r

df_model <- read_parquet("data/parcels_shore_clean.parquet") |>
  filter(
    sale_price_2020 > 0,
    sale_year_1 >= 2000 & sale_year_1 <= 2025,
    dist_to_ocean_mi > 0,
    land_value > 0,
    improvement_value > 0,
    !is.na(pct_seasonal),
    !is.na(census_tract_geoid),
    !is.na(flood_risk) & flood_risk != "Undetermined",
    !is.na(year_constructed)
  ) |>
  mutate(
    across(c(sale_price_2020, dist_to_ocean_mi, land_value,
             improvement_value, pct_seasonal), as.numeric),
    age        = 2026 - as.integer(year_constructed),
    flood_risk = factor(flood_risk, levels = c("Low", "Moderate", "High"))
  ) |>
  filter(age >= 0, age <= 300)

# Shared style helpers
HIST_FILL  <- "#2166ac"
HIST_ALPHA <- 0.85

# figS1: Sale price in 2020 USD (log scale)
figS1 <- ggplot(df_model, aes(x = log10(sale_price_2020))) +
  geom_histogram(bins = 60, fill = HIST_FILL, color = NA, alpha = HIST_ALPHA) +
  scale_x_continuous(
    breaks = 3:7,
    labels = c("$1K", "$10K", "$100K", "$1M", "$10M")
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Sale price, 2020 USD (log scale)", y = "Count")

ggsave(file.path(paper_dir, "figS1_price2020_hist.png"),
       figS1, width = 6, height = 4, dpi = 300, bg = "white")

# figS2: Distance to ocean (log scale)
figS2 <- ggplot(df_model, aes(x = log10(dist_to_ocean_mi))) +
  geom_histogram(bins = 50, fill = HIST_FILL, color = NA, alpha = HIST_ALPHA) +
  scale_x_continuous(
    breaks = log10(c(0.1, 0.5, 1, 5, 10, 50)),
    labels = c("0.1", "0.5", "1", "5", "10", "50")
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Distance to ocean, miles (log scale)", y = "Count")

ggsave(file.path(paper_dir, "figS2_dist_hist.png"),
       figS2, width = 6, height = 4, dpi = 300, bg = "white")

# figS3: Structure age
figS3 <- ggplot(df_model, aes(x = age)) +
  geom_histogram(bins = 75, fill = HIST_FILL, color = NA, alpha = HIST_ALPHA) +
  scale_x_continuous(breaks = seq(0, 200, 25), limits = c(0, 200)) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Structure age (years)", y = "Count")

ggsave(file.path(paper_dir, "figS3_age_hist.png"),
       figS3, width = 6, height = 4, dpi = 300, bg = "white")

# figS4: Land value (log scale)
figS4 <- ggplot(df_model, aes(x = log10(land_value))) +
  geom_histogram(bins = 60, fill = HIST_FILL, color = NA, alpha = HIST_ALPHA) +
  scale_x_continuous(
    breaks = 3:7,
    labels = c("$1K", "$10K", "$100K", "$1M", "$10M")
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Land value (log scale)", y = "Count")

ggsave(file.path(paper_dir, "figS4_land_hist.png"),
       figS4, width = 6, height = 4, dpi = 300, bg = "white")

# figS5: Improvement value (log scale)
figS5 <- ggplot(df_model, aes(x = log10(improvement_value))) +
  geom_histogram(bins = 60, fill = HIST_FILL, color = NA, alpha = HIST_ALPHA) +
  scale_x_continuous(
    breaks = 3:7,
    labels = c("$1K", "$10K", "$100K", "$1M", "$10M")
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Improvement value (log scale)", y = "Count")

ggsave(file.path(paper_dir, "figS5_improve_hist.png"),
       figS5, width = 6, height = 4, dpi = 300, bg = "white")

# figS6: Seasonal housing share
figS6 <- ggplot(df_model, aes(x = pct_seasonal)) +
  geom_histogram(bins = 40, fill = HIST_FILL, color = NA, alpha = HIST_ALPHA) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Seasonal housing share", y = "Count")

ggsave(file.path(paper_dir, "figS6_seasonal_hist.png"),
       figS6, width = 6, height = 4, dpi = 300, bg = "white")

# figS7: Flood risk category (model sample)
flood_model_counts <- df_model |>
  count(flood_risk) |>
  mutate(flood_risk = factor(flood_risk, levels = c("Low", "Moderate", "High")))

figS7 <- ggplot(flood_model_counts, aes(x = flood_risk, y = n, fill = flood_risk)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(n / sum(n), accuracy = 0.1)),
            vjust = -0.5, size = 3.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = FLOOD_COLORS[c("Low", "Moderate", "High")]) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  guides(fill = "none") +
  labs(x = NULL, y = "Parcel count")

ggsave(file.path(paper_dir, "figS7_flood_bar.png"),
       figS7, width = 6, height = 4, dpi = 300, bg = "white")

# ── Combined panel: all model-variable distributions ─────────────────────────
top_row    <- figS1 | figS4 | figS5
middle_row <- figS2 | figS3 | figS6
bottom_row <- plot_spacer() | figS7 | plot_spacer()

figS_panel <- top_row / middle_row / bottom_row +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 10, face = "bold"))

ggsave(file.path(paper_dir, "figS_vars_panel.png"),
       figS_panel, width = 14, height = 12, dpi = 300, bg = "white")

message("Done — paper figures saved to figures/paper/")
