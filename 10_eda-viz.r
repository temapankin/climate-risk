setwd("/Volumes/SSD/climate-risk/")

library(tidyverse)
library(sf)
library(tigris)
library(arrow)
library(scales)
library(patchwork)

options(tigris_use_cache = TRUE)
dir.create("figures", showWarnings = FALSE)

SHORE_FIPS <- c("001", "009", "025", "029")

nj_counties <- counties("NJ", cb = TRUE, year = 2023) |> st_transform(4326)
shore_co    <- nj_counties |> filter(COUNTYFP %in% SHORE_FIPS)
cousub      <- county_subdivisions("NJ", year = 2023) |>
                 st_transform(4326) |> filter(COUNTYFP %in% SHORE_FIPS)
tracts_sf   <- tracts("NJ", year = 2023) |>
                 st_transform(4326) |> filter(COUNTYFP %in% SHORE_FIPS)

parcels <- open_dataset("data/parcels_shore_clean.parquet") |>
  filter(mod_iv_year == 2023) |>
  select(mod_iv_munis_name, sale_price, flood_risk,
         dist_to_ocean_mi, pct_seasonal, census_tract_geoid) |>
  collect() |>
  mutate(across(c(sale_price, dist_to_ocean_mi, pct_seasonal), as.numeric))

# ── Figure 1: Study area — shore parcels coloured by county ─────────────────

cafra_mask <- arrow::read_parquet("data/shore_mask.parquet") |>
  mutate(geometry = sf::st_as_sfc(geometry, crs = 3424)) |>
  sf::st_as_sf(crs = 3424) |>
  sf::st_transform(4326) |>
  mutate(layer = "Study area (CAFRA)")

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
  labs(title = "Jersey Shore Parcels by County (2023)", x = NULL, y = NULL) +
  theme_void() +
  theme(
    plot.title          = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.title.position = "plot",
    legend.position     = "right"
  )

ggsave("figures/fig1_parcels_county.png", fig1, width = 7, height = 10, dpi = 300, bg = "white")

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

fig2a <- map_base +
  inset_element(bar_inset, left = 0.0, bottom = 0.62, right = 0.65, top = 0.98)

ggsave("figures/fig2a_sales_map.png", fig2a, width = 6, height = 9, dpi = 300, bg = "white")

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
