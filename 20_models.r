setwd("/Volumes/SSD/climate-risk/")

library(tidyverse)
library(arrow)
library(fixest)

# ── Load data ──────────────────────────────────────────────────────────────────
df <- read_parquet("data/parcels_shore_clean.parquet") |>
  filter(
    sale_price_2020 > 0,
    dist_to_ocean_mi > 0,
    !is.na(pct_seasonal),
    !is.na(census_tract_geoid)
  ) |>
  mutate(
    log_price    = log(sale_price_2020),
    log_dist     = log(dist_to_ocean_mi),
    year_fe      = as.factor(mod_iv_year),
    tract_fe     = as.factor(census_tract_geoid)
  )

cat(sprintf("Observations: %s\n", format(nrow(df), big.mark = ",")))

# ── OLS: year FE + census-tract FE ────────────────────────────────────────────
# NOTE: pct_seasonal is a census-tract-level variable (from ACS).
# Including tract FEs absorbs all between-tract variation, making pct_seasonal
# collinear with the tract dummies — fixest will drop it and warn.
# Model (1) keeps tract FEs without pct_seasonal (cleanly identified);
# Model (2) replaces tract FEs with county FEs so pct_seasonal can enter.

# Model 1: log(price) ~ log(dist_to_shore) | tract + year
m1 <- feols(
  log_price ~ log_dist | tract_fe + year_fe,
  data    = df,
  cluster = ~tract_fe   # SEs clustered at census-tract level
)

# Model 2: log(price) ~ pct_seasonal + log(dist_to_shore) | county + year
# (tract FEs replaced by county FEs so the tract-level ACS var is identified)
df <- df |> mutate(county_fe = as.factor(substr(census_tract_geoid, 1, 5)))

m2 <- feols(
  log_price ~ pct_seasonal + log_dist | county_fe + year_fe,
  data    = df,
  cluster = ~tract_fe
)

# ── Results ────────────────────────────────────────────────────────────────────
etable(m1, m2,
  headers  = c("Tract + Year FE", "County + Year FE"),
  digits   = 4,
  se.below = TRUE
)
