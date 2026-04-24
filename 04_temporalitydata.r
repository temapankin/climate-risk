library(tidycensus)
library(dplyr)

# Pull ACS B25004 for NJ coastal tracts
acs_seasonal <- get_acs(
  geography = "tract",
  variables = c(
    total_vacant   = "B25004_001",
    seasonal_units = "B25004_006" 
  ),
  state = "NJ",
  county = c("Ocean", "Monmouth", "Atlantic", "Cape May"),
  year = 2022,
  output = "wide"
) |>
  mutate(pct_seasonal_acs = seasonal_unitsE / total_vacantE)

# export to csv


SHORE_COUNTIES <- c("Ocean", "Monmouth", "Atlantic", "Cape May")

# Pull two non-overlapping vintages
acs_early <- get_acs(geography = "tract", variables = c(total = "B25001_001", seasonal = "B25004_006"),
                     state = "NJ", county = SHORE_COUNTIES, year = 2013, survey = "acs5", output = "wide")

acs_late  <- get_acs(geography = "tract", variables = c(total = "B25001_001", seasonal = "B25004_006"),
                     state = "NJ", county = SHORE_COUNTIES, year = 2023, survey = "acs5", output = "wide")

# Compute shares
acs_early <- acs_early |> mutate(pct_seasonal = seasonalE / totalE, period = "early")
acs_late  <- acs_late  |> mutate(pct_seasonal = seasonalE / totalE, period = "late")

write.csv(acs_early, "data/acs_seasonal_early.csv", row.names = FALSE)
write.csv(acs_late, "data/acs_seasonal_late.csv", row.names = FALSE)