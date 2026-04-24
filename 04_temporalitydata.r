library(tidycensus)
library(dplyr)

SHORE_COUNTIES <- c("Ocean", "Monmouth", "Atlantic", "Cape May")

ACS_VARS <- c(total = "B25001_001", seasonal = "B25004_006")

# 2013 ACS → covers MOD-IV sales 2005–2013
acs_2013 <- get_acs(
  geography = "tract", variables = ACS_VARS,
  state = "NJ", county = SHORE_COUNTIES,
  year = 2013, survey = "acs5", output = "wide"
) |>
  mutate(pct_seasonal = seasonalE / totalE) |>
  select(GEOID, NAME, pct_seasonal)

# 2023 ACS → covers MOD-IV sales 2014–2024
acs_2023 <- get_acs(
  geography = "tract", variables = ACS_VARS,
  state = "NJ", county = SHORE_COUNTIES,
  year = 2023, survey = "acs5", output = "wide"
) |>
  mutate(pct_seasonal = seasonalE / totalE) |>
  select(GEOID, NAME, pct_seasonal)

write.csv(acs_2013, "data/acs_seasonal_2013.csv", row.names = FALSE)
write.csv(acs_2023, "data/acs_seasonal_2023.csv", row.names = FALSE)
