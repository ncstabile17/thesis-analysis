library(tidyverse)
library(haven)
library(stringr)
library(lubridate)
library(sf)
library(gridExtra)
library(tidycensus)
library(httr)
library(jsonlite)

# I want to get some Census data so I can make some maps to look at how things have changed 
# and get a sense of some of my key variables

# reading in and setting API key
census_key <- read_file("data/census_api_key.txt")

census_api_key(census_key, install = TRUE, overwrite = TRUE)

make_acs_map <- function(year, census_var, state_fips, county_fips, geometry_flag) {
  
  acs_map_data <- get_acs(
    geography = "tract",
    variables = census_var,
    state = state_fips,
    county = county_fips,
    year = year,
    geometry = geometry_flag
  )
  
  acs_map <- ggplot(data = acs_map_data) +
    geom_sf(aes(fill = estimate)) +
    theme_void()
  
  return(list(acs_map_data, acs_map))
  
}

rent_2018 <- make_acs_map(2018, "B25064_001", 11, 1, FALSE)
rent_2010 <- make_acs_map(2010, "B25064_001", 11, 1, TRUE)

rent_data_2018 <- rent_2018[[1]] %>% 
  rename(
    estimate_2018 = estimate,
    moe_2018 = moe
  ) %>% 
  select(-NAME, -variable)

rent_data_2010 <- rent_2010[[1]] %>% 
  rename(
    estimate_2010 = estimate,
    moe_2010 = moe
  )

rent_data_change <- left_join(rent_data_2010, rent_data_2018, by = "GEOID") %>% 
  mutate(rent_change = (estimate_2018 - estimate_2010)/estimate_2010 * 100)

ggplot(data = rent_data_change) +
  geom_sf(aes(fill = rent_change)) +
  theme_void()

ggplot(data = all_permits) +
  geom_sf(aes(fill = rent_change)) +
  theme_void()