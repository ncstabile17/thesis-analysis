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

make_acs_map <- function(year, census_var, state_fips, county_fips) {
  
  acs_map_data <- get_acs(
    geography = "tract",
    variables = census_var,
    state = state_fips,
    county = county_fips,
    year = year,
    geometry = TRUE
  )
  
  acs_map <- ggplot(data = acs_map_data) +
    geom_sf(aes(fill = estimate)) +
    theme_void()
  
  return(list(acs_map_data, acs_map))
  
}

rent_2018 <- make_acs_map(2018, "B25064_001", 11, 1)
rent_2010 <- make_acs_map(2010, "B25064_001", 11, 1)

rent_data_2018 <- rent_2018[[1]]
