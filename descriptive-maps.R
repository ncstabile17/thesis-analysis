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
# census_key <- read_file("data/census_api_key.txt")

# census_api_key(census_key, install = TRUE, overwrite = TRUE)

# reading in permits data
all_new_building_permits <- read_csv("data/all_new_building_permits.csv") %>% 
  mutate(address_id = as.character(maraddressrepositoryid)) 

# reading in DC Census tract shape files
dc_tracts_2010 <- st_read("data/Census_Tracts_in_2010.shp") %>% 
  select(TRACT, GEOID, geometry)

# reading in permits data from Jenny Schuetz
schuetz_permits <- read_csv("data/tract_permits-GEOID.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  rename(new_permits = 'Permits, new construction',
         new_units = 'Units permitted')

# Custom function to get arbitrary ACS data
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

# Getting ACS data on Median Gross Rent for 2010 and 2018 
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

# Calculating and mapping change in rent by Census tract
rent_data_change <- left_join(rent_data_2010, rent_data_2018, by = "GEOID") %>% 
  mutate(rent_change = (estimate_2018 - estimate_2010)/estimate_2010 * 100)

ggplot(data = rent_data_change) +
  geom_sf(aes(fill = rent_change)) +
  theme_void() +
  scale_fill_continuous(
    low = "white", high = "blue", name = "% Rent Change 2010-2018"
  )


# now working with Jenny Schuetz's permit data

# adding permit data to rent data
rent_data_change <- left_join(
  rent_data_change,
  schuetz_permits, 
)

ggplot(data = rent_data_change) +
  geom_sf(aes(fill = new_units)) +
  theme_void() + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "New Units 2008-2016", label = scales::comma
  )

# now working with the permits data from DC Open Data

all_new_building_permits <- st_as_sf(
  all_new_building_permits,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE)

# Permit data doesn't have tract or geoID information, adding additional data source
dc_addresses <- st_read("data/Address_Points.csv") %>% 
  select(address_id = ADDRESS_ID, TRACT = CENSUS_TRACT)

all_new_building_permits <- 
  left_join(all_new_building_permits, dc_addresses, by = "address_id")

all_new_permits_merged <- st_join(
  all_new_building_permits, # points
  dc_tracts_2010, # polygons
  join = st_within
)

all_new_permits_merged <- st_set_geometry(all_new_permits_merged, NULL)

all_new_permits_by_tract <- all_new_permits_merged %>%
  group_by(GEOID) %>%
  summarize(
    permit_count = n(),
  )

all_new_permits_by_tract <- dc_tracts_2010 %>%
  left_join(all_new_permits_by_tract, by = "GEOID")

# TODO: something looks wrong here, but maybe not?
ggplot(data = all_new_permits_by_tract) +
  geom_sf(aes(fill = permit_count)) +
  theme_void()

# Working with HMDA data


