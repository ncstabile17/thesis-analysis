library(tidyverse)
library(haven)
library(stringr)
library(lubridate)
library(sf)
library(gridExtra)
library(tidycensus)
library(httr)
library(jsonlite)
library(RColorBrewer)
library(scales)

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
  select(census_tract = TRACT, GEOID, geometry)

# reading in permits data from Jenny Schuetz
schuetz_permits <- read_csv("data/tract_permits-GEOID.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  rename(new_permits = 'Permits, new construction',
         new_units = 'Units permitted')

# reading in HMDA data
all_hmda <- read_csv("data/all_hmda.csv") %>% 
  rename(census_tract = census_tract_number)

all_hmda$census_tract <- str_remove_all(all_hmda$census_tract, "[.]")

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
  select(address_id = ADDRESS_ID, census_tract = CENSUS_TRACT)

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

all_hmda_by_tract <- all_hmda %>%
  group_by(census_tract) %>%
  summarize(
    total_loan_amount_000s = sum(loan_amount_000s),
    num_loans = n()
  )

all_hmda_by_tract <- dc_tracts_2010 %>%
  left_join(all_hmda_by_tract, by = "census_tract")

ggplot(data = all_hmda_by_tract) +
  geom_sf(aes(fill = total_loan_amount_000s)) +
  scale_fill_distiller(palette = "YlGnBu",
                       name = "Total Loans in 000s",
                       labels = dollar_format()) +
  ggtitle(str_wrap("Total mortgage loan amounts highest west of Rock Creek Park from 2007-2017", 70)) +
  labs(caption = str_wrap("Source: 2007-2017 DC Home Mortgage Disclosure Act (HMDA) data available through Consumer Financial Protection Bureau", 100)) +
  theme_void() + 
  theme(
    plot.caption = element_text(hjust = 0)
  )

ggplot(data = all_hmda_by_tract) +
  geom_sf(aes(fill = num_loans)) +
  scale_fill_distiller(palette = "YlGnBu",
                       name = "Total Number of Loans",
                       labels = comma) +
  ggtitle(str_wrap("Mortgage loans concentrated west of Rock Creek Park and center of city from 2007-2017", 70)) +
  labs(caption = str_wrap("Source: 2007-2017 DC Home Mortgage Disclosure Act (HMDA) data available through Consumer Financial Protection Bureau", 100)) +
  theme_void() + 
  theme(
    plot.caption = element_text(hjust = 0)
  )

all_hmda %>% 
  group_by(applicant_race_name_1) %>% 
  summarize(total_loan_amount_000s = sum(loan_amount_000s)) %>% 
  mutate(applicant_race_name_1 = 
           str_replace_all(applicant_race_name_1, 
                           c("Information not provided by applicant in mail, Internet, or telephone application" = "Not Provided"))) %>% 
  ggplot() +
  geom_col(aes(x = reorder(str_wrap(applicant_race_name_1, 15), -total_loan_amount_000s), 
               y = total_loan_amount_000s),
           fill = "#326fa8") +
  scale_y_continuous(labels = dollar_format()) +
  ylab("Total Loan Amount in 000s") +
  xlab("Applicant Race") +
  ggtitle(str_wrap("White mortgage applicants received large share of loan amount from 2007-2017 in DC", 70)) +
  labs(caption = "Source: 2007-2017 DC Home Mortgage Disclosure Act (HMDA) data available through Consumer Financial Protection Bureau") +
  theme_minimal()

all_hmda %>% 
  group_by(year) %>% 
  summarize(total_loan_amount_000s = sum(loan_amount_000s)) %>%
  ggplot(aes(x = factor(year), 
          y = total_loan_amount_000s, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = dollar_format(),
                     name = "Total Loan Amount in 000s") +
  xlab("Year") +
  ggtitle(str_wrap("Total mortgage loan amount varied widely from 2007-2017 in DC", 70)) +
  labs(caption = "Source: 2007-2017 DC Home Mortgage Disclosure Act (HMDA) data available through Consumer Financial Protection Bureau") +
  theme_minimal()
  