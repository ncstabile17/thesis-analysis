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
make_acs_map <- function(.year, .census_var, .state_fips, .county_fips, .geometry_flag) {
  
  acs_map_data <- get_acs(
    geography = "tract",
    variables = .census_var,
    state = .state_fips,
    county = .county_fips,
    year = .year,
    geometry = .geometry_flag
  )
  
  acs_map <- ggplot(data = acs_map_data) +
    geom_sf(aes(fill = estimate)) +
    theme_void()
  
  return(list(acs_map_data, acs_map))
  
}

# Function to rename ACS variables
rename_clean_acs <- function(.acs_data, .var_name, .moe_name) {
  
  cleaned_data <- .acs_data[[1]] %>% 
    rename(
      {{.var_name}} := estimate,
      {{.moe_name}} := moe
    ) %>% 
    select(-NAME, -variable)
  
  return(cleaned_data)
  
}

# Getting ACS data on Median Gross Rent for 2010 and 2019 
rent_2019 <- make_acs_map(2019, "B25064_001", 11, 1, FALSE)
rent_2010 <- make_acs_map(2010, "B25064_001", 11, 1, TRUE)

# Getting data on rental units for 2010 and 2019
renter_occupied_2010 <- make_acs_map(2010, "B25003_003", 11, 1, FALSE)
vacant_for_rent_2010 <- make_acs_map(2010, "B25004_002", 11, 1, FALSE)
rented_not_occupied_2010 <- make_acs_map(2010, "B25004_003", 11, 1, FALSE)

renter_occupied_2019 <- make_acs_map(2019, "B25003_003", 11, 1, FALSE)
vacant_for_rent_2019 <- make_acs_map(2019, "B25004_002", 11, 1, FALSE)
rented_not_occupied_2019 <- make_acs_map(2019, "B25004_003", 11, 1, FALSE)

# Cleaning and renaming ACS variables
rent_2010 <- rename_clean_acs(rent_2010, med_rent_2010, med_rent_moe_2010)
rent_2019 <- rename_clean_acs(rent_2019, med_rent_2019, med_rent_moe_2019)

renter_occupied_2010 <- rename_clean_acs(renter_occupied_2010, renter_occupied_2010, renter_occupied_moe_2010)
renter_occupied_2019 <- rename_clean_acs(renter_occupied_2019, renter_occupied_2019, renter_occupied_moe_2019)

vacant_for_rent_2010 <- rename_clean_acs(vacant_for_rent_2010, vacant_for_rent_2010, vacant_for_rent_moe_2010)
vacant_for_rent_2019 <- rename_clean_acs(vacant_for_rent_2019, vacant_for_rent_2019, vacant_for_rent_moe_2019)

rented_not_occupied_2010 <- rename_clean_acs(rented_not_occupied_2010, rented_not_occupied_2010, rented_not_occupied_moe_2010)
rented_not_occupied_2019 <- rename_clean_acs(rented_not_occupied_2019, rented_not_occupied_2019, rented_not_occupied_moe_2019)


# Combining variables and calculating rent change and percent rent change by Census tract
# TODO: sum total rental units and calculate change in rental units
combined_rent_data <- list(rent_2010, rent_2019, 
               renter_occupied_2010, renter_occupied_2019, 
               vacant_for_rent_2010, vacant_for_rent_2019,
               rented_not_occupied_2010, rented_not_occupied_2019) %>% 
  reduce(left_join, by = "GEOID") %>% 
  mutate(med_rent_per_change = (med_rent_2019 - med_rent_2010)/med_rent_2010 * 100,
         rent_change = med_rent_2019 - med_rent_2010,
         all_rental_units_2010 = renter_occupied_2010 + rented_not_occupied_2010 + vacant_for_rent_2010,
         all_rental_units_2019 = renter_occupied_2019 + rented_not_occupied_2019 + vacant_for_rent_2019,
         rental_unit_change = all_rental_units_2019 - all_rental_units_2010)

# Mapping percent change in rent
ggplot(data = combined_rent_data) +
  geom_sf(aes(fill = med_rent_per_change)) +
  theme_void() +
  scale_fill_distiller(name = "% Rent Change 2010-2019",
                       palette = "YlGnBu") +
  ggtitle("Largest increases in median rent concentrated in few Census tracts") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2014-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Mapping rent change with limit on rent change (to reduce outlier skewing color scheme)
combined_rent_data %>% 
  mutate(
    med_rent_change = if_else(med_rent_change > 1200, 1200, med_rent_change)) %>% 
  ggplot() +
  geom_sf(aes(fill = med_rent_change)) +
  theme_void() +
  scale_fill_distiller(name = "Rent Change 2010-2019",
                       palette = "YlGnBu",
                       limits = c(-150, 1200)) +
  ggtitle("Largest increases in median rent concentrated in few Census tracts") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Mapping rent change without limit on rent change
ggplot(data = combined_rent_data) +
  geom_sf(aes(fill = med_rent_change)) +
  theme_void() +
  scale_fill_distiller(name = "Rent Change 2010-2019",
                       palette = "YlGnBu") +
  ggtitle("Largest increases in median rent concentrated in few Census tracts") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Distribution of change in rent by dollars 
combined_rent_data %>% 
  ggplot() +
  geom_histogram(
    aes(x = med_rent_change),
    bins = 100,
    fill = "blue") +
  scale_x_continuous(labels = dollar_format()) + 
  xlab('Tract change in median rent') +
  ylab('Tract count') +
  ggtitle(label = '', subtitle = '') +
  theme_minimal()

summary(combined_rent_data$med_rent_change)

# Mapping unit change 
ggplot(data = combined_rent_data) +
  geom_sf(aes(fill = rental_unit_change)) +
  theme_void() +
  scale_fill_distiller(name = "Unit Change 2010-2019",
                       palette = "YlGnBu") +
  ggtitle("Placeholder") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Distribution of unit change  
combined_rent_data %>% 
  ggplot() +
  geom_histogram(
    aes(x = rental_unit_change),
    bins = 100,
    fill = "blue") +
  scale_x_continuous(labels = comma_format()) + 
  xlab('Tract change in units') +
  ylab('Tract count') +
  ggtitle(label = '', subtitle = '') +
  theme_minimal()

summary(combined_rent_data$rental_unit_change)

# now working with Jenny Schuetz's permit data

# adding permit data to rent data
rent_data_change <- left_join(
  rent_data_change,
  schuetz_permits, 
)

ggplot(data = rent_data_change) +
  geom_sf(aes(fill = new_units)) +
  theme_void() + 
  scale_fill_distiller(name = "New Units 2008-2016",
                       palette = "YlGnBu",
                       labels = comma) +
  ggtitle("New housing units highly concentrated in few Census tracts") +
  labs(caption = str_wrap("Source: Data compiled by and used with permission from Jenny Schuetz of the Brookings Institution. For data collection details, see Schuetz, J. (2019). Teardowns, popups, and renovations: How does housing supply change? Journal of Regional Science, 60(3), 459-480. doi:10.1111/jors.12470.", 100)) +
  theme(
    plot.caption = element_text(hjust = 0)
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
# ggplot(data = all_new_permits_by_tract) +
#   geom_sf(aes(fill = permit_count)) +
#   theme_void()

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
  