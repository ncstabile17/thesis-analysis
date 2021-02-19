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
# all_new_building_permits <- read_csv("data/all_new_building_permits.csv") %>% 
  mutate(address_id = as.character(maraddressrepositoryid)) 

# reading in DC Census tract shape files
# dc_tracts_2010 <- st_read("data/Census_Tracts_in_2010.shp") %>% 
#  select(census_tract = TRACT, GEOID, geometry)

# reading in permits data from Jenny Schuetz
 # schuetz_permits <- read_csv("data/tract_permits-GEOID.csv") %>% 
 # mutate(GEOID = as.character(GEOID)) %>% 
 # rename(new_permits = 'Permits, new construction',
#         new_units = 'Units permitted')

# reading in HMDA data
all_hmda <- read_csv("data/all_hmda.csv") %>% 
  rename(census_tract = census_tract_number)

all_hmda$census_tract <- str_remove_all(all_hmda$census_tract, "[.]")

# TODO: Figure out which subsidized unit data to use, they might be the same but 
# it also looks like most recent in Housing Insights is from 2011,
# maybe combine with Open Data or just use Open Data, would have to remove doubles if combining

# Reading in subsidized affordable housing data from Housing Insights database
affordable_units_data <- read_csv("data/affordable_units.csv") %>% 
  rename()

census_vars_2010 <- load_variables(2010, "acs5", cache = TRUE)
census_vars_2019 <- load_variables(2019, "acs5", cache = TRUE)

# TODO: Rewrite all this code so I just loop through and combine all the variables given an arbitrary list
# Custom function to get arbitrary ACS data
get_acs_data <- function(.year, .census_var, .state_fips, .county_fips, .geometry_flag = FALSE) {
  
  acs_data <- get_acs(
    geography = "tract",
    variables = .census_var,
    state = .state_fips,
    county = .county_fips,
    year = .year,
    geometry = .geometry_flag
  )
  
  return(acs_data)
}

# Function to rename ACS variables
rename_clean_acs <- function(.acs_data, .var_name, .moe_name) {
  
  cleaned_data <- .acs_data %>% 
    rename(
      {{.var_name}} := estimate,
      {{.moe_name}} := moe
    ) %>% 
    select(-NAME, -variable)
  
  return(cleaned_data)
}

# Getting ACS data on Median Gross Rent for 2010 and 2019 
rent_2019 <- get_acs_data(2019, "B25064_001", 11, 1, TRUE)
rent_2010 <- get_acs_data(2010, "B25064_001", 11, 1, FALSE)

# Getting data on rental units for 2010 and 2019
renter_occupied_2010 <- get_acs_data(2010, "B25003_003", 11, 1, FALSE)
vacant_for_rent_2010 <- get_acs_data(2010, "B25004_002", 11, 1, FALSE)
rented_not_occupied_2010 <- get_acs_data(2010, "B25004_003", 11, 1, FALSE)

renter_occupied_2019 <- get_acs_data(2019, "B25003_003", 11, 1, FALSE)
vacant_for_rent_2019 <- get_acs_data(2019, "B25004_002", 11, 1, FALSE)
rented_not_occupied_2019 <- get_acs_data(2019, "B25004_003", 11, 1, FALSE)

# Other demographic data 
total_pop_2010 <- get_acs_data(2010, "B01003_001", 11, 1, FALSE)
black_pop_2010 <- get_acs_data(2010, "B03002_004", 11, 1, FALSE)
med_income_2010 <- get_acs_data(2010, "B19013_001", 11, 1, FALSE)
med_home_value_2010 <- get_acs_data(2010, "B25077_001", 11, 1, FALSE)

total_pop_2019 <- get_acs_data(2019, "B01003_001", 11, 1, FALSE)
black_pop_2019 <- get_acs_data(2019, "B03002_004", 11, 1, FALSE)
med_income_2019 <- get_acs_data(2019, "B19013_001", 11, 1, FALSE)
med_home_value_2019 <- get_acs_data(2019, "B25077_001", 11, 1, FALSE)

# Cleaning and renaming ACS variables
rent_2010 <- rename_clean_acs(rent_2010, med_rent_2010, med_rent_moe_2010)
rent_2019 <- rename_clean_acs(rent_2019, med_rent_2019, med_rent_moe_2019)

renter_occupied_2010 <- rename_clean_acs(renter_occupied_2010, renter_occupied_2010, renter_occupied_moe_2010)
renter_occupied_2019 <- rename_clean_acs(renter_occupied_2019, renter_occupied_2019, renter_occupied_moe_2019)

vacant_for_rent_2010 <- rename_clean_acs(vacant_for_rent_2010, vacant_for_rent_2010, vacant_for_rent_moe_2010)
vacant_for_rent_2019 <- rename_clean_acs(vacant_for_rent_2019, vacant_for_rent_2019, vacant_for_rent_moe_2019)

rented_not_occupied_2010 <- rename_clean_acs(rented_not_occupied_2010, rented_not_occupied_2010, rented_not_occupied_moe_2010)
rented_not_occupied_2019 <- rename_clean_acs(rented_not_occupied_2019, rented_not_occupied_2019, rented_not_occupied_moe_2019)

total_pop_2010 <- rename_clean_acs(total_pop_2010, total_pop_2010, total_pop_moe_2010)
total_pop_2019 <- rename_clean_acs(total_pop_2019, total_pop_2019, total_pop_moe_2019)

black_pop_2010 <- rename_clean_acs(black_pop_2010, black_pop_2010, black_pop_moe_2010)
black_pop_2019 <- rename_clean_acs(black_pop_2019, black_pop_2019, black_pop_moe_2019)

med_income_2010 <- rename_clean_acs(med_income_2010, med_income_2010, med_income_moe_2010)
med_income_2019 <- rename_clean_acs(med_income_2019, med_income_2019, med_income_moe_2019)

med_home_value_2010 <- rename_clean_acs(med_home_value_2010, med_home_value_2010, med_income_moe_2010)
med_home_value_2019 <- rename_clean_acs(med_home_value_2019, med_home_value_2019, med_income_moe_2019)

# TODO: Get affordable units, rent-controlled (?), crime rate

# Getting rent by income data table
# This table has the population count at different income bands that paid a specific range of rents
# For example, hh's making under $10,000 who paid between $500 and $600 in rent
rent_by_income_data_2010 <- get_acs(
  geography = "tract",
  table = "B25122",
  state = 11,
  county = 1,
  year = 2010
) 

# Pivoting data so each variable estimate is its own column 
# Summing values across low-income bands to get total low income that paid a particular rent
# For example, all hh's making under $50,000 that paid between $400 and $500 in rent
# This will be used to create weighted average rent for low-income hh's
rent_by_income_data_2010_wider <- rent_by_income_data_2010 %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  mutate(low_inc_100 = B25122_004 + B25122_021 + B25122_038 + B25122_055,
         low_inc_150 = B25122_005 + B25122_022 + B25122_039 + B25122_056,
         low_inc_250 = B25122_006 + B25122_023 + B25122_040 + B25122_057,
         low_inc_350 = B25122_007 + B25122_024 + B25122_041 + B25122_058,
         low_inc_450 = B25122_008 + B25122_025 + B25122_042 + B25122_059,
         low_inc_550 = B25122_009 + B25122_026 + B25122_043 + B25122_060,
         low_inc_650 = B25122_010 + B25122_027 + B25122_044 + B25122_061,
         low_inc_750 = B25122_011 + B25122_028 + B25122_045 + B25122_062,
         low_inc_850 = B25122_012 + B25122_029 + B25122_046 + B25122_063,
         low_inc_950 = B25122_013 + B25122_030 + B25122_047 + B25122_064,
         low_inc_1125 = B25122_014 + B25122_031 + B25122_048 + B25122_065,
         low_inc_1375 = B25122_015 + B25122_032 + B25122_049 + B25122_066,
         low_inc_1750 = B25122_016 + B25122_033 + B25122_050 + B25122_067,
         low_inc_2250 = B25122_017 + B25122_034 + B25122_051 + B25122_068,
         total_low_inc_2010 = low_inc_100 + low_inc_150 + low_inc_250 + low_inc_350 +
           low_inc_450 + low_inc_550 + low_inc_650 + low_inc_750 + low_inc_850 + 
           low_inc_950 + low_inc_1125 + low_inc_1375 + low_inc_1750 + low_inc_2250)

# Calculating weighted average low income rent
avg_low_inc_rent_2010 <-  rent_by_income_data_2010_wider %>% 
  mutate(avg_low_inc_rent_2010 = 
           (low_inc_100*100 + low_inc_150*150 + low_inc_250*250 + low_inc_350*350
            + low_inc_450*450 + low_inc_550*550 + low_inc_650*650 + low_inc_750*750
            + low_inc_850*850 + low_inc_950*950 + low_inc_1125*1125 + low_inc_1375*1375
            + low_inc_1750*1750 + low_inc_2250*2250)/total_low_inc_2010) %>% 
  select(GEOID, avg_low_inc_rent_2010, total_low_inc_2010)

# Getting rent by income data table
# This table has the population count at different income bands that paid a specific range of rents
# For example, hh's making under $10,000 who paid between $500 and $600 in rent
rent_by_income_data_2019 <- get_acs(
  geography = "tract",
  table = "B25122",
  state = 11,
  county = 1,
  year = 2019
) 

# Pivoting data so each variable estimate is its own column 
# Summing values across low-income bands to get total low income that paid a particular rent
# For example, all hh's making under $50,000 that paid between $400 and $500 in rent
# This will be used to create weighted average rent for low-income hh's
rent_by_income_data_2019_wider <- rent_by_income_data_2019 %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  mutate(low_inc_100 = B25122_004 + B25122_021 + B25122_038 + B25122_055,
         low_inc_150 = B25122_005 + B25122_022 + B25122_039 + B25122_056,
         low_inc_250 = B25122_006 + B25122_023 + B25122_040 + B25122_057,
         low_inc_350 = B25122_007 + B25122_024 + B25122_041 + B25122_058,
         low_inc_450 = B25122_008 + B25122_025 + B25122_042 + B25122_059,
         low_inc_550 = B25122_009 + B25122_026 + B25122_043 + B25122_060,
         low_inc_650 = B25122_010 + B25122_027 + B25122_044 + B25122_061,
         low_inc_750 = B25122_011 + B25122_028 + B25122_045 + B25122_062,
         low_inc_850 = B25122_012 + B25122_029 + B25122_046 + B25122_063,
         low_inc_950 = B25122_013 + B25122_030 + B25122_047 + B25122_064,
         low_inc_1125 = B25122_014 + B25122_031 + B25122_048 + B25122_065,
         low_inc_1375 = B25122_015 + B25122_032 + B25122_049 + B25122_066,
         low_inc_1750 = B25122_016 + B25122_033 + B25122_050 + B25122_067,
         low_inc_2250 = B25122_017 + B25122_034 + B25122_051 + B25122_068,
         total_low_inc_2019 = low_inc_100 + low_inc_150 + low_inc_250 + low_inc_350 +
           low_inc_450 + low_inc_550 + low_inc_650 + low_inc_750 + low_inc_850 + 
           low_inc_950 + low_inc_1125 + low_inc_1375 + low_inc_1750 + low_inc_2250)

# Calculating weighted average low income rent
avg_low_inc_rent_2019 <-  rent_by_income_data_2019_wider %>% 
  mutate(avg_low_inc_rent_2019 = 
           (low_inc_100*100 + low_inc_150*150 + low_inc_250*250 + low_inc_350*350
            + low_inc_450*450 + low_inc_550*550 + low_inc_650*650 + low_inc_750*750
            + low_inc_850*850 + low_inc_950*950 + low_inc_1125*1125 + low_inc_1375*1375
            + low_inc_1750*1750 + low_inc_2250*2250)/total_low_inc_2019) %>% 
  select(GEOID, avg_low_inc_rent_2019, total_low_inc_2019)

# Getting total low-income renters that are housing cost-burdened (rent greater than 30% of income)
low_inc_cost_burden_data_2010 <- get_acs(
  geography = "tract",
  table = "B25106",
  state = 11,
  county = 1,
  year = 2010
) 

low_inc_cost_burden_data_2010 <- low_inc_cost_burden_data_2010 %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  mutate(low_inc_cost_burden_2010 = B25106_028 + B25106_032 + B25106_036,
         total_low_inc_2_2010 = B25106_025 + B25106_029 + B25106_033,
         per_low_inc_cost_burden_2010 = low_inc_cost_burden_2010/total_low_inc_2_2010) %>% 
  select(GEOID, low_inc_cost_burden_2010, total_low_inc_2_2010, per_low_inc_cost_burden_2010)

# Getting total low-income renters that are housing cost-burdened (rent greater than 30% of income)
low_inc_cost_burden_data_2019 <- get_acs(
  geography = "tract",
  table = "B25106",
  state = 11,
  county = 1,
  year = 2019
) 

low_inc_cost_burden_data_2019 <- low_inc_cost_burden_data_2019 %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  mutate(low_inc_cost_burden_2019 = B25106_028 + B25106_032 + B25106_036,
         total_low_inc_2_2019 = B25106_025 + B25106_029 + B25106_033,
         per_low_inc_cost_burden_2019 = low_inc_cost_burden_2019/total_low_inc_2_2019) %>% 
  select(GEOID, low_inc_cost_burden_2019, total_low_inc_2_2019, per_low_inc_cost_burden_2019)

# Getting rent-controlled units by combining variables for Tenure By Year Structure Built By Units In Structure
# Only getting renter-occupied pre-1979 buildings with 5+ units; not exactly rent control, but close
rent_control_data_2010 <- get_acs(
  geography = "tract",
  table = "B25127",
  state = 11,
  county = 1,
  year = 2010
) 

rent_control_2010 <- rent_control_data_2010 %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  mutate(rent_control_2010 = B25127_049 + B25127_050 + B25127_051) %>% 
  select(GEOID, rent_control_2010)

# Getting rent-controlled units by combining variables for Tenure By Year Structure Built By Units In Structure
# Only getting renter-occupied pre-1979 buildings with 5+ units; not exactly rent control, but close
rent_control_data_2019 <- get_acs(
  geography = "tract",
  table = "B25127",
  state = 11,
  county = 1,
  year = 2019
) 

rent_control_2019 <- rent_control_data_2019 %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  mutate(rent_control_2019 = B25127_049 + B25127_050 + B25127_051) %>% 
  select(GEOID, rent_control_2019)

# Combining variables and calculating rent change and percent rent change by Census tract
combined_rent_data <- list(rent_2019, rent_2010, 
               renter_occupied_2010, renter_occupied_2019, 
               vacant_for_rent_2010, vacant_for_rent_2019,
               rented_not_occupied_2010, rented_not_occupied_2019, 
               total_pop_2010, total_pop_2019, 
               black_pop_2010, black_pop_2019, 
               avg_low_inc_rent_2010, avg_low_inc_rent_2019,
               med_income_2010, med_income_2019,
               med_home_value_2010, med_home_value_2019,
               low_inc_cost_burden_data_2010, low_inc_cost_burden_data_2019,
               rent_control_2010, rent_control_2019) %>% 
  reduce(left_join, by = "GEOID") %>% 
  mutate(med_rent_per_change = (med_rent_2019 - med_rent_2010)/med_rent_2010 * 100,
         med_rent_change = med_rent_2019 - med_rent_2010,
         all_rental_units_2010 = renter_occupied_2010 + rented_not_occupied_2010 + vacant_for_rent_2010,
         all_rental_units_2019 = renter_occupied_2019 + rented_not_occupied_2019 + vacant_for_rent_2019,
         rental_unit_change = all_rental_units_2019 - all_rental_units_2010,
         per_black_pop_2010 = black_pop_2010/total_pop_2010,
         per_black_pop_2019 = black_pop_2019/total_pop_2019,
         black_pop_change = black_pop_2019 - black_pop_2010,
         low_inc_rent_change = avg_low_inc_rent_2019 - avg_low_inc_rent_2010,
         low_inc_pop_change = total_low_inc_2019 - total_low_inc_2010)

# Mapping percent change in median rent
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

# Mapping median rent change with limit on rent change (to reduce outlier skewing color scheme)
combined_rent_data %>% 
  mutate(
    med_rent_change = if_else(med_rent_change > 1200, 1200, med_rent_change),
    med_rent_change = if_else(med_rent_change < -100, -100, med_rent_change)) %>% 
  ggplot() +
  geom_sf(aes(fill = med_rent_change)) +
  theme_void() +
  scale_fill_distiller(name = "Rent Change 2010-2019",
                       palette = "YlGnBu") +
  ggtitle("Largest increases in median rent concentrated in few Census tracts") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Mapping median rent change without limit on rent change
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

# Distribution of change in median rent by dollars 
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

# Distribution of change in median rent for tracts with decreasing black population 
combined_rent_data %>% 
  filter(black_pop_change < 0) %>% 
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

# Distribution of change in median rent for tracts with increasing black population 
combined_rent_data %>% 
  filter(black_pop_change > 0) %>% 
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

# Mapping avg low-inc rent change without limit on rent change
ggplot(data = combined_rent_data) +
  geom_sf(aes(fill = low_inc_rent_change)) +
  theme_void() +
  scale_fill_distiller(name = "Low-inc Rent Change 2010-2019",
                       palette = "YlGnBu") +
  ggtitle("Placeholder") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Distribution of change in avg low-inc rent by dollars 
combined_rent_data %>% 
  ggplot() +
  geom_histogram(
    aes(x = low_inc_rent_change),
    bins = 100,
    fill = "blue") +
  scale_x_continuous(labels = dollar_format()) + 
  xlab('Tract change in avg low-inc rent') +
  ylab('Tract count') +
  ggtitle(label = '', subtitle = '') +
  theme_minimal()

summary(combined_rent_data$low_inc_rent_change)

# Mapping unit change with limit to remove outliers
combined_rent_data %>% 
  mutate(
    rental_unit_change = if_else(rental_unit_change > 700, 700, rental_unit_change),
    rental_unit_change = if_else(rental_unit_change < -300, -300, rental_unit_change)) %>% 
  ggplot() +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(name = "Unit Change 2010-2019",
                       palette = "YlGnBu") +
  ggtitle("Placeholder") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Mapping unit change without outlier limit
combined_rent_data %>% 
  ggplot() +
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

# Mapping change in Black population 
combined_rent_data %>% 
  ggplot() +
  geom_sf(aes(fill = black_pop_change)) +
  theme_void() +
  scale_fill_distiller(name = "Change in Black population 2010-2019",
                       palette = "YlGnBu") +
  ggtitle("Placeholder") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Mapping change in Black population, only show increase
combined_rent_data %>% 
  ggplot() +
  geom_sf(aes(fill = black_pop_change)) +
  theme_void() +
  scale_fill_distiller(name = "Change in Black population 2010-2019",
                       palette = "YlGnBu",
                       limits = c(0, 2000)) +
  ggtitle("Placeholder") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Mapping change in Black population, only show decrease
combined_rent_data %>% 
  ggplot() +
  geom_sf(aes(fill = black_pop_change)) +
  theme_void() +
  scale_fill_distiller(name = "Change in Black population 2010-2019",
                       palette = "YlGnBu",
                       limits = c(-1300, 0)) +
  ggtitle("Placeholder") +
  labs(caption = "Source: American Community Survey 5-year estimates 2006-2010 and 2015-2019.") +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Distribution of change in Black population
combined_rent_data %>% 
  ggplot() +
  geom_histogram(
    aes(x = black_pop_change),
    bins = 100,
    fill = "blue") +
  scale_x_continuous(labels = comma_format()) + 
  xlab('Change in Black population') +
  ylab('Tract count') +
  ggtitle(label = '', subtitle = '') +
  theme_minimal()

summary(combined_rent_data$black_pop_change)

# TODO: Make scatter plots to examine relationships of all these variables
# TODO: Where did housing units get added? What did those look like in 2010 (in terms of race and income)
# TODO: Where did rent increase (the most)? What did those look like in 2010 (in terms of race and income)

# Change in rent by median income in 2010
combined_rent_data %>% 
  mutate(maj_black_2010 = if_else(per_black_pop_2010 > .50, 1, 0)) %>% 
  ggplot(aes(x = med_income_2010, 
             y = med_rent_change)) + 
  geom_point(aes(color = as.character(maj_black_2010))) +
  geom_smooth(method = "lm", se=FALSE) +
  theme_minimal()

# Change in units by median income in 2010
combined_rent_data %>% 
  mutate(maj_black_2010 = if_else(per_black_pop_2010 > .50, 1, 0)) %>% 
  ggplot(aes(x = med_income_2010, 
             y = rental_unit_change)) + 
  geom_point(aes(color = as.character(maj_black_2010))) +
  geom_smooth(method = "lm", se=FALSE) +
  theme_minimal()

# Change in rent by change in units
combined_rent_data %>% 
  mutate(maj_black_2010 = if_else(per_black_pop_2010 > .50, 1, 0)) %>% 
  ggplot(aes(x = med_rent_change, 
             y = rental_unit_change)) + 
  geom_point(aes(color = as.character(maj_black_2010))) +
  geom_smooth(method = "lm", se=FALSE) +
  theme_minimal()

# Change in rent by change in black population
combined_rent_data %>% 
  ggplot(aes(x = med_rent_change, 
             y = black_pop_change)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  theme_minimal()

# Change in rent by change in low-income population
combined_rent_data %>% 
  ggplot(aes(x = med_rent_change, 
             y = low_inc_pop_change)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  theme_minimal()

# Change in rental units by change in low-income population
combined_rent_data %>% 
  ggplot(aes(x = rental_unit_change, 
             y = low_inc_pop_change)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  theme_minimal()

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
  