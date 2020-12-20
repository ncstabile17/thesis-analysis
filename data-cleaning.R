library(tidyverse)

# I want to import permit data from various years so I can merge them and look at them together

all_permits <- data.frame()

append_permit_data <- function(.year) {
  
  file_name <- str_glue(
    "data/permits/Building_Permits_in_{.year}.csv"
  )
  
  new_permit_data <- 
    read_csv(file_name, col_types = cols(
      PERMIT_CATEGORY_NAME = col_character()
    )) %>% 
    rename_with(tolower) %>%
    filter(permit_subtype_name == "NEW BUILDING") %>% 
    select(-permit_category_name)

  all_permits <- bind_rows(
    all_permits,
    new_permit_data
  )
  
  return(all_permits)
  
}

permit_years <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                  "2017", "2018", "2019", "2020")

all_permits <- map_df(permit_years, .f = ~append_permit_data(.year = .x))

write.csv(all_permits, "data/all_new_building_permits.csv")


# Importing and combining DC HMDA mortgage data

all_hmda <- data.frame()

append_hmda_data <- function(.year) {
  
  file_name <- str_glue(
    "data/hmda/hmda_{.year}_dc_originated-records_labels.csv"
  )
  
  new_hmda_data <- 
    read_csv(file_name) %>% 
    rename(year = as_of_year)
  
  all_hmda <- bind_rows(
    all_hmda,
    new_hmda_data
  )
  
  return(all_hmda)
  
}

hmda_years <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                  "2017")

all_hmda <- map_df(hmda_years, .f = ~append_hmda_data(.year = .x))

# Removing variables with only NA values
all_hmda <- all_hmda[ , colSums(is.na(all_hmda)) < nrow(all_hmda)] 

write.csv(all_hmda, "data/all_hmda.csv")


