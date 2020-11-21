library(tidyverse)

# I want to import permit data from various years so I can merge them and look at them together

all_permits <- data.frame()

append_permit_data <- function(.year) {
  
  file_name <- str_glue(
    "data/Building_Permits_in_{.year}.csv"
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

