# R Script for pulling and examining living standards data
# Author: Henry DeMarco
# Date Created: June 17, 2024
# Last Updated: July 9, 2024
# Progress Tracking Spreadsheet Updated: July 9, 2024 (!)

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

#(!) indicates temporary comments to remove

#Packages
library(tidyverse)
library(tidycensus)

# Census API Key
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

# Creating basic objects

# Year for all data pull
year <- 2022

# Get labels
acs5 <- load_variables(2022, "acs5", cache = TRUE)

# County FIPS codes and name
county_code <- c("003") #Albemarle FIPS Code

# A custom R function that creates a table of all variable codes and metadata
all_acs_meta <- function(){
  # Gets the list of all variables from all acs5 metadata tables
  vars1 <- load_variables(year, "acs5", cache = TRUE) %>% select(-geography)
  vars2 <- load_variables(year, "acs5/subject", cache = TRUE)
  vars3 <- load_variables(year, "acs5/profile", cache = TRUE)
  
  # Provides column with specific lookup
  vars1$dataset_table <- "acs5"
  vars2$dataset_table <- "acs5/subject"
  vars3$dataset_table <- "acs5/profile"
  
  # Combine all table rows
  all_vars_meta <- rbind(vars1, vars2, vars3)
  
  return(all_vars_meta)
}

# Creates a table of all the metadata called "meta_table"
meta_table <- all_acs_meta()

# Opens the newly made table
View(meta_table)

############################

# Table: B19051 (Earnings in the Past 12 Months for Households)

# Charlottesville (Rent Burden), 2022

cville_tract_housing_cost_2022 <- get_acs(geography = "tract", 
                              table = "B25070", 
                              state = "VA", 
                              county = "540", 
                              survey = "acs5", 
                              year = 2022, 
                              cache_table = TRUE)

cville_tract_housing_total_2022 <- get_acs(geography = "tract", 
                                           variable = "B19051_001", 
                                           state = "VA", 
                                           county = "540", 
                                           survey = "acs5", 
                                           year = 2022, 
                                           cache_table = TRUE) %>%
  select(total_households = estimate, geoid = GEOID) 

## Tract level stats: counts
cville_housing_costs_counts_2022 <- 
  cville_tract_housing_cost_2022 %>%
  left_join(acs5 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
    ),
    "_percent",
    "")
  ) %>%
  
  mutate(denom = total - not_computed,
         Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
         `Severely Burdened` =  `50.0_or_more`,
         `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
         
  ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`, total_renting = total) %>%
  left_join(cville_tract_housing_total_2022) %>%
  mutate(Renting = total_renting/total_households) %>%
  mutate(county_type = "Census Tracts")

## Tract level stats: percentages
cville_housing_costs_percentages_2022 <- 
  cville_tract_housing_cost_2022 %>%
  left_join(acs5 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
    ),
    "_percent",
    "")
  ) %>%
  
  mutate(denom = total - not_computed,
         Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
         `Severely Burdened` =  `50.0_or_more`,
         `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
         
  ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`, total_renting = total) %>%
  left_join(cville_tract_housing_total_2022) %>%
  mutate(across(c(Burdened, `Severely Burdened`, `Not Burdened`), ~.x/denom), Renting = total_renting/total_households) %>%
  mutate(county_type = "Census Tracts")


# Albemarle (Rent Burden), 2022

alb_tract_housing_total_2022 <- get_acs(geography = "tract", 
                                           variable = "B19051_001", 
                                           state = "VA", 
                                           county = "003", 
                                           survey = "acs5", 
                                           year = 2022, 
                                           cache_table = TRUE) %>%
  select(total_households = estimate, geoid = GEOID) 

alb_tract_housing_cost_2022 <- get_acs(geography = "tract", 
                                          table = "B25070", 
                                          state = "VA", 
                                          county = "003", 
                                          survey = "acs5", 
                                          year = 2022, 
                                          cache_table = TRUE)

## Tract level stats: counts
alb_housing_costs_counts_2022 <- 
  alb_tract_housing_cost_2022 %>%
  left_join(acs5 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
    ),
    "_percent",
    "")
  ) %>%
  
  mutate(denom = total - not_computed,
         Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
         `Severely Burdened` =  `50.0_or_more`,
         `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
         
  ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`, total_renting = total) %>%
  left_join(alb_tract_housing_total_2022) %>%
  mutate(Renting = total_renting/total_households) %>%
  mutate(county_type = "Census Tracts")

## Tract level stats: percentages
alb_housing_costs_percentages_2022 <- 
  alb_tract_housing_cost_2022 %>%
  left_join(acs5 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
    ),
    "_percent",
    "")
  ) %>%
  
  mutate(denom = total - not_computed,
         Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
         `Severely Burdened` =  `50.0_or_more`,
         `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
         
  ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`, total_renting = total) %>%
  left_join(alb_tract_housing_total_2022) %>%
  mutate(across(c(Burdened, `Severely Burdened`, `Not Burdened`), ~.x/denom), Renting = total_renting/total_households) %>%
  mutate(county_type = "Census Tracts")

# Table: B20002 (Median Earnings in the Past 12 Months, Inflation-Adjusted)

# Charlottesville (Median Earnings), 2022

median_earnings_cville_2022 <- get_acs(
  geography = "tract",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B20002",
  survey = "acs5",
  year = 2022)

# Finalizing Table:

cville_median_earnings_2022 <- median_earnings_cville_2022 %>% 
  mutate(year = "2022",
         label = case_when(
           variable == "B20002_001" ~ "Median Earnings in Past 12 Months (Inflation Adjusted): Total",
           variable == "B20002_002" ~ "Median Earnings in Past 12 Months (Inflation Adjusted): Male",
           variable == "B20002_003" ~ "Median Earnings in Past 12 Months (Inflation Adjusted): Female",
         ))

# Generating CSV:

write.csv(cville_median_earnings_2022, "temp_data/cville_median_earnings_2022.csv")

# Albemarle (Median Earnings), 2022

median_earnings_alb_2022 <- get_acs(
  geography = "tract",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B20002",
  survey = "acs5",
  year = 2022)

# Finalizing Table:

alb_median_earnings_2022 <- median_earnings_alb_2022 %>% 
  mutate(year = "2022",
         label = case_when(
           variable == "B20002_001" ~ "Median Earnings in Past 12 Months (Inflation Adjusted): Total",
           variable == "B20002_002" ~ "Median Earnings in Past 12 Months (Inflation Adjusted): Male",
           variable == "B20002_003" ~ "Median Earnings in Past 12 Months (Inflation Adjusted): Female",
         ))

# Generating CSV:

write.csv(alb_median_earnings_2022, "temp_data/alb_median_earnings_2022.csv")

# Table: B25064 (Median Gross Rent (Dollars)) - County Level, 2012-2022

# Charlottesville (Gross Rent), 2012-2022

gross_rent_cville_2012_2022 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "540",
           table = "B25064",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x)
  )

# Finalizing table:

cville_gross_rent_2012_2022 <- gross_rent_cville_2012_2022 %>% 
         mutate(label = case_when(
           variable == "B25064_001" ~ "Gross rent"
         ))

# Generating CSV:

write.csv(cville_gross_rent_2012_2022, "temp_data/cville_gross_rent_2012_2022.csv")

# Albemarle (Gross Rent), 2012-2022

gross_rent_alb_2012_2022 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "003",
           table = "B25064",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x)
  )

# Finalizing table:

alb_gross_rent_2012_2022 <- gross_rent_alb_2012_2022 %>% 
  mutate(label = case_when(
    variable == "B25064_001" ~ "Gross rent"
  ))

# Generating CSV:

write.csv(alb_gross_rent_2012_2022, "temp_data/alb_gross_rent_2012_2022.csv")

# Table: B25064 (Median Gross Rent (Dollars)) - Tract Level, 2022

# Charlottesville (Gross Rent: Tracts), 2022

gross_rent_tract_cville_2022 <- get_acs(
  geography = "tract",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B25064",
  survey = "acs5",
  year = 2022)

# Finalizing table:

cville_gross_rent_tract_2022 <- gross_rent_tract_cville_2022 %>% 
  mutate(year = "2022",
    label = case_when(
    variable == "B25064_001" ~ "Gross rent"
  ))

# Generating CSV:

write.csv(cville_gross_rent_tract_2022, "temp_data/cville_gross_rent_tract_2022.csv")

# Albemarle (Gross Rent: Tracts), 2022

gross_rent_tract_alb_2022 <- get_acs(
  geography = "tract",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B25064",
  survey = "acs5",
  year = 2022)

# Finalizing table:

alb_gross_rent_tract_2022 <- gross_rent_tract_alb_2022 %>% 
  mutate(year = "2022",
    label = case_when(
    variable == "B25064_001" ~ "Gross rent"
  ))

# Generating CSV:

write.csv(alb_gross_rent_tract_2022, "temp_data/alb_gross_rent_tract_2022.csv")

# Table: B25003 (Home Ownership) - Tract Level, 2022

# Charlottesville (Home Ownership), 2022

home_ownership_cville_2022 <- get_acs(
  geography = "tract",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B25003",
  summary_var = "B25003_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable %in% c("B25003_002", "B25003_003")
  )
         
# Finalizing table:

cville_home_ownership_2022 <- home_ownership_cville_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B25003_002" ~ "Owner occupied",
           variable == "B25003_003" ~ "Renter occupied",
         ))

# Generating CSV:

write.csv(cville_home_ownership_2022, "temp_data/cville_home_ownership_2022.csv")

# Albemarle (Home Ownership), 2022

home_ownership_alb_2022 <- get_acs(
  geography = "tract",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B25003",
  summary_var = "B25003_001",
  survey = "acs5",
  year = 2022)  %>% 
  filter(variable %in% c("B25003_002", "B25003_003")
  )

# Finalizing table:

alb_home_ownership_2022 <- home_ownership_alb_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B25003_002" ~ "Owner occupied",
           variable == "B25003_003" ~ "Renter occupied",
         ))

# Generating CSV:

write.csv(alb_home_ownership_2022, "temp_data/alb_home_ownership_2022.csv")

# Home Ownership by Race Tables: B25003A-B25003I, 2012 & 2022

# Note: three different methods for calculating home ownership by race are attempted
# Method one extends from lines 381 to 1151
# Method two extends from lines 1153 to 1317
# Method two extends from lines 1319 to 1975

# Method one: long approach (manually pulling each table):

          # Table: B25003A (Home Ownership by Race, White Alone), 2012 & 2022
          
          # Charlottesville (Home Ownership by Race, White Alone), 2022
          
          home_ownership_white_alone_cville_2022 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003A",
            summary_var = "B25003A_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003A_002", "B25003A_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_white_alone_2022 <- home_ownership_white_alone_cville_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003A_002" ~ "Owner occupied",
                     variable == "B25003A_003" ~ "Renter occupied",
                   ))
          
          # Charlottesville (Home Ownership by Race, White Alone), 2012
          
          home_ownership_white_alone_cville_2012 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003A",
            summary_var = "B25003A_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003A_002", "B25003A_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_white_alone_2012 <- home_ownership_white_alone_cville_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003A_002" ~ "Owner occupied",
                     variable == "B25003A_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, White Alone), 2022
          
          home_ownership_white_alone_alb_2022 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003A",
            summary_var = "B25003A_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003A_002", "B25003A_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_white_alone_2022 <- home_ownership_white_alone_alb_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003A_002" ~ "Owner occupied",
                     variable == "B25003A_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, White Alone), 2012
          
          home_ownership_white_alone_alb_2012 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003A",
            summary_var = "B25003A_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003A_002", "B25003A_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_white_alone_2012 <- home_ownership_white_alone_alb_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003A_002" ~ "Owner occupied",
                     variable == "B25003A_003" ~ "Renter occupied",
                   ))
          
          # Table: B25003B (Home Ownership by Race, Black or African American Alone), 2012 & 2022
          
          # Charlottesville (Home Ownership by Race, Black or African American Alone), 2022
          
          home_ownership_black_alone_cville_2022 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003B",
            summary_var = "B25003B_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003B_002", "B25003B_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_black_alone_2022 <- home_ownership_black_alone_cville_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003B_002" ~ "Owner occupied",
                     variable == "B25003B_003" ~ "Renter occupied",
                   ))
          
          # Charlottesville (Home Ownership by Race, Black or African American Alone), 2012
          
          home_ownership_black_alone_cville_2012 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003B",
            summary_var = "B25003B_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003B_002", "B25003B_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_black_alone_2012 <- home_ownership_black_alone_cville_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003B_002" ~ "Owner occupied",
                     variable == "B25003B_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Black or African American Alone), 2022
          
          home_ownership_black_alone_alb_2022 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003B",
            summary_var = "B25003B_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003B_002", "B25003B_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_black_alone_2022 <- home_ownership_black_alone_alb_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003B_002" ~ "Owner occupied",
                     variable == "B25003B_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Black or African American Alone), 2012
          
          home_ownership_black_alone_alb_2012 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003B",
            summary_var = "B25003B_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003B_002", "B25003B_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_black_alone_2012 <- home_ownership_black_alone_alb_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003B_002" ~ "Owner occupied",
                     variable == "B25003B_003" ~ "Renter occupied",
                   ))
          
          # Table: B25003C (Home Ownership by Race, American Indian and Alaska Native Alone), 2012 & 2022
          
          # Charlottesville (Home Ownership by Race, American Indian and Alaska Native Alone), 2022
          
          home_ownership_native_alone_cville_2022 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003C",
            summary_var = "B25003C_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003C_002", "B25003C_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_native_alone_2022 <- home_ownership_native_alone_cville_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003C_002" ~ "Owner occupied",
                     variable == "B25003C_003" ~ "Renter occupied",
                   ))
          
          # Charlottesville (Home Ownership by Race, American Indian and Alaska Native Alone), 2012
          
          home_ownership_native_alone_cville_2012 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003C",
            summary_var = "B25003C_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003C_002", "B25003C_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_native_alone_2012 <- home_ownership_native_alone_cville_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003C_002" ~ "Owner occupied",
                     variable == "B25003C_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, American Indian and Alaska Native Alone), 2022
          
          home_ownership_native_alone_alb_2022 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003C",
            summary_var = "B25003C_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003C_002", "B25003C_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_native_alone_2022 <- home_ownership_native_alone_alb_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003C_002" ~ "Owner occupied",
                     variable == "B25003C_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, American Indian and Alaska Native Alone), 2012
          
          home_ownership_native_alone_alb_2012 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003C",
            summary_var = "B25003C_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003C_002", "B25003C_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_native_alone_2012 <- home_ownership_native_alone_alb_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003C_002" ~ "Owner occupied",
                     variable == "B25003C_003" ~ "Renter occupied",
                   ))
          
          # Table: B25003D (Home Ownership by Race, Asian Alone), 2012 & 2022
          
          # Charlottesville (Home Ownership by Race, Asian Alone), 2022
          
          home_ownership_asian_alone_cville_2022 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003D",
            summary_var = "B25003D_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003D_002", "B25003D_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_asian_alone_2022 <- home_ownership_asian_alone_cville_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003D_002" ~ "Owner occupied",
                     variable == "B25003D_003" ~ "Renter occupied",
                   ))
          
          # Charlottesville (Home Ownership by Race, Asian Alone), 2012
          
          home_ownership_asian_alone_cville_2012 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003D",
            summary_var = "B25003D_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003D_002", "B25003D_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_asian_alone_2012 <- home_ownership_asian_alone_cville_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003D_002" ~ "Owner occupied",
                     variable == "B25003D_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Asian Alone), 2022
          
          home_ownership_asian_alone_alb_2022 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003D",
            summary_var = "B25003D_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003D_002", "B25003D_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_asian_alone_2022 <- home_ownership_asian_alone_alb_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003D_002" ~ "Owner occupied",
                     variable == "B25003D_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Asian Alone), 2012
          
          home_ownership_asian_alone_alb_2012 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003D",
            summary_var = "B25003D_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003D_002", "B25003D_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_asian_alone_2012 <- home_ownership_asian_alone_alb_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003D_002" ~ "Owner occupied",
                     variable == "B25003D_003" ~ "Renter occupied",
                   ))
          
          # Table: B25003E (Home Ownership by Race, Native Hawaiian and Other Pacific Islander Alone), 2012 & 2022
          
          # Charlottesville (Home Ownership by Race, Native Hawaiian and Other Pacific Islander Alone), 2022
          
          home_ownership_pacific_islander_alone_cville_2022 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003E",
            summary_var = "B25003E_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003E_002", "B25003E_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_pacific_islander_alone_2022 <- home_ownership_pacific_islander_alone_cville_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003E_002" ~ "Owner occupied",
                     variable == "B25003E_003" ~ "Renter occupied",
                   ))
          
          # Charlottesville (Home Ownership by Race, Native Hawaiian and Other Pacific Islander Alone), 2012
          
          home_ownership_pacific_islander_alone_cville_2012 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003E",
            summary_var = "B25003E_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003E_002", "B25003E_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_pacific_islander_alone_2012 <- home_ownership_pacific_islander_alone_cville_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003E_002" ~ "Owner occupied",
                     variable == "B25003E_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Native Hawaiian and Other Pacific Islander Alone), 2022
          
          home_ownership_pacific_islander_alone_alb_2022 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003E",
            summary_var = "B25003E_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003E_002", "B25003E_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_pacific_islander_alone_2022 <- home_ownership_pacific_islander_alone_alb_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003E_002" ~ "Owner occupied",
                     variable == "B25003E_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Native Hawaiian and Other Pacific Islander Alone), 2012
          
          home_ownership_pacific_islander_alone_alb_2012 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003E",
            summary_var = "B25003E_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003E_002", "B25003E_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_pacific_islander_alone_2012 <- home_ownership_pacific_islander_alone_alb_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003E_002" ~ "Owner occupied",
                     variable == "B25003E_003" ~ "Renter occupied",
                   ))
          
          # Table: B25003F (Home Ownership by Race, Some Other Race Alone), 2012 & 2022
          
          # Charlottesville (Home Ownership by Race, Some Other Race Alone), 2022
          
          home_ownership_other_race_alone_cville_2022 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003F",
            summary_var = "B25003F_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003F_002", "B25003F_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_other_race_alone_2022 <- home_ownership_other_race_alone_cville_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003F_002" ~ "Owner occupied",
                     variable == "B25003F_003" ~ "Renter occupied",
                   ))
          
          # Charlottesville (Home Ownership by Race, Some Other Race Alone), 2012
          
          home_ownership_other_race_alone_cville_2012 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003F",
            summary_var = "B25003F_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003F_002", "B25003F_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_other_race_alone_2012 <- home_ownership_other_race_alone_cville_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003F_002" ~ "Owner occupied",
                     variable == "B25003F_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Some Other Race Alone), 2022
          
          home_ownership_other_race_alone_alb_2022 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003F",
            summary_var = "B25003F_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003F_002", "B25003F_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_other_race_alone_2022 <- home_ownership_other_race_alone_alb_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003F_002" ~ "Owner occupied",
                     variable == "B25003F_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Some Other Race Alone), 2012
          
          home_ownership_other_race_alone_alb_2012 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003F",
            summary_var = "B25003F_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003F_002", "B25003F_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_other_race_alone_2012 <- home_ownership_other_race_alone_alb_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003F_002" ~ "Owner occupied",
                     variable == "B25003F_003" ~ "Renter occupied",
                   ))
          
          # Table: B25003G (Home Ownership by Race, Two or More Races), 2012 & 2022
          
          # Charlottesville (Home Ownership by Race, Two or More Races), 2022
          
          home_ownership_two_or_more_races_cville_2022 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003G",
            summary_var = "B25003G_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003G_002", "B25003G_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_two_or_more_races_2022 <- home_ownership_two_or_more_races_cville_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003G_002" ~ "Owner occupied",
                     variable == "B25003G_003" ~ "Renter occupied",
                   ))
          
          # Charlottesville (Home Ownership by Race, Two or More Races), 2012
          
          home_ownership_two_or_more_races_cville_2012 <- get_acs(
            geography = "county",
            county = "540",
            cache = TRUE,
            state = "VA",
            table = "B25003G",
            summary_var = "B25003G_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003G_002", "B25003G_003")
            )
          
          # Finalizing table:
          
          cville_home_ownership_two_or_more_races_2012 <- home_ownership_two_or_more_races_cville_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003G_002" ~ "Owner occupied",
                     variable == "B25003G_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Two or More Races), 2022
          
          home_ownership_two_or_more_races_alb_2022 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003G",
            summary_var = "B25003G_001",
            survey = "acs5",
            year = 2022)  %>% 
            filter(variable %in% c("B25003G_002", "B25003G_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_two_or_more_races_2022 <- home_ownership_two_or_more_races_alb_2022 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2022",
                   label = case_when(
                     variable == "B25003G_002" ~ "Owner occupied",
                     variable == "B25003G_003" ~ "Renter occupied",
                   ))
          
          # Albemarle (Home Ownership by Race, Two or More Races), 2012
          
          home_ownership_two_or_more_races_alb_2012 <- get_acs(
            geography = "county",
            county = "003",
            cache = TRUE,
            state = "VA",
            table = "B25003G",
            summary_var = "B25003G_001",
            survey = "acs5",
            year = 2012)  %>% 
            filter(variable %in% c("B25003G_002", "B25003G_003")
            )
          
          # Finalizing table:
          
          alb_home_ownership_two_or_more_races_2012 <- home_ownership_two_or_more_races_alb_2012 %>% 
            mutate(percent = 100 * (estimate / summary_est),
                   year = "2012",
                   label = case_when(
                     variable == "B25003G_002" ~ "Owner occupied",
                     variable == "B25003G_003" ~ "Renter occupied",
                   ))
          
          # Add Race columns to each dataframe
          cville_home_ownership_white_alone_2022 <- cville_home_ownership_white_alone_2022 %>%
            mutate(Race = "White Alone")
          cville_home_ownership_white_alone_2012 <- cville_home_ownership_white_alone_2012 %>%
            mutate(Race = "White Alone")
          alb_home_ownership_white_alone_2022 <- alb_home_ownership_white_alone_2022 %>%
            mutate(Race = "White Alone")
          alb_home_ownership_white_alone_2012 <- alb_home_ownership_white_alone_2012 %>%
            mutate(Race = "White Alone")
          
          cville_home_ownership_black_alone_2022 <- cville_home_ownership_black_alone_2022 %>%
            mutate(Race = "Black Alone")
          cville_home_ownership_black_alone_2012 <- cville_home_ownership_black_alone_2012 %>%
            mutate(Race = "Black Alone")
          alb_home_ownership_black_alone_2022 <- alb_home_ownership_black_alone_2022 %>%
            mutate(Race = "Black Alone")
          alb_home_ownership_black_alone_2012 <- alb_home_ownership_black_alone_2012 %>%
            mutate(Race = "Black Alone")
          
          cville_home_ownership_asian_alone_2022 <- cville_home_ownership_asian_alone_2022 %>%
            mutate(Race = "Asian Alone")
          cville_home_ownership_asian_alone_2012 <- cville_home_ownership_asian_alone_2012 %>%
            mutate(Race = "Asian Alone")
          alb_home_ownership_asian_alone_2022 <- alb_home_ownership_asian_alone_2022 %>%
            mutate(Race = "Asian Alone")
          alb_home_ownership_asian_alone_2012 <- alb_home_ownership_asian_alone_2012 %>%
            mutate(Race = "Asian Alone")
          
          cville_home_ownership_pacific_islander_alone_2022 <- cville_home_ownership_pacific_islander_alone_2022 %>%
            mutate(Race = "Pacific Islander Alone")
          cville_home_ownership_pacific_islander_alone_2012 <- cville_home_ownership_pacific_islander_alone_2012 %>%
            mutate(Race = "Pacific Islander Alone")
          alb_home_ownership_pacific_islander_alone_2022 <- alb_home_ownership_pacific_islander_alone_2022 %>%
            mutate(Race = "Pacific Islander Alone")
          alb_home_ownership_pacific_islander_alone_2012 <- alb_home_ownership_pacific_islander_alone_2012 %>%
            mutate(Race = "Pacific Islander Alone")
          
          cville_home_ownership_other_race_alone_2022 <- cville_home_ownership_other_race_alone_2022 %>%
            mutate(Race = "Other Race Alone")
          cville_home_ownership_other_race_alone_2012 <- cville_home_ownership_other_race_alone_2012 %>%
            mutate(Race = "Other Race Alone")
          alb_home_ownership_other_race_alone_2022 <- alb_home_ownership_other_race_alone_2022 %>%
            mutate(Race = "Other Race Alone")
          alb_home_ownership_other_race_alone_2012 <- alb_home_ownership_other_race_alone_2012 %>%
            mutate(Race = "Other Race Alone")
          
          cville_home_ownership_two_or_more_races_2022 <- cville_home_ownership_two_or_more_races_2022 %>%
            mutate(Race = "Two or More Races")
          cville_home_ownership_two_or_more_races_2012 <- cville_home_ownership_two_or_more_races_2012 %>%
            mutate(Race = "Two or More Races")
          alb_home_ownership_two_or_more_races_2022 <- alb_home_ownership_two_or_more_races_2022 %>%
            mutate(Race = "Two or More Races")
          alb_home_ownership_two_or_more_races_2012 <- alb_home_ownership_two_or_more_races_2012 %>%
            mutate(Race = "Two or More Races")
          
          
          # Combining all data
          
          combined_home_ownership_race <- bind_rows(
            cville_home_ownership_white_alone_2022,
            cville_home_ownership_white_alone_2012,
            alb_home_ownership_white_alone_2022,
            alb_home_ownership_white_alone_2012,
            cville_home_ownership_black_alone_2022,
            cville_home_ownership_black_alone_2012,
            alb_home_ownership_black_alone_2022,
            alb_home_ownership_black_alone_2012,
            cville_home_ownership_asian_alone_2022,
            cville_home_ownership_asian_alone_2012,
            alb_home_ownership_asian_alone_2022,
            alb_home_ownership_asian_alone_2012,
            cville_home_ownership_pacific_islander_alone_2022,
            cville_home_ownership_pacific_islander_alone_2012,
            alb_home_ownership_pacific_islander_alone_2022,
            alb_home_ownership_pacific_islander_alone_2012,
            cville_home_ownership_other_race_alone_2022,
            cville_home_ownership_other_race_alone_2012,
            alb_home_ownership_other_race_alone_2022,
            alb_home_ownership_other_race_alone_2012,
            cville_home_ownership_two_or_more_races_2022,
            cville_home_ownership_two_or_more_races_2012,
            alb_home_ownership_two_or_more_races_2022,
            alb_home_ownership_two_or_more_races_2012)
          
# Method two: (fewer lines of code)
 
          vars <- c("B25003A_001", # white alone, total
                    "B25003A_002", # white alone, owner occupied
                    "B25003A_003", # white alone, renter occupied
                    "B25003B_001", # black alone, total
                    "B25003B_002", # black alone, owner occupied
                    "B25003B_003", # black alone, renter occupied
                    "B25003C_001", # native alone, total
                    "B25003C_002", # native alone, owner occupied
                    "B25003C_003", # native alone, renter occupied
                    "B25003D_001", # asian alone, total
                    "B25003D_002", # asian alone, owner occupied
                    "B25003D_003", # asian alone, renter occupied
                    "B25003E_001", # asian alone, total
                    "B25003E_002", # pacific islander alone, owner occupied
                    "B25003E_003", # pacific islander alone, renter occupied
                    "B25003F_001", # some other race alone, total
                    "B25003F_002", # some other race alone, owner occupied
                    "B25003F_003", # some other race alone, renter occupied
                    "B25003G_001", # two or more races, total
                    "B25003G_002", # two or more races, owner occupied
                    "B25003G_003", # two or more races, renter occupied
                    "B25003H_001", # white alone, not hispanic or latino, total
                    "B25003H_002", # white alone, not hispanic or latino, owner occupied
                    "B25003H_003", # white alone, not hispanic or latino, renter occupied
                    "B25003I_001", # hispanic or latino, total
                    "B25003I_002", # hispanic or latino, owner occupied
                    "B25003I_003") # hispanic or latino, renter occupied
          
          # Charlottesville, 2022
          
          home_ownership_cville_2022 <- get_acs(geography = "county",
                                             variable = vars,
                                             county = "540",
                                             cache = TRUE,
                                             state = "VA",
                                             survey = "acs5",
                                             year = 2022,
                                             output = "wide")
          
          # Calculate percentages, rename variables, select variables
          home_ownership_cville_2022 <- home_ownership_cville_2022 %>%
            select(-ends_with("M")) %>% 
            mutate(homeowners_white_alone_pct = 100 * B25003A_002E / B25003A_001E,
                   renters_white_alone_pct = 100 * B25003A_003E / B25003A_001E,
                   homeowners_black_alone_pct = 100 * B25003B_002E / B25003B_001E,
                   renters_black_alone_pct = 100 * B25003B_003E / B25003B_001E,
                   homeowners_native_alone_pct = 100 * B25003C_002E / B25003C_001E,
                   renters_native_alone_pct = 100 * B25003C_003E / B25003C_001E,
                   homeowners_asian_alone_pct = 100 * B25003D_002E / B25003D_001E,
                   renters_asian_alone_pct = 100 * B25003D_003E / B25003D_001E,
                   homeowners_pacific_islander_alone_pct = 100 * B25003E_002E / B25003E_001E,
                   renters_pacific_islander_alone_pct = 100 * B25003E_003E / B25003E_001E,
                   homeowners_some_other_race_pct = 100 * B25003F_002E / B25003F_001E,
                   renters_some_other_race_pct = 100 * B25003F_003E / B25003F_001E,
                   homeowners_two_or_more_races_pct = 100 * B25003G_002E / B25003G_001E,
                   renters_two_or_more_races_pct = 100 * B25003G_003E / B25003G_001E,
                   homeowners_white_alone_not_hispanic_pct = 100 * B25003H_002E / B25003H_001E,
                   renters_white_alone_not_hispanic_pct = 100 * B25003H_003E / B25003H_001E,
                   homeowners_hispanic_latino_pct = 100 * B25003I_002E / B25003I_001E,
                   renters_hispanic_latino_pct = 100 * B25003I_003E / B25003I_001E) %>%
            select(GEOID, NAME, contains("pct"))
          
          # Charlottesville, 2012
          
          home_ownership_cville_2012 <- get_acs(geography = "county",
                                             table = vars,
                                             county = "540",
                                             cache = TRUE,
                                             state = "VA",
                                             survey = "acs5",
                                             year = 2012,
                                             output = "wide")
          
          # Calculate percentages, rename variables, select variables
          home_ownership_cville_2012 <- home_ownership_cville_2012 %>%
            select(-ends_with("M")) %>% 
            mutate(homeowners_white_alone_pct = 100 * B25003A_002E / B25003A_001E,
                   renters_white_alone_pct = 100 * B25003A_003E / B25003A_001E,
                   homeowners_black_alone_pct = 100 * B25003B_002E / B25003B_001E,
                   renters_black_alone_pct = 100 * B25003B_003E / B25003B_001E,
                   homeowners_native_alone_pct = 100 * B25003C_002E / B25003C_001E,
                   renters_native_alone_pct = 100 * B25003C_003E / B25003C_001E,
                   homeowners_asian_alone_pct = 100 * B25003D_002E / B25003D_001E,
                   renters_asian_alone_pct = 100 * B25003D_003E / B25003D_001E,
                   homeowners_pacific_islander_alone_pct = 100 * B25003E_002E / B25003E_001E,
                   renters_pacific_islander_alone_pct = 100 * B25003E_003E / B25003E_001E,
                   homeowners_some_other_race_pct = 100 * B25003F_002E / B25003F_001E,
                   renters_some_other_race_pct = 100 * B25003F_003E / B25003F_001E,
                   homeowners_two_or_more_races_pct = 100 * B25003G_002E / B25003G_001E,
                   renters_two_or_more_races_pct = 100 * B25003G_003E / B25003G_001E,
                   homeowners_white_alone_not_hispanic_pct = 100 * B25003H_002E / B25003H_001E,
                   renters_white_alone_not_hispanic_pct = 100 * B25003H_003E / B25003H_001E,
                   homeowners_hispanic_latino_pct = 100 * B25003I_002E / B25003I_001E,
                   renters_hispanic_latino_pct = 100 * B25003I_003E / B25003I_001E) %>%
            select(GEOID, NAME, contains("pct"))
          
          # Albemarle, 2022
          
          home_ownership_alb_2022 <- get_acs(geography = "county",
                               variable = vars,
                               county = "003",
                               cache = TRUE,
                               state = "VA",
                               survey = "acs5",
                               year = 2022,
                               output = "wide")
          
          # Calculate percentages, rename variables, select variables
          home_ownership_alb_2022 <- home_ownership_alb_2022 %>%
            select(-ends_with("M")) %>% 
            mutate(homeowners_white_alone_pct = 100 * B25003A_002E / B25003A_001E,
                   renters_white_alone_pct = 100 * B25003A_003E / B25003A_001E,
                   homeowners_black_alone_pct = 100 * B25003B_002E / B25003B_001E,
                   renters_black_alone_pct = 100 * B25003B_003E / B25003B_001E,
                   homeowners_native_alone_pct = 100 * B25003C_002E / B25003C_001E,
                   renters_native_alone_pct = 100 * B25003C_003E / B25003C_001E,
                   homeowners_asian_alone_pct = 100 * B25003D_002E / B25003D_001E,
                   renters_asian_alone_pct = 100 * B25003D_003E / B25003D_001E,
                   homeowners_pacific_islander_alone_pct = 100 * B25003E_002E / B25003E_001E,
                   renters_pacific_islander_alone_pct = 100 * B25003E_003E / B25003E_001E,
                   homeowners_some_other_race_pct = 100 * B25003F_002E / B25003F_001E,
                   renters_some_other_race_pct = 100 * B25003F_003E / B25003F_001E,
                   homeowners_two_or_more_races_pct = 100 * B25003G_002E / B25003G_001E,
                   renters_two_or_more_races_pct = 100 * B25003G_003E / B25003G_001E,
                   homeowners_white_alone_not_hispanic_pct = 100 * B25003H_002E / B25003H_001E,
                   renters_white_alone_not_hispanic_pct = 100 * B25003H_003E / B25003H_001E,
                   homeowners_hispanic_latino_pct = 100 * B25003I_002E / B25003I_001E,
                   renters_hispanic_latino_pct = 100 * B25003I_003E / B25003I_001E) %>%
            select(GEOID, NAME, contains("pct"))
          
          # Albemarle, 2012
          
          home_ownership_alb_2012 <- get_acs(geography = "county",
                                             variable = vars,
                                             county = "003",
                                             cache = TRUE,
                                             state = "VA",
                                             survey = "acs5",
                                             year = 2012,
                                             output = "wide")
          
          # Calculate percentages, rename variables, select variables
          home_ownership_alb_2012 <- home_ownership_alb_2012 %>%
            select(-ends_with("M")) %>% 
            mutate(homeowners_white_alone_pct = 100 * B25003A_002E / B25003A_001E,
                   renters_white_alone_pct = 100 * B25003A_003E / B25003A_001E,
                   homeowners_black_alone_pct = 100 * B25003B_002E / B25003B_001E,
                   renters_black_alone_pct = 100 * B25003B_003E / B25003B_001E,
                   homeowners_native_alone_pct = 100 * B25003C_002E / B25003C_001E,
                   renters_native_alone_pct = 100 * B25003C_003E / B25003C_001E,
                   homeowners_asian_alone_pct = 100 * B25003D_002E / B25003D_001E,
                   renters_asian_alone_pct = 100 * B25003D_003E / B25003D_001E,
                   homeowners_pacific_islander_alone_pct = 100 * B25003E_002E / B25003E_001E,
                   renters_pacific_islander_alone_pct = 100 * B25003E_003E / B25003E_001E,
                   homeowners_some_other_race_pct = 100 * B25003F_002E / B25003F_001E,
                   renters_some_other_race_pct = 100 * B25003F_003E / B25003F_001E,
                   homeowners_two_or_more_races_pct = 100 * B25003G_002E / B25003G_001E,
                   renters_two_or_more_races_pct = 100 * B25003G_003E / B25003G_001E,
                   homeowners_white_alone_not_hispanic_pct = 100 * B25003H_002E / B25003H_001E,
                   renters_white_alone_not_hispanic_pct = 100 * B25003H_003E / B25003H_001E,
                   homeowners_hispanic_latino_pct = 100 * B25003I_002E / B25003I_001E,
                   renters_hispanic_latino_pct = 100 * B25003I_003E / B25003I_001E) %>%
            select(GEOID, NAME, contains("pct"))
          
# Method three (based on United Way Code): 
   
  # Charlottesville, 2022
          
          # White alone
          
          vars_B25003A <- c(pop_own_white = "B25003A_002", # renter occupied
                            pop_rent_white = "B25003A_003" # owner occupied
          )
          
          home_ownership_white_cville_2022 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003A,
                                                      year = 2022,
                                                      survey = "acs5")
          
          # Black alone
          
          vars_B25003B <- c(pop_own_black = "B25003B_002", # renter occupied
                            pop_rent_black = "B25003B_003" # owner occupied
          )
          
          home_ownership_black_cville_2022 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003B,
                                                      year = 2022,
                                                      survey = "acs5")
          
          # American Indian/Alaska Native
          
          vars_B25003C <- c(pop_own_aian = "B25003C_002", # renter occupied
                            pop_rent_aian = "B25003C_003" # owner occupied
          )
          
          home_ownership_aian_cville_2022 <- get_acs(geography = "county",
                                                     state = "VA",
                                                     county = "540",
                                                     var = vars_B25003C,
                                                     year = 2022,
                                                     survey = "acs5")
          
          # Asian alone
          
          vars_B25003D <- c(pop_own_asian = "B25003D_002", # renter occupied
                            pop_rent_asian = "B25003D_003" # owner occupied) 
          )
          
          home_ownership_asian_cville_2022 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003D,
                                                      year = 2022,
                                                      survey = "acs5")
          
          # Native Hawaiian / Pacific Islander Alone
          
          vars_B25003E <- c(pop_own_nhpi = "B25003E_002", # renter occupied
                            pop_rent_nhpi = "B25003E_003" # owner occupied
          )
          
          home_ownership_nhpi_cville_2022 <- get_acs(geography = "county",
                                                     state = "VA",
                                                     county = "540",
                                                     var = vars_B25003E,
                                                     year = 2022,
                                                     survey = "acs5")
          
          # Some other race alone
          
          vars_B25003F <- c(pop_own_other = "B25003F_002", # renter occupied
                            pop_rent_other = "B25003F_003" # owner occupied)
          )
          
          home_ownership_other_cville_2022 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003F,
                                                      year = 2022,
                                                      survey = "acs5")                 
          
          # Two or more races
          
          vars_B25003G <- c(pop_own_two_or_more = "B25003G_002", # renter occupied
                            pop_rent_two_or_more = "B25003G_003" # owner occupied)
          )
          
          home_ownership_two_or_more_cville_2022 <- get_acs(geography = "county",
                                                            state = "VA",
                                                            county = "540",
                                                            var = vars_B25003G,
                                                            year = 2022,
                                                            survey = "acs5")                  
          
          
          # White alone, not hispanic or latino              
          
          vars_B25003H <- c(pop_own_white_non_hisp = "B25003H_002", # renter occupied
                            pop_rent_white_non_hisp = "B25003H_003" # owner occupied)
          )
          
          home_ownership_white_non_hisp_cville_2022 <- get_acs(geography = "county",
                                                               state = "VA",
                                                               county = "540",
                                                               var = vars_B25003H,
                                                               year = 2022,
                                                               survey = "acs5")                                       
          
          # Hispanic or latino               
          
          vars_B25003I <- c(pop_own_hisp = "B25003I_002", # renter occupied
                            pop_rent_hisp = "B25003I_003" # owner occupied)
          )
          
          home_ownership_hisp_cville_2022 <- get_acs(geography = "county",
                                                     state = "VA",
                                                     county = "540",
                                                     var = vars_B25003I,
                                                     year = 2022,
                                                     survey = "acs5")
          
          # Combined race tables
          
          home_ownership_race_cville_2022 <- bind_rows(home_ownership_white_cville_2022, home_ownership_black_cville_2022,
                                                       home_ownership_aian_cville_2022, home_ownership_asian_cville_2022,
                                                       home_ownership_nhpi_cville_2022, home_ownership_other_cville_2022,
                                                       home_ownership_two_or_more_cville_2022, home_ownership_white_non_hisp_cville_2022,
                                                       home_ownership_hisp_cville_2022)
          
          # Get totals from B25003 summary table, joined to the race/ethn specific tables to calculate proportions
          vars_B25003 <- c(total_tenure = "B25003_001", 
                           total_homeowner = "B25003_002", 
                           total_renter = "B25003_003")
          
          tenure_totals <- get_acs(geography = "county",
                                   state = "VA",
                                   county = "540",
                                   var = vars_B25003,
                                   year = 2022,
                                   survey = "acs5")
          
          home_ownership_race_cville_2022 <- bind_rows(home_ownership_race_cville_2022, tenure_totals)
          
          # Identify the values for total_tenure, total_homeowner, and total_renter
          total_tenure_val <- home_ownership_race_cville_2022 %>%
            filter(variable == "total_tenure") %>%
            pull(estimate)
          
          total_homeowner_val <- home_ownership_race_cville_2022 %>%
            filter(variable == "total_homeowner") %>%
            pull(estimate)
          
          total_renter_val <- home_ownership_race_cville_2022 %>%
            filter(variable == "total_renter") %>%
            pull(estimate)
          
          # Filter out the total rows and create new columns
          home_ownership_race_cville_2022_processed <- home_ownership_race_cville_2022 %>%
            filter(variable != "total_tenure" & variable != "total_homeowner" & variable != "total_renter") %>%
            mutate(total_tenure = total_tenure_val,
                   total_homeowner = total_homeowner_val,
                   total_renter = total_renter_val)
          
          # Calculate percentages (unfinished) (!)
                 
  # Charlottesville, 2012   
          
          # White alone
          
          vars_B25003A <- c(pop_own_white = "B25003A_002", # renter occupied
                            pop_rent_white = "B25003A_003" # owner occupied
          )
          
          home_ownership_white_cville_2012 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003A,
                                                      year = 2012,
                                                      survey = "acs5")
          
          # Black alone
          
          vars_B25003B <- c(pop_own_black = "B25003B_002", # renter occupied
                            pop_rent_black = "B25003B_003" # owner occupied
          )
          
          home_ownership_black_cville_2012 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003B,
                                                      year = 2012,
                                                      survey = "acs5")
          
          # American Indian/Alaska Native
          
          vars_B25003C <- c(pop_own_aian = "B25003C_002", # renter occupied
                            pop_rent_aian = "B25003C_003" # owner occupied
          )
          
          home_ownership_aian_cville_2012 <- get_acs(geography = "county",
                                                     state = "VA",
                                                     county = "540",
                                                     var = vars_B25003C,
                                                     year = 2012,
                                                     survey = "acs5")
          
          # Asian alone
          
          vars_B25003D <- c(pop_own_asian = "B25003D_002", # renter occupied
                            pop_rent_asian = "B25003D_003" # owner occupied) 
          )
          
          home_ownership_asian_cville_2012 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003D,
                                                      year = 2012,
                                                      survey = "acs5")
          
          # Native Hawaiian / Pacific Islander Alone
          
          vars_B25003E <- c(pop_own_nhpi = "B25003E_002", # renter occupied
                            pop_rent_nhpi = "B25003E_003" # owner occupied
          )
          
          home_ownership_nhpi_cville_2012 <- get_acs(geography = "county",
                                                     state = "VA",
                                                     county = "540",
                                                     var = vars_B25003E,
                                                     year = 2012,
                                                     survey = "acs5")
          
          # Some other race alone
          
          vars_B25003F <- c(pop_own_other = "B25003F_002", # renter occupied
                            pop_rent_other = "B25003F_003" # owner occupied)
          )
          
          home_ownership_other_cville_2012 <- get_acs(geography = "county",
                                                      state = "VA",
                                                      county = "540",
                                                      var = vars_B25003F,
                                                      year = 2012,
                                                      survey = "acs5")                 
          
          # Two or more races
          
          vars_B25003G <- c(pop_own_two_or_more = "B25003G_002", # renter occupied
                            pop_rent_two_or_more = "B25003G_003" # owner occupied)
          )
          
          home_ownership_two_or_more_cville_2012 <- get_acs(geography = "county",
                                                            state = "VA",
                                                            county = "540",
                                                            var = vars_B25003G,
                                                            year = 2012,
                                                            survey = "acs5")                  
          
          
          # White alone, not hispanic or latino              
          
          vars_B25003H <- c(pop_own_white_non_hisp = "B25003H_002", # renter occupied
                            pop_rent_white_non_hisp = "B25003H_003" # owner occupied)
          )
          
          home_ownership_white_non_hisp_cville_2012 <- get_acs(geography = "county",
                                                               state = "VA",
                                                               county = "540",
                                                               var = vars_B25003H,
                                                               year = 2012,
                                                               survey = "acs5")                                       
          
          # Hispanic or latino               
          
          vars_B25003I <- c(pop_own_hisp = "B25003I_002", # renter occupied
                            pop_rent_hisp = "B25003I_003" # owner occupied)
          )
          
          home_ownership_hisp_cville_2012 <- get_acs(geography = "county",
                                                     state = "VA",
                                                     county = "540",
                                                     var = vars_B25003I,
                                                     year = 2012,
                                                     survey = "acs5")
          
          # Combined race tables
          
          home_ownership_race_cville_2012 <- bind_rows(home_ownership_white_cville_2012, home_ownership_black_cville_2012,
                                                       home_ownership_aian_cville_2012, home_ownership_asian_cville_2012,
                                                       home_ownership_nhpi_cville_2012, home_ownership_other_cville_2012,
                                                       home_ownership_two_or_more_cville_2012, home_ownership_white_non_hisp_cville_2012,
                                                       home_ownership_hisp_cville_2012)
          
          # Get totals from B25003 summary table, joined to the race/ethn specific tables to calculate proportions
          vars_B25003 <- c(total_tenure = "B25003_001", 
                           total_homeowner = "B25003_002", 
                           total_renter = "B25003_003")
          
          tenure_totals <- get_acs(geography = "county",
                                   state = "VA",
                                   county = "540",
                                   var = vars_B25003,
                                   year = 2012,
                                   survey = "acs5")
          
          home_ownership_race_cville_2012 <- bind_rows(home_ownership_race_cville_2012, tenure_totals)
          
          # Identify the values for total_tenure, total_homeowner, and total_renter
          total_tenure_val <- home_ownership_race_cville_2012 %>%
            filter(variable == "total_tenure") %>%
            pull(estimate)
          
          total_homeowner_val <- home_ownership_race_cville_2012 %>%
            filter(variable == "total_homeowner") %>%
            pull(estimate)
          
          total_renter_val <- home_ownership_race_cville_2012 %>%
            filter(variable == "total_renter") %>%
            pull(estimate)
          
          # Filter out the total rows and create new columns
          home_ownership_race_cville_2012_processed <- home_ownership_race_cville_2012 %>%
            filter(variable != "total_tenure" & variable != "total_homeowner" & variable != "total_renter") %>%
            mutate(total_tenure = total_tenure_val,
                   total_homeowner = total_homeowner_val,
                   total_renter = total_renter_val)
          
          # Calculate percentages (unfinished) (!)
          
# Albemarle, 2022
          
          # White alone
          
          vars_B25003A <- c(pop_own_white = "B25003A_002", # renter occupied
                            pop_rent_white = "B25003A_003" # owner occupied
          )
          
          home_ownership_white_alb_2022 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003A,
                                                   year = 2022,
                                                   survey = "acs5")
          
          # Black alone
          
          vars_B25003B <- c(pop_own_black = "B25003B_002", # renter occupied
                            pop_rent_black = "B25003B_003" # owner occupied
          )
          
          home_ownership_black_alb_2022 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003B,
                                                   year = 2022,
                                                   survey = "acs5")
          
          # American Indian/Alaska Native
          
          vars_B25003C <- c(pop_own_aian = "B25003C_002", # renter occupied
                            pop_rent_aian = "B25003C_003" # owner occupied
          )
          
          home_ownership_aian_alb_2022 <- get_acs(geography = "county",
                                                  state = "VA",
                                                  county = "003",
                                                  var = vars_B25003C,
                                                  year = 2022,
                                                  survey = "acs5")
          
          # Asian alone
          
          vars_B25003D <- c(pop_own_asian = "B25003D_002", # renter occupied
                            pop_rent_asian = "B25003D_003" # owner occupied) 
          )
          
          home_ownership_asian_alb_2022 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003D,
                                                   year = 2022,
                                                   survey = "acs5")
          
          # Native Hawaiian / Pacific Islander Alone
          
          vars_B25003E <- c(pop_own_nhpi = "B25003E_002", # renter occupied
                            pop_rent_nhpi = "B25003E_003" # owner occupied
          )
          
          home_ownership_nhpi_alb_2022 <- get_acs(geography = "county",
                                                  state = "VA",
                                                  county = "003",
                                                  var = vars_B25003E,
                                                  year = 2022,
                                                  survey = "acs5")
          
          # Some other race alone
          
          vars_B25003F <- c(pop_own_other = "B25003F_002", # renter occupied
                            pop_rent_other = "B25003F_003" # owner occupied)
          )
          
          home_ownership_other_alb_2022 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003F,
                                                   year = 2022,
                                                   survey = "acs5")                 
          
          # Two or more races
          
          vars_B25003G <- c(pop_own_two_or_more = "B25003G_002", # renter occupied
                            pop_rent_two_or_more = "B25003G_003" # owner occupied)
          )
          
          home_ownership_two_or_more_alb_2022 <- get_acs(geography = "county",
                                                         state = "VA",
                                                         county = "003",
                                                         var = vars_B25003G,
                                                         year = 2022,
                                                         survey = "acs5")                  
          
          
          # White alone, not hispanic or latino              
          
          vars_B25003H <- c(pop_own_white_non_hisp = "B25003H_002", # renter occupied
                            pop_rent_white_non_hisp = "B25003H_003" # owner occupied)
          )
          
          home_ownership_white_non_hisp_alb_2022 <- get_acs(geography = "county",
                                                            state = "VA",
                                                            county = "003",
                                                            var = vars_B25003H,
                                                            year = 2022,
                                                            survey = "acs5")                                       
          
          # Hispanic or latino               
          
          vars_B25003I <- c(pop_own_hisp = "B25003I_002", # renter occupied
                            pop_rent_hisp = "B25003I_003" # owner occupied)
          )
          
          home_ownership_hisp_alb_2022 <- get_acs(geography = "county",
                                                  state = "VA",
                                                  county = "003",
                                                  var = vars_B25003I,
                                                  year = 2022,
                                                  survey = "acs5")
          
          # Combined race tables
          
          home_ownership_race_alb_2022 <- bind_rows(home_ownership_white_alb_2022, home_ownership_black_alb_2022,
                                                    home_ownership_aian_alb_2022, home_ownership_asian_alb_2022,
                                                    home_ownership_nhpi_alb_2022, home_ownership_other_alb_2022,
                                                    home_ownership_two_or_more_alb_2022, home_ownership_white_non_hisp_alb_2022,
                                                    home_ownership_hisp_alb_2022)
          
          # Get totals from B25003 summary table, joined to the race/ethn specific tables to calculate proportions
          vars_B25003 <- c(total_tenure = "B25003_001", 
                           total_homeowner = "B25003_002", 
                           total_renter = "B25003_003")
          
          tenure_totals <- get_acs(geography = "county",
                                   state = "VA",
                                   county = "003",
                                   var = vars_B25003,
                                   year = 2022,
                                   survey = "acs5")
          
          home_ownership_race_alb_2022 <- bind_rows(home_ownership_race_alb_2022, tenure_totals)
          
          # Identify the values for total_tenure, total_homeowner, and total_renter
          total_tenure_val <- home_ownership_race_alb_2022 %>%
            filter(variable == "total_tenure") %>%
            pull(estimate)
          
          total_homeowner_val <- home_ownership_race_alb_2022 %>%
            filter(variable == "total_homeowner") %>%
            pull(estimate)
          
          total_renter_val <- home_ownership_race_alb_2022 %>%
            filter(variable == "total_renter") %>%
            pull(estimate)
          
          # Filter out the total rows and create new columns
          home_ownership_race_alb_2022_processed <- home_ownership_race_alb_2022 %>%
            filter(variable != "total_tenure" & variable != "total_homeowner" & variable != "total_renter") %>%
            mutate(total_tenure = total_tenure_val,
                   total_homeowner = total_homeowner_val,
                   total_renter = total_renter_val)
          
          # Calculate percentages (unfinished) (!)
          
# Albemarle, 2012   
          
          # White alone
          
          vars_B25003A <- c(pop_own_white = "B25003A_002", # renter occupied
                            pop_rent_white = "B25003A_003" # owner occupied
          )
          
          home_ownership_white_alb_2012 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003A,
                                                   year = 2012,
                                                   survey = "acs5")
          
          # Black alone
          
          vars_B25003B <- c(pop_own_black = "B25003B_002", # renter occupied
                            pop_rent_black = "B25003B_003" # owner occupied
          )
          
          home_ownership_black_alb_2012 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003B,
                                                   year = 2012,
                                                   survey = "acs5")
          
          # American Indian/Alaska Native
          
          vars_B25003C <- c(pop_own_aian = "B25003C_002", # renter occupied
                            pop_rent_aian = "B25003C_003" # owner occupied
          )
          
          home_ownership_aian_alb_2012 <- get_acs(geography = "county",
                                                  state = "VA",
                                                  county = "003",
                                                  var = vars_B25003C,
                                                  year = 2012,
                                                  survey = "acs5")
          
          # Asian alone
          
          vars_B25003D <- c(pop_own_asian = "B25003D_002", # renter occupied
                            pop_rent_asian = "B25003D_003" # owner occupied) 
          )
          
          home_ownership_asian_alb_2012 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003D,
                                                   year = 2012,
                                                   survey = "acs5")
          
          # Native Hawaiian / Pacific Islander Alone
          
          vars_B25003E <- c(pop_own_nhpi = "B25003E_002", # renter occupied
                            pop_rent_nhpi = "B25003E_003" # owner occupied
          )
          
          home_ownership_nhpi_alb_2012 <- get_acs(geography = "county",
                                                  state = "VA",
                                                  county = "003",
                                                  var = vars_B25003E,
                                                  year = 2012,
                                                  survey = "acs5")
          
          # Some other race alone
          
          vars_B25003F <- c(pop_own_other = "B25003F_002", # renter occupied
                            pop_rent_other = "B25003F_003" # owner occupied)
          )
          
          home_ownership_other_alb_2012 <- get_acs(geography = "county",
                                                   state = "VA",
                                                   county = "003",
                                                   var = vars_B25003F,
                                                   year = 2012,
                                                   survey = "acs5")                 
          
          # Two or more races
          
          vars_B25003G <- c(pop_own_two_or_more = "B25003G_002", # renter occupied
                            pop_rent_two_or_more = "B25003G_003" # owner occupied)
          )
          
          home_ownership_two_or_more_alb_2012 <- get_acs(geography = "county",
                                                         state = "VA",
                                                         county = "003",
                                                         var = vars_B25003G,
                                                         year = 2012,
                                                         survey = "acs5")                  
          
          
          # White alone, not hispanic or latino              
          
          vars_B25003H <- c(pop_own_white_non_hisp = "B25003H_002", # renter occupied
                            pop_rent_white_non_hisp = "B25003H_003" # owner occupied)
          )
          
          home_ownership_white_non_hisp_alb_2012 <- get_acs(geography = "county",
                                                            state = "VA",
                                                            county = "003",
                                                            var = vars_B25003H,
                                                            year = 2012,
                                                            survey = "acs5")                                       
          
          # Hispanic or latino               
          
          vars_B25003I <- c(pop_own_hisp = "B25003I_002", # renter occupied
                            pop_rent_hisp = "B25003I_003" # owner occupied)
          )
          
          home_ownership_hisp_alb_2012 <- get_acs(geography = "county",
                                                  state = "VA",
                                                  county = "003",
                                                  var = vars_B25003I,
                                                  year = 2012,
                                                  survey = "acs5")
          
          # Combined race tables
          
          home_ownership_race_alb_2012 <- bind_rows(home_ownership_white_alb_2012, home_ownership_black_alb_2012,
                                                    home_ownership_aian_alb_2012, home_ownership_asian_alb_2012,
                                                    home_ownership_nhpi_alb_2012, home_ownership_other_alb_2012,
                                                    home_ownership_two_or_more_alb_2012, home_ownership_white_non_hisp_alb_2012,
                                                    home_ownership_hisp_alb_2012)
          
          # Get totals from B25003 summary table, joined to the race/ethn specific tables to calculate proportions
          vars_B25003 <- c(total_tenure = "B25003_001", 
                           total_homeowner = "B25003_002", 
                           total_renter = "B25003_003")
          
          tenure_totals <- get_acs(geography = "county",
                                   state = "VA",
                                   county = "003",
                                   var = vars_B25003,
                                   year = 2012,
                                   survey = "acs5")
          
          home_ownership_race_alb_2012 <- bind_rows(home_ownership_race_alb_2012, tenure_totals)
          
          # Identify the values for total_tenure, total_homeowner, and total_renter
          total_tenure_val <- home_ownership_race_alb_2012 %>%
            filter(variable == "total_tenure") %>%
            pull(estimate)
          
          total_homeowner_val <- home_ownership_race_alb_2012 %>%
            filter(variable == "total_homeowner") %>%
            pull(estimate)
          
          total_renter_val <- home_ownership_race_alb_2012 %>%
            filter(variable == "total_renter") %>%
            pull(estimate)
          
          # Filter out the total rows and create new columns
          home_ownership_race_alb_2012_processed <- home_ownership_race_alb_2012 %>%
            filter(variable != "total_tenure" & variable != "total_homeowner" & variable != "total_renter") %>%
            mutate(total_tenure = total_tenure_val,
                   total_homeowner = total_homeowner_val,
                   total_renter = total_renter_val)
          
          # Calculate percentages (unfinished) (!)    
          
  
# Table: B19013 (Median Household Income) - By Tract

# Charlottesville (Median Household Income), 2022

median_household_income_cville_2022 <- get_acs(
  geography = "tract",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B19013",
  survey = "acs5",
  year = 2022)

# Finalizing table

cville_median_household_income_2022 <- median_household_income_cville_2022 %>% 
  mutate(year = "2022",
         label = case_when(
           variable == "B19013_001" ~ "Median Household Income in Past 12 Months (Inflation Adjusted)"
         ))

# Generating CSV:

write.csv(cville_median_household_income_2022, "temp_data/cville_median_household_income_2022.csv")

# Albemarle (Median Household Income), 2022

median_household_income_alb_2022 <- get_acs(
  geography = "tract",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B19013",
  survey = "acs5",
  year = 2022)

# Finalizing table

alb_median_household_income_2022 <- median_household_income_alb_2022 %>% 
  mutate(year = "2022",
         label = case_when(
           variable == "B19013_001" ~ "Median Household Income in Past 12 Months (Inflation Adjusted)"
         ))

# Generating CSV:

write.csv(alb_median_household_income_2022, "temp_data/alb_median_household_income_2022.csv")

# Median Household Income by Race Tables: B19013A-B19013I, 2022

# Note: two methods attempted
  # Method one: lines 2088-2405
  # Method two: lines 2407-2469

# Long approach (manually pulling each table):

        # Table: B19013A (Median Household Income by Race, White Alone), 2022
        
        # Charlottesville (Median Household Income by Race, White Alone), 2022
        median_white_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013A",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_white_household_income_2022 <- median_white_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "White Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, White Alone), 2022
        median_white_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013A",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_white_household_income_2022 <- median_white_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "White Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013B (Median Household Income by Race, Black or African American Alone), 2022
        
        # Charlottesville (Median Household Income by Race, Black or African American Alone), 2022
        median_black_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013B",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_black_household_income_2022 <- median_black_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "Black or African American Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, Black or African American Alone), 2022
        median_black_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013B",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_black_household_income_2022 <- median_black_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "Black or African American Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013C (Median Household Income by Race, American Indian and Alaska Native Alone), 2022
        
        # Charlottesville (Median Household Income by Race, American Indian and Alaska Native Alone), 2022
        median_native_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013C",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_native_household_income_2022 <- median_native_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "American Indian and Alaska Native Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, American Indian and Alaska Native Alone), 2022
        median_native_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013C",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_native_household_income_2022 <- median_native_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "American Indian and Alaska Native Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013D (Median Household Income by Race, Asian Alone), 2022
        
        # Charlottesville (Median Household Income by Race, Asian Alone), 2022
        median_asian_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013D",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_asian_household_income_2022 <- median_asian_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "Asian Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, Asian Alone), 2022
        median_asian_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013D",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_asian_household_income_2022 <- median_asian_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "Asian Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013E (Median Household Income by Race, Native Hawaiian and Other Pacific Islander Alone), 2022
        
        # Charlottesville (Median Household Income by Race, Native Hawaiian and Other Pacific Islander Alone), 2022
        median_pacific_islander_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013E",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_pacific_islander_household_income_2022 <- median_pacific_islander_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "Native Hawaiian and Other Pacific Islander Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, Native Hawaiian and Other Pacific Islander Alone), 2022
        median_pacific_islander_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013E",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_pacific_islander_household_income_2022 <- median_pacific_islander_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "Native Hawaiian and Other Pacific Islander Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013F (Median Household Income by Race, Some Other Race Alone), 2022
        
        # Charlottesville (Median Household Income by Race, Some Other Race Alone), 2022
        median_other_race_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013F",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_other_race_household_income_2022 <- median_other_race_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "Some Other Race Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, Some Other Race Alone), 2022
        median_other_race_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013F",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_other_race_household_income_2022 <- median_other_race_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "Some Other Race Alone", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013G (Median Household Income by Race, Two or More Races), 2022
        
        # Charlottesville (Median Household Income by Race, Two or More Races Alone), 2022
        median_two_or_more_races_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013G",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_two_or_more_races_household_income_2022 <- median_two_or_more_races_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "Two or More Races", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, Two or More Races Alone), 2022
        median_two_or_more_races_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013G",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_two_or_more_races_household_income_2022 <- median_two_or_more_races_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "Two or More Races", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013H (Median Household Income by Race, White Alone, Not Hispanic or Latino), 2022
        
        # Charlottesville (Median Household Income by Race, White Alone, Not Hispanic or Latino Alone), 2022
        median_white_not_hispanic_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013H",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_white_not_hispanic_household_income_2022 <- median_white_not_hispanic_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "White Alone, Not Hispanic or Latino", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, White Alone, Not Hispanic or Latino Alone), 2022
        median_white_not_hispanic_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013H",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_white_not_hispanic_household_income_2022 <- median_white_not_hispanic_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "White Alone, Not Hispanic or Latino", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Table: B19013I (Median Household Income by Race, Hispanic or Latino), 2022
        
        # Charlottesville (Median Household Income by Race, Hispanic or Latino), 2022
        median_hispanic_household_income_cville_2022 <- get_acs(
          geography = "county",
          county = "540",
          cache = TRUE,
          state = "VA",
          table = "B19013I",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        cville_median_hispanic_household_income_2022 <- median_hispanic_household_income_cville_2022 %>% 
          mutate(year = "2022", race = "Hispanic or Latino", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Albemarle (Median Household Income by Race, Hispanic or Latino), 2022
        median_hispanic_household_income_alb_2022 <- get_acs(
          geography = "county",
          county = "003",
          cache = TRUE,
          state = "VA",
          table = "B19013I",
          survey = "acs5",
          year = 2022
        )
        
        # Finalizing table
        alb_median_hispanic_household_income_2022 <- median_hispanic_household_income_alb_2022 %>% 
          mutate(year = "2022", race = "Hispanic or Latino", label = "Median Household Income in Past 12 Months (Inflation Adjusted)")
        
        # Combining data (Charlottesville)
        
        cville_median_household_income_race_2022 <- bind_rows(
          cville_median_white_household_income_2022,
          cville_median_black_household_income_2022,
          cville_median_native_household_income_2022,
          cville_median_asian_household_income_2022,
          cville_median_pacific_islander_household_income_2022,
          cville_median_other_race_household_income_2022,
          cville_median_two_or_more_races_household_income_2022,
          cville_median_white_not_hispanic_household_income_2022,
          cville_median_hispanic_household_income_2022
        )
        
        # Combining data (Albemarle)
        
        alb_median_household_income_race_2022 <- bind_rows(
          alb_median_white_household_income_2022,
          alb_median_black_household_income_2022,
          alb_median_native_household_income_2022,
          alb_median_asian_household_income_2022,
          alb_median_pacific_islander_household_income_2022,
          alb_median_other_race_household_income_2022,
          alb_median_two_or_more_races_household_income_2022,
          alb_median_white_not_hispanic_household_income_2022,
          alb_median_hispanic_household_income_2022
        )
    
# Alternate method (fewer lines of code)
        
        vars <- c("B19013A_001", # Median Household Income by Race, White Alone
                  "B19013B_001", # Median Household Income by Race, Black or African American Alone
                  "B19013C_001", # Median Household Income by Race, American Indian and Alaska Native Alone
                  "B19013D_001", # Median Household Income by Race, Asian Alone
                  "B19013E_001", # Median Household Income by Race, Native Hawaiian and Other Pacific Islander Alone
                  "B19013F_001", # Median Household Income by Race, Some Other Race Alone
                  "B19013G_001", # Median Household Income by Race, Two or More Races
                  "B19013H_001", # Median Household Income by Race, White Alone, Not Hispanic or Latino
                  "B19013I_001") # Median Household Income by Race, Hispanic or Latino
        
        # Charlottesville, 2022
        
        median_income_cville_2022 <- get_acs(geography = "county",
                                              variable = vars,
                                              county = "540",
                                              cache = TRUE,
                                              state = "VA",
                                              survey = "acs5",
                                              year = 2022,
                                              output = "wide")
        
        # Finalize
        median_income_cville_2022 <- median_income_cville_2022 %>%
          select(-ends_with("M")) %>% 
          rename(
            median_household_income_white_alone = B19013A_001E,
            median_household_income_black_alone = B19013B_001E,
            median_household_income_native_alone = B19013C_001E,
            median_household_income_asian_alone = B19013D_001E,
            median_household_income_pacific_islander_alone = B19013E_001E,
            median_household_income_some_other_race_alone = B19013F_001E,
            median_household_income_two_or_more_races = B19013G_001E,
            median_household_income_white_alone_not_hispanic_latino = B19013H_001E,
            median_household_income_hispanic_latino = B19013I_001E
            )
        
        # Albemarle, 2022
        
        median_income_alb_2022 <- get_acs(geography = "county",
                                             variable = vars,
                                             county = "003",
                                             cache = TRUE,
                                             state = "VA",
                                             survey = "acs5",
                                             year = 2022,
                                             output = "wide")
        
        # Finalize
        median_income_alb_2022 <- median_income_alb_2022 %>%
          select(-ends_with("M")) %>% 
          rename(
            median_household_income_white_alone = B19013A_001E,
            median_household_income_black_alone = B19013B_001E,
            median_household_income_native_alone = B19013C_001E,
            median_household_income_asian_alone = B19013D_001E,
            median_household_income_pacific_islander_alone = B19013E_001E,
            median_household_income_some_other_race_alone = B19013F_001E,
            median_household_income_two_or_more_races = B19013G_001E,
            median_household_income_white_alone_not_hispanic_latino = B19013H_001E,
            median_household_income_hispanic_latino = B19013I_001E
          )
        
# ALICE Data (2010-2022)
# url: https://www.unitedforalice.org/Attachments/StateDataSheet/2024%20ALICE%20-%20Virginia%20Data%20Sheet.xlsx
# destfile <- "temp_data/VA_ALICE_2024.xlsx"

# Charlottesville (ALICE Households), 2010-2022

sheets <- excel_sheets("temp_data/VA_ALICE_2024.xlsx")

alice_va <- read_excel("temp_data/VA_ALICE_2024.xlsx", sheet = sheets[2]) %>%
  rename_with(~ tolower(str_replace_all(.x, ":|-| ", "_") )
  ) %>%
  filter(geo.id2 == "51540")

alice_cville <- alice_va %>%
  select(year, households, poverty_households, alice_households, above_alice_households) %>%
  gather(level, number, -year, -households) %>%
  mutate(pct = number/households*100 )

# Generating CSV:

write.csv(alice_cville, "temp_data/cville_alice_2010_2022.csv")

# Albemarle (ALICE Households), 2010-2022

sheets <- excel_sheets("temp_data/VA_ALICE_2024.xlsx")

alice_va <- read_excel("temp_data/VA_ALICE_2024.xlsx", sheet = sheets[2]) %>%
  rename_with(~ tolower(str_replace_all(.x, ":|-| ", "_") )
  ) %>%
  filter(geo.id2 == "51003")

alice_alb <- alice_va %>%
  select(year, households, poverty_households, alice_households, above_alice_households) %>%
  gather(level, number, -year, -households) %>%
  mutate(pct = number/households*100 )

# Generating CSV:

write.csv(alice_alb, "temp_data/alb_alice_2010_2022.csv")
