# R Script for pulling and examining health data
# Author: Henry DeMarco
# Date Created: June 17, 2024
# Last Updated: July 23, 2024

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

#(!) indicates temporary comments to remove

#Packages
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(readxl)

# Census API Key
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

# Creating basic objects

# Year for all data pull
year <- 2022

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

# Topics of interest (!)

  # Health Insurance Coverage (by race / tract)
  # Rate of food insecurity in county (overall / among children), 2022
  # Number / percent without health insurance (by race / ethnicity, tract)
  # CDC Places Measures (health conditions by tract)
  # Food Insecurity (by county)
  # Life expectancy
     # By race / ethnicity

############################

# Table: S2701 (Health Insurance Coverage by Race / Ethnicity)
  # S2701_C01 is total, C02 is insured, C03 is percent insured, C04 is uninsured, C05 is percent uninsured
  # S2701_C01_016 through S2701_C01_024 is coverage by race / ethnicity

# Charlottesville (Insurance by Race / Ethnicity), 2022

insurance_counts_cville_2022 <- get_acs(
  geography = "county",
  state = "VA",
  county = "540",
  table = "S2701",
  survey = "acs5", 
  cache = TRUE,
  year = 2022) %>% 
  filter(variable %in% c("S2701_C04_016", "S2701_C04_017", "S2701_C04_018", "S2701_C04_019",
                         "S2701_C04_020", "S2701_C04_021", "S2701_C04_022", "S2701_C04_023",
                         "S2701_C04_024")
  )

# Finalizing table:
cville_insurance_counts_2022 <- insurance_counts_cville_2022 %>% 
  mutate(year = "2022",
    label = case_when(
    variable == "S2701_C04_016" ~ "White alone",
    variable == "S2701_C04_017" ~ "Black or African American alone",
    variable == "S2701_C04_018" ~ "American Indian and Alaska Native alone",
    variable == "S2701_C04_019" ~ "Asian alone",
    variable == "S2701_C04_020" ~ "Native Hawaiian and Other Pacific Islander alone",
    variable == "S2701_C04_021" ~ "Some other race alone",
    variable == "S2701_C04_022" ~ "Two or more races",
    variable == "S2701_C04_023" ~ "Hispanic or Latino (of any race)",
    variable == "S2701_C04_024" ~ "White alone, not Hispanic or Latino")) %>% 
  rename(uninsured_count = estimate) %>% 
  mutate(shared_variable = gsub("C04", "", variable))

# Percents are calculated by race/ethnicity, so percent table is pulled to join with count table

insurance_per_cville_2022 <- get_acs(
           geography = "county",
           state = "VA",
           county = "540",
           table = "S2701",
           survey = "acs5", 
           cache = TRUE,
           year = 2022) %>% 
           filter(variable %in% c("S2701_C05_016", "S2701_C05_017", "S2701_C05_018", "S2701_C05_019",
                         "S2701_C05_020", "S2701_C05_021", "S2701_C05_022", "S2701_C05_023",
                         "S2701_C05_024")
           )

# Finalizing table:
cville_insurance_per_2022 <- insurance_per_cville_2022 %>% 
  mutate(year = "2022",
           label = case_when(
           variable == "S2701_C05_016" ~ "White alone",
           variable == "S2701_C05_017" ~ "Black or African American alone",
           variable == "S2701_C05_018" ~ "American Indian and Alaska Native alone",
           variable == "S2701_C05_019" ~ "Asian alone",
           variable == "S2701_C05_020" ~ "Native Hawaiian and Other Pacific Islander alone",
           variable == "S2701_C05_021" ~ "Some other race alone",
           variable == "S2701_C05_022" ~ "Two or more races",
           variable == "S2701_C05_023" ~ "Hispanic or Latino (of any race)",
           variable == "S2701_C05_024" ~ "White alone, not Hispanic or Latino")) %>% 
  rename(percent_uninsured = estimate) %>% 
  mutate(shared_variable = gsub("C05", "", variable))

# Joining count table with percent table

cville_insurance_final_2022 <- left_join(cville_insurance_counts_2022, 
                                         cville_insurance_per_2022 %>% select(shared_variable, percent_uninsured), 
                                         by = "shared_variable")

# Generating CSV

write.csv(cville_insurance_final_2022, "temp_data/cville_insurance_final_2022.csv")

# Albemarle (Insurance by Race / Ethnicity), 2022

insurance_counts_alb_2022 <- get_acs(
  geography = "county",
  state = "VA",
  county = "003",
  table = "S2701",
  survey = "acs5", 
  cache = TRUE,
  year = 2022) %>% 
  filter(variable %in% c("S2701_C04_016", "S2701_C04_017", "S2701_C04_018", "S2701_C04_019",
                         "S2701_C04_020", "S2701_C04_021", "S2701_C04_022", "S2701_C04_023",
                         "S2701_C04_024")
  )

# Finalizing table:
alb_insurance_counts_2022 <- insurance_counts_alb_2022 %>% 
  mutate(year = "2022",
    label = case_when(
    variable == "S2701_C04_016" ~ "White alone",
    variable == "S2701_C04_017" ~ "Black or African American alone",
    variable == "S2701_C04_018" ~ "American Indian and Alaska Native alone",
    variable == "S2701_C04_019" ~ "Asian alone",
    variable == "S2701_C04_020" ~ "Native Hawaiian and Other Pacific Islander alone",
    variable == "S2701_C04_021" ~ "Some other race alone",
    variable == "S2701_C04_022" ~ "Two or more races",
    variable == "S2701_C04_023" ~ "Hispanic or Latino (of any race)",
    variable == "S2701_C04_024" ~ "White alone, not Hispanic or Latino")) %>% 
  rename(uninsured_count = estimate) %>% 
  mutate(shared_variable = gsub("C04", "", variable))

# Percents are calculated by race/ethnicity, so percent table is pulled to join with count table

insurance_per_alb_2022 <- get_acs(
  geography = "county",
  state = "VA",
  county = "003",
  table = "S2701",
  survey = "acs5", 
  cache = TRUE,
  year = 2022) %>% 
  filter(variable %in% c("S2701_C05_016", "S2701_C05_017", "S2701_C05_018", "S2701_C05_019",
                         "S2701_C05_020", "S2701_C05_021", "S2701_C05_022", "S2701_C05_023",
                         "S2701_C05_024")
  )

# Finalizing table:
alb_insurance_per_2022 <- insurance_per_alb_2022 %>% 
  mutate(year = "2022",
    label = case_when(
    variable == "S2701_C05_016" ~ "White alone",
    variable == "S2701_C05_017" ~ "Black or African American alone",
    variable == "S2701_C05_018" ~ "American Indian and Alaska Native alone",
    variable == "S2701_C05_019" ~ "Asian alone",
    variable == "S2701_C05_020" ~ "Native Hawaiian and Other Pacific Islander alone",
    variable == "S2701_C05_021" ~ "Some other race alone",
    variable == "S2701_C05_022" ~ "Two or more races",
    variable == "S2701_C05_023" ~ "Hispanic or Latino (of any race)",
    variable == "S2701_C05_024" ~ "White alone, not Hispanic or Latino"))  %>% 
  rename(percent_uninsured = estimate) %>% 
  mutate(shared_variable = gsub("C05", "", variable))

# Joining count table with percent table

alb_insurance_final_2022 <- left_join(alb_insurance_counts_2022, 
                                         alb_insurance_per_2022 %>% select(shared_variable, percent_uninsured), 
                                         by = "shared_variable")

# Generating CSV

write.csv(alb_insurance_final_2022, "temp_data/alb_insurance_final_2022.csv")

# Table: S2701 (Health Insurance Coverage by Tract)

# Charlottesville (Health Insurance Coverage by Tract), 2022

insurance_tract_cville_2022 <- get_acs(
  geography = "tract",
  state = "VA",
  county = "540",
  table = "S2701",
  survey = "acs5", 
  cache = TRUE,
  summary_var = "S2701_C01_001",
  year = 2022) %>% 
  filter(variable %in% c("S2701_C04_001")
  )

# Finalizing table:
cville_insurance_tract_2022 <- insurance_tract_cville_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
         variable == "S2701_C04_001" ~ "Uninsured"
         ))

# Generating CSV

write.csv(cville_insurance_tract_2022, "temp_data/cville_insurance_tract_2022.csv")

# Albemarle (Health Insurance Coverage by Tract), 2022

insurance_tract_alb_2022 <- get_acs(
  geography = "tract",
  state = "VA",
  county = "003",
  table = "S2701",
  survey = "acs5", 
  cache = TRUE,
  summary_var = "S2701_C01_001",
  year = 2022) %>% 
  filter(variable %in% c("S2701_C04_001")
  )

# Finalizing table:
alb_insurance_tract_2022 <- insurance_tract_alb_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "S2701_C04_001" ~ "Uninsured"
         ))

# Generating CSV

write.csv(alb_insurance_tract_2022, "temp_data/alb_insurance_tract_2022.csv")

# define api endpoint with query for locality (limit is 1000; each locality alone is below this)
api_alb <- "https://data.cdc.gov/resource/cwsq-ngmh.json/?StateAbbr=VA&countyname=Albemarle"
api_cvl <- "https://data.cdc.gov/resource/cwsq-ngmh.json/?StateAbbr=VA&countyname=Charlottesville"
# technically one is supposed to get an app token and include it in the parameter url
# but you can make a limited number of requests without an app token, 
# and we should only need to do this once.
# more on the api here: https://dev.socrata.com/foundry/data.cdc.gov/cwsq-ngmh

# read in data
df_alb <- fromJSON(api_alb)
df_cvl <- fromJSON(api_cvl)

# bind rows
df <- bind_rows(df_alb, df_cvl)

# Breaking up into screenings versus prevalence

cdc_outcomes <- df %>% 
  filter(measureid %in% c("DIABETES", "OBESITY", "CHD", "BPHIGH", "DEPRESSION", 
                          "CASTHMA", "COPD", "CANCER", "STROKE",
                          "HIGHCHOL", "KIDNEY"
                          ))

# Selecting columns of interest

cdc_outcomes <- cdc_outcomes %>% 
  select("year", "stateabbr", "countyname", "countyfips", "locationname", "measure", "category", "measureid", "data_value", "totalpopulation", "short_question_text")


# Generating CSV

write.csv(cdc_outcomes, "temp_data/cdc_outcomes.csv")

cdc_prevention <- df %>% 
  filter(measureid %in% c("CHECKUP", "DENTAL", "CERVICAL", "COLON_SCREEN",
                          "MAMMOUSE", "MHLTH", "PHLTH"
                          ))

# Selecting columns of interest

cdc_prevention <- cdc_prevention %>% 
  select("year", "stateabbr", "countyname", "countyfips", "locationname", "measure", "category", "measureid", "data_value", "totalpopulation", "short_question_text")

# Generating CSV

write.csv(cdc_prevention, "temp_data/cdc_prevention.csv")

# Food Insecurity Data
# url: https://map.feedingamerica.org (data was requested)
# destfile <- temp_data/meal_gap_cville_alb_2022.csv"

meal_gap_cville_alb_2022 <- read.csv("temp_data/meal_gap_cville_alb_2022.csv")

# Meal Gap, Charlottesville (2022)

cville_meal_gap_2022 <- meal_gap_cville_alb_2022 %>% 
  filter(County..State == "Charlottesville city, Virginia")

# Meal Gap, Albemarle (2022)

alb_meal_gap_2022 <- meal_gap_cville_alb_2022 %>% 
  filter(County..State == "Albemarle County, Virginia")

# Life Expectancy Data
# url: https://www.countyhealthrankings.org/health-data/virginia/data-and-resources
# destfile <- temp_data/county_health_va.xlsx"

va_life_expec <- read_xlsx("temp_data/county_health_va.xlsx", sheet = "Additional Measure Data", skip = 1)

# Filtering for Albemarle, Charlottesville

alb_cville_life_expec <- va_life_expec %>%
  filter(County %in% c("Albemarle", "Charlottesville City"))

write.csv(alb_cville_life_expec, "temp_data/alb_cville_life_expec.csv")


