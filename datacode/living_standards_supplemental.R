# R Script for pulling and examining supplemental living standards data
# Author: Henry DeMarco
# Date Created: October 24, 2024
# Last Updated: October 29, 2024

# Description: this analysis is intended to help answer questions about living standards data in Cville/Albemarle tracts with large UVA student populations.

## ACS Tables Used:
# B01001 (Sex by Age)
# B01001B (Sex by Age, Black Alone)
# B01001A (Sex by Age, White Alone)
# B22003 (Receipt of Food Stamps/SNAP in the Past 12 Months by Poverty Status)

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

# Load packages
library(tidyverse)
library(tidycensus)

# Census API Key
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

# Creating basic objects ----

# Year for ACS data (single year)
year <- 2022

# County FIPS codes 
county_codes <- c("003", "540") # Albemarle, Charlottesville FIPS Code

# Name for combined region
region_name <- "Charlottesville-Albemarle Region"

# Read in tract names
tract_names <- read_csv("data/regional_tractnames.csv")
tract_names <- tract_names %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  rename("county" = "locality",
         "tract_num" = "tract") %>% 
  filter(locality_num %in% county_codes)

# ACS Variables ----
# A custom R function that creates a table of variable codes and metadata 
# for ACS 5-year, including subject and profile tables
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
# View(meta_table)

# Get GEOID for all Census tracts in Charlottesville and Albemarle
tracts_cville_albemarle <- get_acs(
  geography = "tract",
  variables = "B01001_001",  # Total population (any variable works, we just need GEOID)
  state = "VA",
  county = c("Charlottesville", "Albemarle"),
  survey = "acs5",
  year = 2022
) %>%
  select(GEOID, NAME)  # Select GEOID and NAME (which contains tract info)

# View the result
tracts_cville_albemarle

## ............................................
# Sex by Age: Table: B01001, 2022 ----

# Get Sex by Age table for Charlottesville and Albermale, 2022
vars_B01001_tract <- 
  map_df(c(2022),
         ~ get_acs(
           year = .x,
           geography = "tract",
           state = "VA",
           county = county_codes,
           table = "B01001",
           summary_var = "B01001_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x)
  )

# Specific tracts of interest: 
  # Cville: 2.01, 2.02, 6, 7; GEOID Equivalents: 51540000201, 51540000202, 51540000600,51540000700 
  # Albemarle: 108.01, 109.04, 109.01; GEOID Equivalents: 51003010801, 51003010904, 51003010901
  # Initial ACS pull is filtered to only include these tracts

# Mapping male and female variables
# Age groups align with final grouping, not original acs vars
age_groups <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 to 24 years", "18 to 24 years", "18 to 24 years", "18 to 24 years", 
                "25 to 34 years", "25 to 34 years", "35 to 44 years", "35 to 44 years", 
                "45 to 54 years", "45 to 54 years", "55 to 59 years", "60 to 64 years", 
                "60 to 64 years", "65 to 74 years", "65 to 74 years", "65 to 74 years", 
                "75 to 84 years", "75 to 84 years", "85 years and over"),
  
  male_var = c("B01001_003", "B01001_004", "B01001_005", "B01001_006", 
               "B01001_007", "B01001_008", "B01001_009", "B01001_010", 
               "B01001_011", "B01001_012", "B01001_013", "B01001_014", 
               "B01001_015", "B01001_016", "B01001_017", "B01001_018", 
               "B01001_019", "B01001_020", "B01001_021", "B01001_022", 
               "B01001_023", "B01001_024", "B01001_025"),
  
  female_var = c("B01001_027", "B01001_028", "B01001_029", "B01001_030", 
                 "B01001_031", "B01001_032", "B01001_033", "B01001_034", 
                 "B01001_035", "B01001_036", "B01001_037", "B01001_038", 
                 "B01001_039", "B01001_040", "B01001_041", "B01001_042", 
                 "B01001_043", "B01001_044", "B01001_045", "B01001_046", 
                 "B01001_047", "B01001_048", "B01001_049")
)


sex_ages_tract <- age_groups %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    vars_B01001_tract %>%
      filter(variable %in% c(male_var, female_var))  %>%
      group_by(variable, GEOID, NAME, year) %>%
      mutate(age_group = age_group,
             sex = case_when(variable %in% c(male_var) ~ "Male",
                             variable %in% c(female_var) ~ "Female"),
             year = year) %>%
      select(GEOID, NAME, variable, estimate, moe, summary_est, year, sex, age_group)
  })

# Create combined age groups
sex_age_groups_tract <- sex_ages_tract %>%
  group_by(GEOID, NAME, year, sex, age_group) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe = moe, estimate = estimate),
    summary_est = first(summary_est),
    .groups = 'drop')

# Calculating 'college aged population' in each tract (aged 18-24):

# Combine male and female populations by age group
total_age_groups_tract <- sex_age_groups_tract %>%
  group_by(GEOID, NAME, year, age_group) %>%
  summarize(
    total_estimate = sum(estimate),          # Combine male and female estimates
    total_moe = moe_sum(moe = moe, estimate = estimate),  # Combine margins of error
    summary_est = first(summary_est),        # Total population of the tract (same for male and female)
    .groups = 'drop'
  ) %>% 
  mutate(percentage = (total_estimate / summary_est) * 100,
         percent_round = round(percentage,0))

# Join tract names
total_age_groups_tract <- total_age_groups_tract %>% 
  left_join(tract_names)

# Calculate the percentage that college age groups represent of total tract populations
college_age_groups_tract <- total_age_groups_tract %>%
  filter(age_group %in% c("18 to 24 years"))

# Same for county level
vars_B01001_county <- 
  map_df(c(2022),
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = county_codes,
           table = "B01001",
           summary_var = "B01001_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x)
  )

# Mapping male and female variables
# Age groups align with final grouping, not original acs vars
age_groups <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 to 24 years", "18 to 24 years", "18 to 24 years", "18 to 24 years", 
                "25 to 34 years", "25 to 34 years", "35 to 44 years", "35 to 44 years", 
                "45 to 54 years", "45 to 54 years", "55 to 59 years", "60 to 64 years", 
                "60 to 64 years", "65 to 74 years", "65 to 74 years", "65 to 74 years", 
                "75 to 84 years", "75 to 84 years", "85 years and over"),
  
  male_var = c("B01001_003", "B01001_004", "B01001_005", "B01001_006", 
               "B01001_007", "B01001_008", "B01001_009", "B01001_010", 
               "B01001_011", "B01001_012", "B01001_013", "B01001_014", 
               "B01001_015", "B01001_016", "B01001_017", "B01001_018", 
               "B01001_019", "B01001_020", "B01001_021", "B01001_022", 
               "B01001_023", "B01001_024", "B01001_025"),
  
  female_var = c("B01001_027", "B01001_028", "B01001_029", "B01001_030", 
                 "B01001_031", "B01001_032", "B01001_033", "B01001_034", 
                 "B01001_035", "B01001_036", "B01001_037", "B01001_038", 
                 "B01001_039", "B01001_040", "B01001_041", "B01001_042", 
                 "B01001_043", "B01001_044", "B01001_045", "B01001_046", 
                 "B01001_047", "B01001_048", "B01001_049")
)


sex_ages_county <- age_groups %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    vars_B01001_county %>%
      filter(variable %in% c(male_var, female_var))  %>%
      group_by(variable, GEOID, NAME, year) %>%
      mutate(age_group = age_group,
             sex = case_when(variable %in% c(male_var) ~ "Male",
                             variable %in% c(female_var) ~ "Female"),
             year = year) %>%
      select(GEOID, NAME, variable, estimate, moe, summary_est, year, sex, age_group)
  })

# Create combined age groups
sex_age_groups_county <- sex_ages_county %>%
  group_by(GEOID, NAME, year, sex, age_group) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe = moe, estimate = estimate),
    summary_est = first(summary_est),
    .groups = 'drop')

# Calculating 'college aged population' in each tract (aged 18-24):

# Combine male and female populations by age group
total_age_groups_county <- sex_age_groups_county %>%
  group_by(GEOID, NAME, year, age_group) %>%
  summarize(
    total_estimate = sum(estimate),          # Combine male and female estimates
    total_moe = moe_sum(moe = moe, estimate = estimate),  # Combine margins of error
    summary_est = first(summary_est),        # Total population of the tract (same for male and female)
    .groups = 'drop'
  ) %>% 
  mutate(percentage = (total_estimate / summary_est) * 100,
         percent_round = round(percentage,0))

# Calculate the percentage that college age groups represent of total tract populations
college_age_groups_county <- total_age_groups_county %>%
  filter(age_group %in% c("18 to 24 years"))

## .........
# Families
# Household Type
acs_B11001_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  table = "B11001",
  year = year, 
  survey = "acs5")


## ............................................
# Sex by Age (Black or African American Alone): Table: B01001B, 2022 ----

# Get table for Charlottesville and Albermale, 2022
vars_B01001B <- 
  map_df(c(2022),
         ~ get_acs(
           year = .x,
           geography = "tract",
           state = "VA",
           county = county_codes,
           table = "B01001B",
           summary_var = "B01001B_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x) %>% 
           filter(GEOID %in% c("51540000201", "51540000202", "51540000600", 
                               "51540000700", "51003010801", "51003010904", 
                               "51003010901"))
  )

# Mapping male and female variables
# Age groups align with final grouping, not original acs vars
age_groups2 <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 to 24 years", "18 to 24 years", 
                "25 to 34 years", "25 to 34 years", "35 to 44 years", 
                "45 to 54 years", "55 to 64 years",
                "65 to 74 years",
                "75 to 84 years", "85 years and over"),
  
  male_var = c("B01001B_003", "B01001B_004", "B01001B_005", "B01001B_006", 
               "B01001B_007", "B01001B_008", "B01001B_009", "B01001B_010", 
               "B01001B_011", "B01001B_012", "B01001B_013", "B01001B_014", 
               "B01001B_015", "B01001B_016"),
  
  female_var = c("B01001B_018", "B01001B_019", "B01001B_020", "B01001B_021", 
                 "B01001B_022", "B01001B_023", "B01001B_024", "B01001B_025", 
                 "B01001B_026", "B01001B_027", "B01001B_028", "B01001B_029", 
                 "B01001B_030", "B01001B_031")
)


sex_ages_2_df <- age_groups2 %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    vars_B01001B %>%
      filter(variable %in% c(male_var, female_var))  %>%
      group_by(variable, GEOID, NAME, year) %>%
      mutate(age_group = age_group,
             sex = case_when(variable %in% c(male_var) ~ "Male",
                             variable %in% c(female_var) ~ "Female"),
             year = year) %>%
      select(GEOID, NAME, variable, estimate, moe, summary_est, year, sex, age_group)
  })

# Create combined age groups
sex_age_groups_2_df <- sex_ages_2_df %>%
  group_by(GEOID, NAME, year, sex, age_group) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe = moe, estimate = estimate),
    summary_est = first(summary_est),
    .groups = 'drop')

# Calculating 'college aged population' in each tract (aged 18-24):

# Combine male and female populations by age group
total_age_groups_2_df <- sex_age_groups_2_df %>%
  group_by(GEOID, NAME, year, age_group) %>%
  summarize(
    total_estimate = sum(estimate),          # Combine male and female estimates
    total_moe = moe_sum(moe = moe, estimate = estimate),  # Combine margins of error
    summary_est = first(summary_est),        # Total population of the tract (same for male and female)
    .groups = 'drop'
  )

# Calculate the percentage that college age groups represent of total tract populations
college_age_groups_percentage_B01001B <- total_age_groups_2_df %>%
  mutate(percentage = (total_estimate / summary_est) * 100) %>% 
  filter(age_group %in% c("18 to 24 years"))


## ............................................
# Sex by Age (White Alone): Table: B01001A, 2022 ----

# Get table for Charlottesville and Albermale, 2022
vars_B01001A <- 
  map_df(c(2022),
         ~ get_acs(
           year = .x,
           geography = "tract",
           state = "VA",
           county = county_codes,
           table = "B01001A",
           summary_var = "B01001A_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x) %>% 
           filter(GEOID %in% c("51540000201", "51540000202", "51540000600", 
                               "51540000700", "51003010801", "51003010904", 
                               "51003010901"))
  )


# Mapping male and female variables
# Age groups align with final grouping, not original acs vars
age_groups3 <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 to 24 years", "18 to 24 years", 
                "25 to 34 years", "25 to 34 years", "35 to 44 years", 
                "45 to 54 years", "55 to 64 years",
                "65 to 74 years",
                "75 to 84 years", "85 years and over"),
  
  male_var = c("B01001A_003", "B01001A_004", "B01001A_005", "B01001A_006", 
               "B01001A_007", "B01001A_008", "B01001A_009", "B01001A_010", 
               "B01001A_011", "B01001A_012", "B01001A_013", "B01001A_014", 
               "B01001A_015", "B01001A_016"),
  
  female_var = c("B01001A_018", "B01001A_019", "B01001A_020", "B01001A_021", 
                 "B01001A_022", "B01001A_023", "B01001A_024", "B01001A_025", 
                 "B01001A_026", "B01001A_027", "B01001A_028", "B01001A_029", 
                 "B01001A_030", "B01001A_031")
)


sex_ages_3_df <- age_groups3 %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    vars_B01001A %>%
      filter(variable %in% c(male_var, female_var))  %>%
      group_by(variable, GEOID, NAME, year) %>%
      mutate(age_group = age_group,
             sex = case_when(variable %in% c(male_var) ~ "Male",
                             variable %in% c(female_var) ~ "Female"),
             year = year) %>%
      select(GEOID, NAME, variable, estimate, moe, summary_est, year, sex, age_group)
  })

# Create combined age groups
sex_age_groups_3_df <- sex_ages_3_df %>%
  group_by(GEOID, NAME, year, sex, age_group) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe = moe, estimate = estimate),
    summary_est = first(summary_est),
    .groups = 'drop')

# Calculating 'college aged population' in each tract (aged 18-24):

# Combine male and female populations by age group
total_age_groups_3_df <- sex_age_groups_3_df %>%
  group_by(GEOID, NAME, year, age_group) %>%
  summarize(
    total_estimate = sum(estimate),          # Combine male and female estimates
    total_moe = moe_sum(moe = moe, estimate = estimate),  # Combine margins of error
    summary_est = first(summary_est),        # Total population of the tract (same for male and female)
    .groups = 'drop'
  )

# Calculate the percentage that college age groups represent of total tract populations
college_age_groups_percentage_B01001A <- total_age_groups_3_df %>%
  mutate(percentage = (total_estimate / summary_est) * 100) %>% 
  filter(age_group %in% c("18 to 24 years"))

# Note on Tract 2.02 (10th & Page-Venable):
  # About 66% of the overall white population in this tract is 'college-aged' (18-24)
  # About 15% of the overall Black population in this tract is 'college-aged' (18-24)
  # 2.02 Total: 5436 (3416 / 5436): about 63% white
  # 2.02 Total: 5436 (1366 / 5436): about 25% Black

## ............................................
# Receipt of Food Stamps/SNAP in the Past 12 Months by Poverty Status: Table: B22003, 2022 ----

vars_B22003_tract <- 
  map_df(c(2022),
         ~ get_acs(
           year = .x,
           geography = "tract",
           state = "VA",
           county = county_codes,
           table = "B22003",
           summary_var = "B22003_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x)
  )

# Important codes:
# B22003_001: Total
# B22003_002: Household received Food Stamps/SNAP in the past 12 months
# B22003_002: Income in the past 12 months below poverty level
# B22003_004: Income in the past 12 months at or above poverty level
# B22003_005: Household did not receive Food Stamps/SNAP in the past 12 months
# B22003_006: Income in the past 12 months below poverty level
# B22003_007: Income in the past 12 months at or above poverty level

# Define the SNAP and income-level categories, now including the total population
# SNAP groups align with final grouping, not original acs vars
snap_groups <- tibble::tibble(
  snap_group = c("SNAP", "SNAP: below poverty line", "SNAP: at or above poverty line", "No SNAP", 
                 "No SNAP: below poverty line", "No SNAP: at or above poverty line"),
  
  SNAP_var = c("B22003_002", "B22003_003", "B22003_004", "B22003_005", 
               "B22003_006", "B22003_007")
)

snap_groups_tract <- snap_groups %>%
  pmap_dfr(function(snap_group, SNAP_var) {
    vars_B22003_tract %>%
      filter(variable %in% c(SNAP_var))  %>%
      group_by(variable, GEOID, NAME, year) %>%
      mutate(snap_group = snap_group,
             year = year) %>%
      select(GEOID, NAME, variable, estimate, moe, summary_est, year, snap_group)
  })

# Create combined age groups
snap_all_groups_tract <- snap_groups_tract %>%
  group_by(GEOID, NAME, year, snap_group) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe = moe, estimate = estimate),
    summary_est = first(summary_est),
    .groups = 'drop')

# Calculating SNAP percentages

# Calculate the percentage that SNAP recipients represent of total tract populations
snap_groups_percentage_tract <- snap_all_groups_tract %>%
  mutate(percentage = (estimate / summary_est) * 100)

# Join tract names
snap_groups_percentage_tract <- snap_groups_percentage_tract %>% 
  left_join(tract_names)

# Getting SNAP recipient numbers for Charlottesville as a whole (for context)

vars_B22003_county <- 
  map_df(c(2022),
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = county_codes,
           table = "B22003",
           summary_var = "B22003_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x)
  )

# Calculate the percentage that SNAP recipients represent of total county populations
county_snap_percentage_df <- vars_B22003_county %>%
  mutate(percentage = (estimate / summary_est) * 100)

# About 9% of Charlottesville's total population received SNAP benefits in 2022
