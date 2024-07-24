# R Script for pulling and examining access to knowledge data
# Authors: Henry DeMarco, Beth Mitchell
# Date Created: June 17, 2024
# Last Updated: July 17, 2024

# County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

# Access to Knowledge TOC ----
# AHDI MEASURES, COUNTY & TRACT LEVEL
# Educational Attainment, 25 and Over
# - Source: ACS Table S1501
# - High school graduate or higher: S1501_C01_014
# - Bachelor's degree or higher: S1501_C01_015
# - Graduate or professional degree: S1501_C01_013
# School Enrollment, ages 3-24
# - Source: ACS Table S1401
# - School enrollment for the population age 3 to 24: 
#         S1401_C01_014, S1401_C01_016, 
#         S1401_C01_018, S1401_C01_020, 
#         S1401_C01_022, S1401_C01_024
# OTHER MEASURES, COUNTY & TRACT LEVEL
# Educational Attainment by Race/Ethnicity: 
# - Source: ACS Table S1501
# OTHER MEASURES, BY DISTRICT
# Students enrolled in AP classes by race/ethnicity
# - Source: VDOE
# Students suspended by race/ethnicity (Incomplete)
# - Source: https://schoolquality.virginia.gov/download-data
# Students chronically absent by race/ethnicity
# - Source: https://schoolquality.virginia.gov/download-data

# Packages
library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)

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

## ................................................
# Educational Attainment (AHDI MEASURE): S1501 ----

# Educational Attainment: County & tract ----
# Get ACS data
AHDI_vars_S1501 <- c("High school graduate or higher" = "S1501_C01_014",
                     "Bachelor's degree or higher" = "S1501_C01_015",
                     "Graduate or professional degree" = "S1501_C01_013")

acs_S1501_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = AHDI_vars_S1501,
  summary_var = "S1501_C01_006", 
  year = year, 
  survey = "acs5")

acs_S1501_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  var = AHDI_vars_S1501,
  summary_var = "S1501_C01_006", 
  year = year, 
  survey = "acs5")

# Wrangle tables:
edu_attain_county <- acs_S1501_county %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = variable,
         year = year) %>% 
  rename(c("pop_25_over" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, pop_25_over, percent, label, year)

edu_attain_tract <- acs_S1501_tract %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = variable,
         year = year) %>% 
  rename(c("pop_25_over" = "summary_est")) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, estimate, moe, pop_25_over, percent, label, year)

# Join tract names
edu_attain_tract <- edu_attain_tract %>% 
  left_join(tract_names)

# Educational Attainment: Charlottesville, county & tract ----
cville_edu_attain_county <- edu_attain_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_edu_attain_county, paste0("data/cville_edu_attain_county", "_", year, ".csv"))

cville_edu_attain_tract <- edu_attain_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_edu_attain_tract, paste0("data/cville_edu_attain_tract", "_", year, ".csv"))

# Educational Attainment: Albemarle, county & tract ----
alb_edu_attain_county <- edu_attain_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_edu_attain_county, paste0("data/alb_edu_attain_county", "_", year, ".csv"))

alb_edu_attain_tract <- edu_attain_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_edu_attain_tract, paste0("data/alb_edu_attain_tract", "_", year, ".csv"))

# Educational Attainment: Charlottesville, Albemarle Combined Table ----
region_edu_attain <- edu_attain_county %>% 
  group_by(label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            pop_25_over = sum(pop_25_over),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / pop_25_over), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>% 
  select(region_fips, locality, estimate, moe, pop_25_over, percent, label, year)

write_csv(region_edu_attain, paste0("data/region_edu_attain", "_", year, ".csv"))

## ...........................................................
# School Enrollment, Ages 3-24 (AHDI MEASURE): S1401 ----

# School Enrollment: County & tract ----
# Get ACS data
AHDI_vars_S1401 <- c("3 to 4 year olds enrolled in school" = "S1401_C01_014", 
                     "5 to 9 year olds enrolled in school" = "S1401_C01_016",  
                     "10 to 14 year olds enrolled in school" = "S1401_C01_018", 
                     "15 to 17 year olds enrolled in school" = "S1401_C01_020", 
                     "18 and 19 year olds enrolled in school" = "S1401_C01_022", 
                     "20 to 24 year olds enrolled in school" = "S1401_C01_024",
                     "Population 3 to 4 years" = "S1401_C01_013", 
                     "Population 5 to 9 years" = "S1401_C01_015",  
                     "Population 10 to 14 years" = "S1401_C01_017", 
                     "Population 15 to 17 years" = "S1401_C01_019", 
                     "Population 18 and 19 years" = "S1401_C01_021", 
                     "Population 20 to 24 years" = "S1401_C01_023")

acs_S1401_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = AHDI_vars_S1401,
  year = year, 
  survey = "acs5")

acs_S1401_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  var = AHDI_vars_S1401,
  year = year, 
  survey = "acs5")

# Wrangle data
enroll_county <- acs_S1401_county %>% 
  mutate(cat = case_when(str_detect(variable, "enrolled in school") ~ "enrolled",
                         str_detect(variable, "Population") ~ "pop")) %>% 
  group_by(GEOID, NAME, cat) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            .groups = 'drop') %>% 
  pivot_wider(names_from = cat, values_from = c(estimate, moe)) %>% 
  mutate(percent = round(100 * (estimate_enrolled / estimate_pop), digits = 2),
         label = "3 to 24 year olds enrolled in school",
         year = year) %>% 
  rename(locality = NAME,
         estimate = estimate_enrolled,
         moe = moe_enrolled,
         pop_3_to_24yr = estimate_pop) %>% 
  select(GEOID, locality, estimate, moe, pop_3_to_24yr, percent, label, year)

enroll_tract <- acs_S1401_tract %>% 
  mutate(cat = case_when(str_detect(variable, "enrolled in school") ~ "enrolled",
                         str_detect(variable, "Population") ~ "pop")) %>% 
  group_by(GEOID, NAME, cat) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            .groups = 'drop') %>% 
  pivot_wider(names_from = cat, values_from = c(estimate, moe)) %>% 
  mutate(percent = round(100 * (estimate_enrolled / estimate_pop), digits = 2),
         label = "3 to 24 year olds enrolled in school",
         year = year) %>% 
  rename(estimate = estimate_enrolled,
         moe = moe_enrolled,
         pop_3_to_24yr = estimate_pop) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, estimate, moe, pop_3_to_24yr, percent, label, year)

# Join tract names
enroll_tract <- enroll_tract %>% 
  left_join(tract_names)

# School Enrollment: Charlottesville, county & tract ----
cville_enroll_county <- enroll_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_enroll_county, paste0("data/cville_enroll_county", "_", year, ".csv"))

cville_enroll_tract <- enroll_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_enroll_tract, paste0("data/cville_enroll_tract", "_", year, ".csv"))

# School Enrollment: Albemarle, county & tract ----
alb_enroll_county <- enroll_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_enroll_county, paste0("data/alb_enroll_county", "_", year, ".csv"))

alb_enroll_tract <- enroll_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_enroll_tract, paste0("data/alb_enroll_tract", "_", year, ".csv"))

# School Enrollment: Charlottesville, Albemarle Combined Table ----
region_enroll <- enroll_county %>% 
  group_by(label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            pop_3_to_24yr = sum(pop_3_to_24yr),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / pop_3_to_24yr), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>% 
  select(region_fips, locality, estimate, moe, pop_3_to_24yr, percent, label, year)

write_csv(region_enroll, paste0("data/region_enroll", "_", year, ".csv"))

## .....................................................
# Educational Attainment by Race/Ethnicity: S1501 ----

# Educational Attainment by Race/Ethnicity: County & tract

vars_S1501_race <- c("White total" = "S1501_C01_028",
                "White high_school_up" = "S1501_C01_029",
                "White bachelors_up" = "S1501_C01_030",
                "White_non_hisp total" = "S1501_C01_031",
                "White_non_hisp high_school_up" = "S1501_C01_032",
                "White_non_hisp bachelors_up" = "S1501_C01_033",
                "Black total" = "S1501_C01_034",
                "Black high_school_up" = "S1501_C01_035",
                "Black bachelors_up" = "S1501_C01_036",
                "Amer_Indian total" = "S1501_C01_037",
                "Amer_Indian high_school_up" = "S1501_C01_038",
                "Amer_Indian bachelors_up" = "S1501_C01_039",
                "Asian total" = "S1501_C01_040",
                "Asian high_school_up" = "S1501_C01_041",
                "Asian bachelors_up" = "S1501_C01_042",
                "NHPI total" = "S1501_C01_043",
                "NHPI high_school_up" = "S1501_C01_044",
                "NHPI bachelors_up" = "S1501_C01_045",
                "Other total" = "S1501_C01_046",
                "Other high_school_up" = "S1501_C01_047",
                "Other bachelors_up" = "S1501_C01_048",
                "Multi total" = "S1501_C01_049",
                "Multi high_school_up" = "S1501_C01_050",
                "Multi bachelors_up" = "S1501_C01_051",
                "Hispanic total" = "S1501_C01_052",
                "Hispanic high_school_up" = "S1501_C01_053",
                "Hispanic bachelors_up" = "S1501_C01_054")

# County
acs_S1501_race_county <- get_acs(
  geography = "county",
  county = county_codes,
  state = "VA",
  var = vars_S1501_race,
  survey = "acs5",
  year = year)

# Tract
acs_S1501_race_tract <- get_acs(
  geography = "tract",
  county = county_codes,
  state = "VA",
  var = vars_S1501_race,
  survey = "acs5",
  year = year)

# Wrangle data
# County
edu_attain_race_county_wide <- acs_S1501_race_county %>% 
  separate(variable, into = c("race", "var"), sep = " ") %>% 
  pivot_wider(names_from = var, values_from = c(estimate, moe)) %>% 
  mutate(less_than_hs = estimate_total - estimate_high_school_up,
         hs_only = estimate_high_school_up - estimate_bachelors_up) %>% 
  rename(bachelors_up = estimate_bachelors_up, 
         group_total = estimate_total,
         group_total_moe = moe_total) %>% 
  select(GEOID, NAME, race, less_than_hs, hs_only, bachelors_up, group_total, group_total_moe) 

# Return to long, add percents, prep for data viz
edu_attain_race_county <- edu_attain_race_county_wide %>% 
  pivot_longer(less_than_hs:bachelors_up) %>% 
  mutate(percent = round(100 * (value / group_total), digits = 2),
         year = year) %>% 
  rename(edu_level = name,
         estimate = value,
         locality = NAME)

# Tract
edu_attain_race_tract_wide <- acs_S1501_race_tract %>% 
  separate(variable, into = c("race", "var"), sep = " ") %>% 
  pivot_wider(names_from = var, values_from = c(estimate, moe)) %>% 
  mutate(less_than_hs = estimate_total - estimate_high_school_up,
         hs_only = estimate_high_school_up - estimate_bachelors_up) %>% 
  rename(bachelors_up = estimate_bachelors_up, 
         group_total = estimate_total,
         group_total_moe = moe_total) %>% 
  select(GEOID, NAME, race, less_than_hs, hs_only, bachelors_up, group_total, group_total_moe) 

# Return to long, add percents, prep for data viz
edu_attain_race_tract <- edu_attain_race_tract_wide %>% 
  pivot_longer(less_than_hs:bachelors_up) %>% 
  mutate(percent = round(100 * (value / group_total), digits = 2),
         year = year) %>% 
  rename(edu_level = name,
         estimate = value) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>% 
  select(!c(NAME, state))

# Join tract names
edu_attain_race_tract <- edu_attain_race_tract %>% 
  left_join(tract_names)

# Educational Attainment by Race/Ethnicity: Charlottesville, county & tract ----
cville_edu_attain_race_county <- edu_attain_race_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_edu_attain_race_county, paste0("data/cville_edu_attain_race_county", "_", year, ".csv"))

cville_edu_attain_race_tract <- edu_attain_race_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_edu_attain_race_tract, paste0("data/cville_edu_attain_race_tract", "_", year, ".csv"))

# Educational Attainment by Race/Ethnicity: Albemarle, county & tract ----
alb_edu_attain_race_county <- edu_attain_race_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_edu_attain_race_county, paste0("data/alb_edu_attain_race_county", "_", year, ".csv"))

alb_edu_attain_race_tract <- edu_attain_race_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_edu_attain_race_tract, paste0("data/alb_edu_attain_race_tract", "_", year, ".csv"))

# Educational Attainment by Race/Ethnicity: Charlottesville, Albemarle Combined Table ----
region_edu_attain_race <- edu_attain_race_county %>% 
  group_by(race, edu_level, year) %>% 
  summarize(estimate = sum(estimate),
            group_total_moe = moe_sum(moe = group_total_moe, estimate = group_total),
            group_total = sum(group_total),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / group_total), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>% 
  select(region_fips, locality, race, edu_level, estimate, group_total, group_total_moe, percent, year)

write_csv(region_edu_attain_race, paste0("data/region_edu_attain_race", "_", year, ".csv"))

## .....................................................
# AP Course Enrollment by Race/Ethnicity & Disadvantaged ----
# Source: https://www.doe.virginia.gov/data-policy-funding/data-reports/program-participation-data/advanced-programs
# TEMP SOLUTION TO HTTP 403
# Downloaded 2022-2023.xlsx to tempdata

# Get total for all students enrolled in AP
# Read excel
ap_sheet_1 <- read_xlsx("data/tempdata/2022-2023.xlsx", sheet = "Adv Programs", skip = 3) %>% 
  clean_names()
ap_sheet_1 <- ap_sheet_1[-c(1), ]
# Filter for divisions
ap_sheet_1 <- ap_sheet_1 %>% 
  filter(division_name %in% c("Albemarle County", "Charlottesville City")) 

# Get district totals  
ap_enroll_all <- ap_sheet_1 %>% 
  mutate(students_taking_1_or_more_ap_courses = as.numeric(students_taking_1_or_more_ap_courses),
         label = "All Students") %>% 
  group_by(division_name, label) %>% 
  summarise(students_in_ap_courses = sum(students_taking_1_or_more_ap_courses, na.rm = TRUE),
            .groups = 'drop')

# Get students enrolled in AP by race
# Read excel
ap_sheet_2 <- read_xlsx("data/tempdata/2022-2023.xlsx", sheet = "Adv Programs by Race", skip = 3) %>% 
  clean_names()
ap_sheet_2 <- ap_sheet_2[-c(1), ]
# Filter for divisions
ap_sheet_2 <- ap_sheet_2 %>% 
  filter(division_name %in% c("Albemarle County", "Charlottesville City")) 

# Get district totals 
ap_enroll_race <- ap_sheet_2 %>% 
  mutate(students_taking_1_or_more_ap_courses = as.numeric(students_taking_1_or_more_ap_courses)) %>% 
  rename(label = race) %>% 
  group_by(division_name, label) %>% 
  summarise(students_in_ap_courses = sum(students_taking_1_or_more_ap_courses, na.rm = TRUE),
            .groups = 'drop')

# Get students enrolled in AP by disadvantage
# Read excel
ap_sheet_3 <- read_xlsx("data/tempdata/2022-2023.xlsx", sheet = "Adv Programs by Disadvantage", skip = 3) %>% 
  clean_names()
ap_sheet_3 <- ap_sheet_3[-c(1), ]
# Filter for divisions and disadvantage
ap_sheet_3 <- ap_sheet_3 %>% 
  filter(division_name %in% c("Albemarle County", "Charlottesville City") & disadvantage == "Y") 

# Get district totals for AP enrollment 
ap_enroll_disadv <- ap_sheet_3 %>% 
  mutate(students_taking_1_or_more_ap_courses = as.numeric(students_taking_1_or_more_ap_courses),
         label = "Economically Disadvantaged") %>% 
  group_by(division_name, label) %>% 
  summarise(students_in_ap_courses = sum(students_taking_1_or_more_ap_courses, na.rm = TRUE),
            .groups = 'drop')

# Merge Race and disadvantage tables
ap_enroll <- rbind(ap_enroll_race, ap_enroll_disadv, ap_enroll_all) %>% 
  mutate(school_year = "2022-2023")

# Get School Enrollment, Fall-Membership numbers
# Need to compare all hs enrolled vs 10,11,12 enrollment
# All potential AP students vs students most likely to be enrolled in AP
# Source: https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304

fall_membership_race <- read_csv("data/tempdata/fall_membership_race_2022_2023.csv") %>% 
  clean_names()

fall_membership_race <- fall_membership_race %>%
  group_by(school_year, division_name, race) %>% 
  summarise(group_total = sum(gr_9, gr_10, gr_11, gr_12, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(label = race,
         label = str_replace(label,"Native Hawaiian  or Pacific Islander", "Native Hawaiian or Pacific Islander")) %>%
  select(division_name, label, group_total, school_year)

fall_membership_disadv <- read_csv("data/tempdata/fall_membership_disadv_2022_2023.csv") %>% 
  clean_names()

fall_membership_disadv <- fall_membership_disadv %>%
  group_by(school_year, division_name) %>% 
  summarise(group_total = sum(gr_9, gr_10, gr_11, gr_12, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(label = "Economically Disadvantaged") %>% 
  select(division_name, label, group_total, school_year)

fall_membership_total <- read_csv("data/tempdata/fall_membership_total_2022_2023.csv") %>% 
  clean_names()

fall_membership_total <- fall_membership_total %>%
  group_by(school_year, division_name) %>% 
  summarise(group_total = sum(gr_9, gr_10, gr_11, gr_12, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(label = "All Students") %>% 
  select(division_name, label, group_total, school_year)

# Merge membership tables
fall_membership <- rbind(fall_membership_race, fall_membership_disadv, fall_membership_total)

# Join AP enrollment and membership tables
ap_enroll <- ap_enroll %>% 
    left_join(fall_membership)

# Create percents
ap_enroll <- ap_enroll %>%
  mutate(percent = round(100 * (students_in_ap_courses / group_total), digits = 2))

# AP Course Enrollment: Charlottesville ----
cville_ap_enroll <- ap_enroll %>% 
  filter(division_name == "Charlottesville City")

write_csv(cville_ap_enroll, paste0("data/cville_ap_enroll_2022_2023.csv"))

# AP Course Enrollment: Albemarle ----
alb_ap_enroll <- ap_enroll %>% 
  filter(division_name == "Albemarle County")

write_csv(alb_ap_enroll, paste0("data/alb_ap_enroll_2022_2023.csv"))

## .......................................................
# Suspensions by Race/Ethnicity & Chronic Absenteeism ----
# Source: ACPS report
# Available at schoolquality.virginia.gov 
# Go to:  (1) Download Data (https://schoolquality.virginia.gov/download-data) 
#         (2) Make the following selections:
#             Select Reporting Level: Division
#             Select Divisions (multi-select): Albemarle County Public Schools; Charlottesville City Public Schools
#             Select Data Type: Learning Climate
#             Select Indicators (multi-select): Long Term suspensions; Short Term suspensions; Chronic Absenteeism
#             Select School Year: most recent year available
#         (3) Download Spreadsheet (save zip to tempdata)

# Short Term Suspensions ----
st_suspensions <- read_csv("data/tempdata/vdoe_data/Short Term Suspensions.csv", skip = 3) %>% 
  clean_names()

# Long Term Suspensions ----
lt_suspensions <- read_csv("data/tempdata/vdoe_data/Long Term Suspensions.csv", skip = 3) %>% 
  clean_names()

# Chronic Absenteeism ----
absent <- read_csv("data/tempdata/vdoe_data/Chronic Absenteeism.csv", skip = 3) %>% 
  clean_names()

cville_absenteeism <- absent %>% 
  filter(division == "Charlottesville City Public Schools")

write_csv(cville_absenteeism, paste0("data/cville_absenteeism_2022_2023.csv"))

alb_absenteeism <- absent %>% 
  filter(division == "Albemarle County Public Schools")

write_csv(alb_absenteeism, paste0("data/alb_absenteeism_2022_2023.csv"))

# Combined Region
region_absenteeism <- absent %>% 
  mutate(count_below_10 = as.numeric(count_below_10),
         count_above_10 = as.numeric(count_above_10)) %>% 
  group_by(year, subgroup) %>% 
  summarize(count_below_10 = sum(count_below_10),
            count_above_10 = sum(count_above_10),
            total_count = sum(count_below_10, count_above_10),
            .groups = "drop") %>% 
  mutate(percent_below_10 = round(100 * count_below_10 / total_count, 2),
         percent_above_10 = round(100 * count_above_10 / total_count, 2),
         division = "ACPS & CCS") %>% 
  select(year, division, subgroup, count_below_10:percent_above_10)

write_csv(region_absenteeism, paste0("data/region_absenteeism_2022_2023.csv"))

## .....................................................
## End