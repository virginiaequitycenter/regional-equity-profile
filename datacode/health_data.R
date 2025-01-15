# R Script for pulling and examining health data
# Authors: Beth Mitchell, Henry DeMarco
# Date Created: June 17, 2024
# Last Updated: Aug 16, 2024

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

# Health TOC ----
# AHDI MEASURES, COUNTY & TRACT LEVEL
# County-Level Life Expectancy, incld. Race/Ethnicity, 2024
# - Source: https://www.countyhealthrankings.org/health-data/virginia/data-and-resources
# Tract-Level Life Expectancy, 2015 
# - Source: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html (Source has not updated since 2010-2015 values)
# OTHER MEASURES
# Food Insecurity, 2019-2022
# - Source: https://www.feedingamerica.org/research/map-the-meal-gap/by-county
# Health Insurance Coverage, 2022 
# - By Tract
# - By County, incld. Race/Ethnicity
# - Source: ACS Table S2701
# CDC Places Health Measures  
# - Health Outcomes: "DIABETES", "OBESITY", "CHD", "BPHIGH", "DEPRESSION",
#                     "CASTHMA", "COPD", "CANCER", "STROKE", "HIGHCHOL", "KIDNEY"
# - Prevention: "CHECKUP", "DENTAL", "CERVICAL", "COLON_SCREEN", "MAMMOUSE"
# - (Removed: Health Status: "MHLTH", "PHLTH")
# - Source: https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh/about_data
# EMS Opioid Overdose data
# - Source: 

# Load packages
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(readxl)
library(janitor)

# Census API Key
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

# Creating basic objects ----

# Year for ACS data (single year)
year <- 2017

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

cville_tracts <- tract_names %>% 
  filter(county == "Charlottesville") %>% 
  mutate(GEOID_TRACT_10 = GEOID)

alb_tracts <- tract_names %>% 
  filter(county == "Albemarle")

# 2020 Census Tract to 2010 Census Tract Relationship File
tract_crosswalk <- read_delim("data/tempdata/tab20_tract20_tract10_natl.txt", delim = "|")

alb_tracts_crosswalk <- alb_tracts %>% 
  left_join(tract_crosswalk, by = join_by(GEOID == GEOID_TRACT_20)) %>% 
  filter(substr(GEOID_TRACT_10, 1, 5) == "51003") %>% 
  select(county, locality_num, tract_num, tractnames, GEOID,  GEOID_TRACT_10, NAMELSAD_TRACT_20, NAMELSAD_TRACT_10) %>% 
  mutate(tract_check = case_when(NAMELSAD_TRACT_20 == NAMELSAD_TRACT_10 ~ TRUE,
                                 GEOID == "51003011302" & GEOID_TRACT_10 == "51003011301" ~ FALSE,
                                 substr(NAMELSAD_TRACT_20, 1, 16) == substr(NAMELSAD_TRACT_10, 1, 16) ~ TRUE,
                                 .default = FALSE)) %>% 
  filter(tract_check == TRUE) %>% 
  select(county, GEOID, locality_num, tract_num, tractnames, GEOID_TRACT_10)

tract_names_crosswalk <- rbind(alb_tracts_crosswalk, cville_tracts)

# # Add old tract numbers
# tract_names_update <- tract_names %>% 
#   mutate(GEOID_2010 = case_when(GEOID %in% c("51003011101", "51003011102", "51003011103") ~ "51003011100",
#                                 GEOID %in% c("51003010301", "51003010302", "51003010303") ~ "51003010300",
#                                 GEOID %in% c("51003010501", "51003010502") ~ "51003010500"))
# 
# library(tigris)
# options(tigris_use_cache = TRUE)
# library(ggrepel)
# 
# tracts_alb_2022 <- tracts(state = "VA", county = c("003"), year = 2022)
# tracts_alb_2019 <- tracts(state = "VA", county = c("003"), year = 2019)
# tracts_cville_2022 <- tracts(state = "VA", county = c("540"), year = 2022)
# tracts_cville_2019 <- tracts(state = "VA", county = c("540"), year = 2019)
# 
# ggplot(tracts_alb_2022) +
#   geom_sf(color = "blue") +
#   geom_sf_label(aes(label = TRACTCE), size = 2)
# 
# ggplot(tracts_alb_2019) +
#   geom_sf(color = "purple") +
#   geom_sf_label(aes(label = TRACTCE), size = 2)


# Create tempdata folder
if (!dir.exists("data/tempdata")){
  dir.create("data/tempdata")}

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
meta_table_2017 <- all_acs_meta()



# Opens the newly made table
# View(meta_table)

## .............................................................
# County-Level Life Expectancy, incld. Race/Ethnicity, 2024 ----

# Get Data
# Source: https://www.countyhealthrankings.org/health-data/virginia/data-and-resources

url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2024%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xlsx"

download.file(url, destfile="data/tempdata/countyhealthrankings2024.xlsx", method="libcurl")

# Read data
life_exp_sheet <- read_excel("data/tempdata/countyhealthrankings2024.xlsx", sheet = "Additional Measure Data", skip = 1)

# Reduce, rename, derive
life_exp_sheet <- life_exp_sheet %>% 
  select(FIPS, locality = County, 
         lifeexp_all_est = `Life Expectancy`, lifeexp_all_lb = `95% CI - Low...5`, lifeexp_all_ub = `95% CI - High...6`,
         lifeexp_white_est = `Life Expectancy (Non-Hispanic White)`, lifeexp_white_lb = `Life Expectancy (Non-Hispanic White) 95% CI - Low`, lifeexp_white_ub = `Life Expectancy (Non-Hispanic White) 95% CI - High`,
         lifeexp_black_est = `Life Expectancy (Non-Hispanic Black)`, lifeexp_black_lb = `Life Expectancy (Non-Hispanic Black) 95% CI - Low`, lifeexp_black_ub = `Life Expectancy (Non-Hispanic Black) 95% CI - High`,
         lifeexp_ltnx_est = `Life Expectancy (Hispanic (all races))`, lifeexp_ltnx_lb = `Life Expectancy (Hispanic (all races)) 95% CI - Low`, lifeexp_ltnx_ub = `Life Expectancy (Hispanic (all races)) 95% CI - High`,
         lifeexp_asian_est = `Life Expectancy (Non-Hispanic Asian)`, lifeexp_asian_lb = `Life Expectancy (Non-Hispanic Asian) 95% CI - Low`, lifeexp_asian_ub = `Life Expectancy (Non-Hispanic Asian) 95% CI - High`) %>% 
  mutate(lifeexp_all_moe = (lifeexp_all_ub-lifeexp_all_lb)/2,
         lifeexp_white_moe = (lifeexp_white_ub-lifeexp_white_lb)/2,
         lifeexp_black_moe = (lifeexp_black_ub-lifeexp_black_lb)/2,
         lifeexp_ltnx_moe = (lifeexp_ltnx_ub-lifeexp_ltnx_lb)/2,
         lifeexp_asian_moe = (lifeexp_asian_ub-lifeexp_asian_lb)/2,
         fips = str_remove(FIPS, "51")) %>% 
  mutate_if(is.numeric, round, 1) %>% 
  select(FIPS, fips, locality, lifeexp_all_est, lifeexp_all_lb, lifeexp_all_ub, lifeexp_all_moe, 
         lifeexp_black_est, lifeexp_black_lb, lifeexp_black_ub, lifeexp_black_moe, 
         lifeexp_ltnx_est, lifeexp_ltnx_lb, lifeexp_ltnx_ub, lifeexp_ltnx_moe, 
         lifeexp_white_est, lifeexp_white_lb, lifeexp_white_ub, lifeexp_white_moe, 
         lifeexp_asian_est, lifeexp_asian_lb, lifeexp_asian_ub, lifeexp_asian_moe)

# Filter & wrangle
life_exp_county <- life_exp_sheet %>% 
  filter(fips %in% county_codes) %>% 
  pivot_longer(lifeexp_all_est:lifeexp_asian_moe) %>% 
  mutate(name = str_remove(name, "lifeexp_"),
         year = 2024) %>% 
  separate(name, into = c("group", "name")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  relocate(year, .after = last_col())

names(life_exp_county) <- c("GEOID", "fips", "locality", "group", "lifeexp_est", "lower_bound", "upper_bound", "lifeexp_moe", "year")

# County-Level Life Expectancy, incld. Race/Ethnicity, 2024: Charlottesville ----
cville_life_exp_county_2024 <- life_exp_county %>% 
  filter(locality == "Charlottesville City")

write_csv(cville_life_exp_county_2024, "data/cville_life_exp_county_2024.csv")

# County-Level Life Expectancy, incld. Race/Ethnicity, 2024: Albemarle ----
alb_life_exp_county_2024 <- life_exp_county %>% 
  filter(locality == "Albemarle")

write_csv(alb_life_exp_county_2024, "data/alb_life_exp_county_2024.csv")

## .....................................
# Tract-Level Life Expectancy, Charlottesville, TJHD 2018 ----
tjhd_lifeexp <- read_csv("data/tjhd_life_exp_estimates_2018.csv")

tjhd_lifeexp_cville <- tjhd_lifeexp %>% 
  filter(county == 540) %>% 
  mutate(GEOID = paste0(st,county,tract_num)) %>% 
  rename(locality_num = county)

# Join tract names
tjhd_lifeexp_cville <- tjhd_lifeexp_cville %>% 
  left_join(cville_tracts, by = join_by(GEOID == GEOID, tract_num == tract_num, locality_num == locality_num))

# Save csv
write_csv(tjhd_lifeexp_cville, "data/cville_tjhd_life_exp_tract_2012.csv")

# Tract-Level Life Expectancy, 2015 ----
# Source: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html (Source has not updated since 2010-2015 values)
url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/VA_A.CSV"
download.file(url, destfile="data/tempdata/va_usasleep.csv", method="libcurl")

# read data and rename
lifeexp_tract_sheet <- read_csv("data/tempdata/va_usasleep.csv", col_types = c("c","c","c","c","n","n","n"))
names(lifeexp_tract_sheet) <- c("GEOID", "state", "locality_num", "tract_num", "life_exp", "se", "flag")

# Filter and derive metrics
lifeexp_tract <- lifeexp_tract_sheet %>%
  filter(locality_num %in% county_codes) %>% 
  rename(lifeexpE = life_exp) %>%
  mutate(lifeexpM = 1.64*se,
         year = 2015) %>%
  select(-se, -flag)

# Join tract names (merging old and new tracts)
lifeexp_tract <- lifeexp_tract %>% 
  full_join(tract_names_crosswalk, by = join_by(GEOID == GEOID_TRACT_10), keep = TRUE)  

# Some tracts missing life Expectancy data: 
# Albemarle: GEOID_2020: 51003010904; GEOID_2010: 51003010902 & 51003010903; Carr's Hill-McCormick Road (UVA)
# Charlottesville: GEOID_2020/GEOID_2010: 51540000202; 10th & Page-Venable
# Charlottesville: GEOID_2020/GEOID_2010: 51540000600; JPA-Fontaine

# Tract-Level Life Expectancy, 2015: Charlottesville ----
cville_life_exp_tract_2015 <- lifeexp_tract %>% 
  filter(county == "Charlottesville")

write_csv(cville_life_exp_tract_2015, "data/cville_life_exp_tract_2015.csv")

# Tract-Level Life Expectancy, 2015: Albemarle ----
alb_life_exp_tract_2015 <- lifeexp_tract %>% 
  filter(county == "Albemarle")

write_csv(alb_life_exp_tract_2015, "data/alb_life_exp_tract_2015.csv")

## ...............................
# Food Insecurity, 2019-2022 ----
# Source: https://map.feedingamerica.org 
# Dataset request form: https://www.feedingamerica.org/research/map-the-meal-gap/by-county

# Read in data
# Need to define col_types for each variable
usda_sheet <- read_excel("data/tempdata/MMG_Data_v2/MMG2024_2019-2022_Data_ToShare_v2.xlsx", sheet = "County", col_types = c(rep("text",3),rep("numeric",16)))
food_insecure <- usda_sheet %>% 
  filter(FIPS %in% c("51003", "51540")) %>% 
  clean_names()

# Food Insecurity, 2019-2022: Charlottesville ----
cville_food_insecure_2019_2022 <- food_insecure %>% 
  filter(county_state == "Charlottesville city, Virginia")

write_csv(cville_food_insecure_2019_2022, "data/cville_food_insecure_2019_2022.csv")

# Food Insecurity, 2019-2022: Albemarle ----
alb_food_insecure_2019_2022 <- food_insecure %>% 
  filter(county_state == "Albemarle County, Virginia")

write_csv(alb_food_insecure_2019_2022, "data/alb_food_insecure_2019_2022.csv")

## ..........................................................
# Health Insurance Coverage, by Tract: Table S2701, 2022 ----
# S2701_C01 is total, C02 is insured, C03 is percent insured, C04 is uninsured, C05 is percent uninsured

# Get ACS data
vars_S2701_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  var = c("S2701_C02_001", "S2701_C04_001"),
  survey = "acs5", 
  cache = TRUE,
  summary_var = "S2701_C01_001",
  year = year) %>% 
  mutate(variable = case_when(variable == "S2701_C02_001" ~ "insured",
                              variable == "S2701_C04_001" ~ "uninsured"))

# Wrangle
health_insured_tract <- vars_S2701_tract %>% 
  pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>% 
  mutate(percent_insured = round(100 * (estimate_insured / summary_est), digits = 2),
         percent_uninsured = round(100 * (estimate_uninsured / summary_est), digits = 2),
         year = year) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE)

# Join tract names
health_insured_tract <- health_insured_tract %>% 
  left_join(tract_names)

# Health Insurance Coverage, by Tract: Charlottesville ----
cville_health_insured_tract <- health_insured_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_health_insured_tract, paste0("data/cville_health_insured_tract", "_", year, ".csv"))

# Health Insurance Coverage, by Tract: Albemarle ----
alb_health_insured_tract <- health_insured_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_health_insured_tract, paste0("data/alb_health_insured_tract", "_", year, ".csv"))

## ..........................................................................
# Health Insurance Coverage, County by Race/Ethnicity: Table S2701, 2022 ----

# Get ACS Data
# Vars for insured/uninsured
acs_vars_S2701 <- c("insured; All" = "S2701_C02_001",
                    "insured; White alone" = "S2701_C02_016",
                    "insured; Black or African American alone" = "S2701_C02_017",
                    "insured; American Indian and Alaska Native alone" = "S2701_C02_018",
                    "insured; Asian alone" = "S2701_C02_019",
                    "insured; Native Hawaiian and Other Pacific Islander alone" = "S2701_C02_020",
                    "insured; Some other race alone" = "S2701_C02_021",
                    "insured; Two or more races" = "S2701_C02_022",
                    "insured; Hispanic or Latino" = "S2701_C02_023",
                    "insured; White non Hispanic" = "S2701_C02_024",
                    "uninsured; All" = "S2701_C04_001",
                    "uninsured; White alone" = "S2701_C04_016",
                    "uninsured; Black or African American alone" = "S2701_C04_017",
                    "uninsured; American Indian and Alaska Native alone" = "S2701_C04_018",
                    "uninsured; Asian alone" = "S2701_C04_019",
                    "uninsured; Native Hawaiian and Other Pacific Islander alone" = "S2701_C04_020",
                    "uninsured; Some other race alone" = "S2701_C04_021",
                    "uninsured; Two or more races" = "S2701_C04_022",
                    "uninsured; Hispanic or Latino" = "S2701_C04_023",
                    "uninsured; White non Hispanic" = "S2701_C04_024")

# Vars for total population counts
acs_vars_S2701_C01 <- c("All" = "S2701_C01_001",
                        "White alone" = "S2701_C01_016",
                        "Black or African American alone" = "S2701_C01_017",
                        "American Indian and Alaska Native alone" = "S2701_C01_018",
                        "Asian alone" = "S2701_C01_019",
                        "Native Hawaiian and Other Pacific Islander alone" = "S2701_C01_020",
                        "Some other race alone" = "S2701_C01_021",
                        "Two or more races" = "S2701_C01_022",
                        "Hispanic or Latino" = "S2701_C01_023",
                        "White non Hispanic" = "S2701_C01_024")

vars_S2701_county <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  var = acs_vars_S2701,
  survey = "acs5",
  year = year)

# Group totals
vars_S2701_C01 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  var = acs_vars_S2701_C01,
  survey = "acs5",
  year = year) %>% 
  rename(group = variable,
         total_est = estimate,
         total_moe = moe)

# Prep table
health_insured_county_race <- vars_S2701_county %>% 
  separate(variable, into=c("variable","group"), sep="; ", remove=FALSE) %>% 
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

# Join tables
health_insured_county_race <- health_insured_county_race %>% 
  left_join(vars_S2701_C01)

# Create percents
health_insured_county_race <- health_insured_county_race %>% 
  mutate(percent_insured = round(100 * (estimate_insured / total_est), digits = 2),
         percent_uninsured = round(100 * (estimate_uninsured / total_est), digits = 2),
         year = year) %>% 
  rename(locality = NAME)
  
# Health Insurance Coverage, County by Race/Ethnicity: Charlottesville ----
cville_health_insured_county_race <- health_insured_county_race %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_health_insured_county_race, paste0("data/cville_health_insured_county_race", "_", year, ".csv"))

# Health Insurance Coverage, County by Race/Ethnicity: Albemarle ----
alb_health_insured_county_race <- health_insured_county_race %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_health_insured_county_race, paste0("data/alb_health_insured_county_race", "_", year, ".csv"))

# Health Insurance Coverage, by Race/Ethnicity: Charlottesville, Albemarle Combined Table ----
region_health_insured_race <- health_insured_county_race %>% 
  group_by(group, year) %>% 
  summarize(estimate_insured = sum(estimate_insured),
            moe_insured = moe_sum(moe = moe_insured, estimate = estimate_insured),
            estimate_uninsured = sum(estimate_uninsured),
            moe_uninsured = moe_sum(moe = moe_uninsured, estimate = estimate_uninsured),
            total_est = sum(total_est),
            .groups = 'drop') %>% 
  mutate(percent_insured = round(100 * (estimate_insured / total_est), digits = 2),
         percent_uninsured = round(100 * (estimate_uninsured / total_est), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>% 
  select(region_fips, locality, group, estimate_insured, moe_insured, estimate_uninsured, moe_uninsured, total_est, percent_insured, percent_uninsured, year)

write_csv(region_health_insured_race, paste0("data/region_health_insured_race", "_", year, ".csv"))

# Health Insurance Coverage 2012-2022, County by Race/Ethnicity: Table S2701, 2012-2022 ----

# 2012-2014
#  2015-2016
#  2017-2022

# Get ACS Data
# Vars for insured/uninsured
acs_vars_S2701 <- c("insured; All" = "S2701_C02_001",
                    "insured; White alone" = "S2701_C02_016",
                    "insured; Black or African American alone" = "S2701_C02_017",
                    "insured; American Indian and Alaska Native alone" = "S2701_C02_018",
                    "insured; Asian alone" = "S2701_C02_019",
                    "insured; Native Hawaiian and Other Pacific Islander alone" = "S2701_C02_020",
                    "insured; Some other race alone" = "S2701_C02_021",
                    "insured; Two or more races" = "S2701_C02_022",
                    "insured; Hispanic or Latino" = "S2701_C02_023",
                    "insured; White non Hispanic" = "S2701_C02_024",
                    "uninsured; All" = "S2701_C04_001",
                    "uninsured; White alone" = "S2701_C04_016",
                    "uninsured; Black or African American alone" = "S2701_C04_017",
                    "uninsured; American Indian and Alaska Native alone" = "S2701_C04_018",
                    "uninsured; Asian alone" = "S2701_C04_019",
                    "uninsured; Native Hawaiian and Other Pacific Islander alone" = "S2701_C04_020",
                    "uninsured; Some other race alone" = "S2701_C04_021",
                    "uninsured; Two or more races" = "S2701_C04_022",
                    "uninsured; Hispanic or Latino" = "S2701_C04_023",
                    "uninsured; White non Hispanic" = "S2701_C04_024")

# Vars for total population counts
acs_vars_S2701_C01 <- c("All" = "S2701_C01_001",
                        "White alone" = "S2701_C01_016",
                        "Black or African American alone" = "S2701_C01_017",
                        "American Indian and Alaska Native alone" = "S2701_C01_018",
                        "Asian alone" = "S2701_C01_019",
                        "Native Hawaiian and Other Pacific Islander alone" = "S2701_C01_020",
                        "Some other race alone" = "S2701_C01_021",
                        "Two or more races" = "S2701_C01_022",
                        "Hispanic or Latino" = "S2701_C01_023",
                        "White non Hispanic" = "S2701_C01_024")

vars_S2701_county_2017_2022 <- map_df(2022:2017,
                             ~ get_acs(
                               year = .x,
                               geography = "county",
                               state = "VA",
                               county = county_codes,
                               var = acs_vars_S2701,
                               survey = "acs5", 
                               cache = TRUE) %>%
                               mutate(year = .x)
)

# Group totals

vars_S2701_C01_2017_2022 <- map_df(2022:2017,
                                      ~ get_acs(
                                        year = .x,
                                        geography = "county",
                                        state = "VA",
                                        county = county_codes,
                                        var = acs_vars_S2701_C01,
                                        survey = "acs5", 
                                        cache = TRUE) %>%
                                        mutate(year = .x)
) %>% 
  rename(group = variable,
         total_est = estimate,
         total_moe = moe)

# Prep table
health_insured_county_race_2017_2022 <- vars_S2701_county_2017_2022 %>% 
  separate(variable, into=c("variable","group"), sep="; ", remove=FALSE) %>% 
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

# Join tables
health_insured_county_race_2017_2022 <- health_insured_county_race_2017_2022 %>% 
  left_join(vars_S2701_C01_2017_2022)

# Create percents
health_insured_county_race_2017_2022 <- health_insured_county_race_2017_2022 %>% 
  mutate(percent_insured = round(100 * (estimate_insured / total_est), digits = 2),
         percent_uninsured = round(100 * (estimate_uninsured / total_est), digits = 2),
         year = year) %>% 
  rename(locality = NAME)

# Health Insurance Coverage, County by Race/Ethnicity: Charlottesville ----
cville_health_insured_county_race_2017_2022 <- health_insured_county_race_2017_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_health_insured_county_race_2017_2022, paste0("data/cville_health_insured_county_race_2017_2022.csv"))

# Health Insurance Coverage, County by Race/Ethnicity: Albemarle ----
alb_health_insured_county_race_2017_2022 <- health_insured_county_race_2017_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_health_insured_county_race_2017_2022, paste0("data/alb_health_insured_county_race_2017_2022.csv"))

# Health Insurance Coverage, by Race/Ethnicity: Charlottesville, Albemarle Combined Table ----
region_health_insured_county_race_2017_2022 <- health_insured_county_race_2017_2022 %>% 
  group_by(group, year) %>% 
  summarize(estimate_insured = sum(estimate_insured),
            moe_insured = moe_sum(moe = moe_insured, estimate = estimate_insured),
            estimate_uninsured = sum(estimate_uninsured),
            moe_uninsured = moe_sum(moe = moe_uninsured, estimate = estimate_uninsured),
            total_est = sum(total_est),
            .groups = 'drop') %>% 
  mutate(percent_insured = round(100 * (estimate_insured / total_est), digits = 2),
         percent_uninsured = round(100 * (estimate_uninsured / total_est), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>% 
  select(region_fips, locality, group, estimate_insured, moe_insured, estimate_uninsured, moe_uninsured, total_est, percent_insured, percent_uninsured, year)

write_csv(region_health_insured_county_race_2017_2022, paste0("data/region_health_insured_county_race_2017_2022.csv"))

## .......................................
# CDC Places Health Measures by Tract ----
# Source: https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh/about_data

# define api endpoint with query for locality (limit is 1000; each locality alone is below this)
# more on the api here: https://dev.socrata.com/foundry/data.cdc.gov/cwsq-ngmh
api_alb <- "https://data.cdc.gov/resource/cwsq-ngmh.json/?StateAbbr=VA&countyname=Albemarle"
api_cvl <- "https://data.cdc.gov/resource/cwsq-ngmh.json/?StateAbbr=VA&countyname=Charlottesville"

# read in data
df_alb <- fromJSON(api_alb)
df_cvl <- fromJSON(api_cvl)

# bind rows
cdc_df <- bind_rows(df_alb, df_cvl)

# Filter for measures of interest
cdc_filtered <- cdc_df %>% 
  filter(measureid %in% c("DIABETES", "OBESITY", "CHD", "BPHIGH", "DEPRESSION", 
                          "CASTHMA", "COPD", "CANCER", "STROKE", "HIGHCHOL", "KIDNEY",
                          "CHECKUP", "DENTAL", "CERVICAL", "COLON_SCREEN",
                          "MAMMOUSE", "MHLTH", "PHLTH"))

# Selecting columns of interest
cdc_filtered <- cdc_filtered %>% 
  rename(GEOID = locationname) %>% 
  select("year", "statedesc", "countyname", "countyfips", "GEOID", 
         "category", "measureid", "data_value", "data_value_unit", "totalpopulation", "short_question_text", "measure")

# Join tract names
cdc_filtered <- cdc_filtered %>% 
  left_join(tract_names)

# CDC data for Data Viz Table

cdc_table_outcomes <- cdc_filtered %>% 
  filter(category == "Health Outcomes") %>% 
  select(GEOID, county, tractnames, measureid, data_value) %>% 
  pivot_wider(names_from = measureid, values_from = data_value)

cdc_table_status <- cdc_filtered %>% 
  filter(category == "Health Status") %>% 
  select(GEOID, county, tractnames, measureid, data_value) %>% 
  pivot_wider(names_from = measureid, values_from = data_value)

cdc_table_prevent <- cdc_filtered %>% 
  filter(category == "Prevention") %>% 
  select(GEOID, county, tractnames, measureid, data_value) %>% 
  pivot_wider(names_from = measureid, values_from = data_value)

# CDC Places Health Measures by Tract: Charlottesville ----
# Health Outcomes
cville_cdc_table_outcomes <- cdc_table_outcomes %>% 
  filter(county == "Charlottesville")

write_csv(cville_cdc_table_outcomes, "data/cville_cdc_table_outcomes_2023.csv")

# Health Prevention
cville_cdc_table_prevent <- cdc_table_prevent %>% 
  filter(county == "Charlottesville")

write_csv(cville_cdc_table_prevent, "data/cville_cdc_table_prevent_2023.csv")

# Heatlh Status
cville_cdc_table_status <- cdc_table_status %>% 
  filter(county == "Charlottesville")

write_csv(cville_cdc_table_status, "data/cville_cdc_table_status_2023.csv")

# CDC Places Health Measures by Tract: Albemarle ----
# Health Outcomes
alb_cdc_table_outcomes <- cdc_table_outcomes %>% 
  filter(county == "Albemarle")

write_csv(alb_cdc_table_outcomes, "data/alb_cdc_table_outcomes_2023.csv")

# Health Prevention
alb_cdc_table_prevent <- cdc_table_prevent %>% 
  filter(county == "Albemarle")

write_csv(alb_cdc_table_prevent, "data/alb_cdc_table_prevent_2023.csv")

# Heatlh Status
alb_cdc_table_status <- cdc_table_status %>% 
  filter(county == "Albemarle")

write_csv(alb_cdc_table_status, "data/alb_cdc_table_status_2023.csv")

# Data Viz testings
# library(gt)
# library(RColorBrewer)
# library(scales)
# library(palettes)
# library(paletteer)
# 
# pal_blue <- colorRampPalette(brewer.pal(9, "Blues"))(15)
# pal_diverge <- colorRampPalette(brewer.pal(11, "RdYlBu"))(15)
# 
# cdc_table_outcomes %>% 
#   filter(county == "Charlottesville") %>% 
#   select(-GEOID.x, -GEOID.y, -county) %>% 
#   mutate(across(OBESITY:KIDNEY, ~ as.numeric(.x))) %>% 
#   gt(rowname_col = "tractnames") %>% 
#   data_color(
#     columns = -tractnames,
#     direction = "column",
#     # fn= scales::col_numeric(
#     #   "Blues",
#     #   c(0, 50),
#     #   na.color = "#808080",
#     #   alpha = FALSE,
#     #   reverse = FALSE
#     # )
#     domain = c(0, 50),
#     palette = "YlOrRd"
#     # reverse = TRUE
#     # na_color = "white"
#   )
# 
# cdc_table_status %>% 
#   filter(county == "Charlottesville") %>% 
#   select(-GEOID.x, -GEOID.y, -county) %>% 
#   gt(rowname_col = "tractnames") %>% 
#   data_color(
#     direction = "column",
#     palette = pal_blue,
#     na_color = "white"
#   )
# 
# cdc_table_prevent %>% 
#   filter(county == "Charlottesville") %>% 
#   select(-GEOID.x, -GEOID.y, -county) %>% 
#   mutate(across(COLON_SCREEN:CHECKUP, ~ as.numeric(.x))) %>% 
#   gt(rowname_col = "tractnames") %>% 
#   data_color(
#     direction = "column",
#     # method = "numeric",
#     domain = c(30,90),
#     palette = "RdYlBu"
#     # na_color = "white"
#   )
# 

