# R Script for pulling and examining demographic data
# Author: Henry DeMarco, Beth Mitchell
# Date Created: June 4, 2024
# Last Updated: June 28, 2024

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
region_name <- "Combined Region"

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

## .................................
# Race: Table B02001, 2012-2022 ----

# Race: Charlottesville, 2012-2022 ----
cville_vars_B02001 <- map_df(2022:2012,
                             ~ get_acs(
                               year = .x,
                               geography = "county",
                               state = "VA",
                               county = "540",
                               table = "B02001",
                               summary_var = "B02001_001",
                               survey = "acs5", 
                               cache = TRUE) %>%
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B02001_002", # White alone
                                                      "B02001_003", # Black or African American alone
                                                      "B02001_004", # American Indian and Alaska Native alone
                                                      "B02001_005", # Asian alone
                                                      "B02001_006", # Native Hawaiian and Other Pacific Islander alone
                                                      "B02001_007", # Some Other Race alone
                                                      "B02001_008")) # Two or More Races
                             )

# Finalizing table:
cville_race_2012_2022 <- cville_vars_B02001 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           variable == "B02001_002" ~ "White alone",
           variable == "B02001_003" ~ "Black or African American alone",
           variable == "B02001_004" ~ "American Indian and Alaska Native alone",
           variable == "B02001_005" ~ "Asian alone",
           variable == "B02001_006" ~ "Native Hawaiian and Other Pacific Islander alone",
           variable == "B02001_007" ~ "Some Other Race alone",
           variable == "B02001_008" ~ "Two or More Races")) %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(cville_race_2012_2022, "data/cville_race_2012_2022.csv")

## ..............................
# Race: Albemarle, 2012-2022 ----
alb_vars_B02001 <- map_df(2022:2012,
                          ~ get_acs(
                            year = .x,
                            geography = "county",
                            state = "VA",
                            county = "003",
                            table = "B02001",
                            summary_var = "B02001_001",
                            survey = "acs5",
                            cache = TRUE) %>%
                            mutate(year = .x) %>% 
                            filter(variable %in% c("B02001_002", # White alone
                                                   "B02001_003", # Black or African American alone
                                                   "B02001_004", # American Indian and Alaska Native alone
                                                   "B02001_005", # Asian alone
                                                   "B02001_006", # Native Hawaiian and Other Pacific Islander alone
                                                   "B02001_007", # Some Other Race alone
                                                   "B02001_008")) # Two or More Races
                          )

# Finalizing table:
alb_race_2012_2022 <- alb_vars_B02001 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           variable == "B02001_002" ~ "White alone",
           variable == "B02001_003" ~ "Black or African American alone",
           variable == "B02001_004" ~ "American Indian and Alaska Native alone",
           variable == "B02001_005" ~ "Asian alone",
           variable == "B02001_006" ~ "Native Hawaiian and Other Pacific Islander alone",
           variable == "B02001_007" ~ "Some Other Race alone",
           variable == "B02001_008" ~ "Two or More Races")) %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(alb_race_2012_2022, "data/alb_race_2012_2022.csv")

## ..............................................................
# Race: Charlottesville, Albemarle Combined Table, 2012-2022 ----
combined_race_2012_2022 <- rbind(alb_race_2012_2022, cville_race_2012_2022)

# Create summary variables for the combined counties
combined_race_summarize <- combined_race_2012_2022 %>% 
  group_by(label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_pop = sum(total_pop))

# Create percentages from estimates
region_race_2012_2022 <- combined_race_summarize %>% 
  mutate(percent = round(((estimate / total_pop) * 100), digits = 2),
         locality = "Combined Region") %>% 
  select(locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(region_race_2012_2022, "data/region_race_2012_2022.csv")

## ..................................................................................
# Ethnicity by Race: Table B03002 (Hispanic or Latino Origin by Race), 2012-2022 ----
# Rows match up for 2012 and 2022 (!)

# Ethnicity by Race: Charlottesville, 2012-2022 ----
cville_vars_B03002 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "540",
           table = "B03002",
           summary_var = "B03002_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B03002_003", # Not Hispanic or Latino: White alone
                                  "B03002_004", # Not Hispanic or Latino: Black or African American alone
                                  "B03002_005", # Not Hispanic or Latino: American Indian and Alaska Native alone
                                  "B03002_006", # Not Hispanic or Latino: Asian alone
                                  "B03002_007", # Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
                                  "B03002_008", # Not Hispanic or Latino: Some other race alone
                                  "B03002_009", # Not Hispanic or Latino: Two or more races
                                  "B03002_012")) # Hispanic or Latino
         )

# Finalizing table:
cville_ethn_race_2012_2022 <- cville_vars_B03002 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           variable == "B03002_003" ~ "Not Hispanic or Latino: White alone",
           variable == "B03002_004" ~ "Not Hispanic or Latino: Black or African American alone",
           variable == "B03002_005" ~ "Not Hispanic or Latino: American Indian and Alaska Native alone",
           variable == "B03002_006" ~ "Not Hispanic or Latino: Asian alone",
           variable == "B03002_007" ~ "Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone",
           variable == "B03002_008" ~ "Not Hispanic or Latino: Some other race alone",
           variable == "B03002_009" ~ "Not Hispanic or Latino: Two or more races",
           variable == "B03002_012" ~ "Hispanic or Latino")) %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(cville_ethn_race_2012_2022, "data/cville_ethn_race_2012_2022.csv")

## ............................................................
# Ethnicity by Race: Albemarle, 2012-2022 ----
alb_vars_B03002 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "003",
           table = "B03002",
           summary_var = "B03002_001",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B03002_003", # Not Hispanic or Latino: White alone
                                  "B03002_004", # Not Hispanic or Latino: Black or African American alone
                                  "B03002_005", # Not Hispanic or Latino: American Indian and Alaska Native alone
                                  "B03002_006", # Not Hispanic or Latino: Asian alone
                                  "B03002_007", # Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
                                  "B03002_008", # Not Hispanic or Latino: Some other race alone
                                  "B03002_009", # Not Hispanic or Latino: Two or more races
                                  "B03002_012")) # Hispanic or Latino
      )

# Finalizing table:
alb_ethn_race_2012_2022 <- alb_vars_B03002 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           variable == "B03002_003" ~ "Not Hispanic or Latino: White alone",
           variable == "B03002_004" ~ "Not Hispanic or Latino: Black or African American alone",
           variable == "B03002_005" ~ "Not Hispanic or Latino: American Indian and Alaska Native alone",
           variable == "B03002_006" ~ "Not Hispanic or Latino: Asian alone",
           variable == "B03002_007" ~ "Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone",
           variable == "B03002_008" ~ "Not Hispanic or Latino: Some other race alone",
           variable == "B03002_009" ~ "Not Hispanic or Latino: Two or more races",
           variable == "B03002_012" ~ "Hispanic or Latino")) %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(alb_ethn_race_2012_2022, "data/alb_ethn_race_2012_2022.csv")

## ..............................................................
# Ethnicity by Race: Charlottesville, Albemarle Combined Table, 2012-2022 ----
combined_ethn_race_2012_2022 <- rbind(alb_ethn_race_2012_2022, cville_ethn_race_2012_2022)

# Create summary variables for the combined counties
combined_ethn_race_summarize <- combined_ethn_race_2012_2022 %>% 
  group_by(label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_pop = sum(total_pop))

# Create percentages from estimates
region_ethn_race_2012_2022 <- combined_ethn_race_summarize %>% 
  mutate(percent = round(((estimate / total_pop) * 100), digits = 2),
         locality = "Combined Region") %>% 
  select(locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(region_ethn_race_2012_2022, "data/region_ethn_race_2012_2022.csv")

## ......................................
# Ethnicity: Table B03003, 2012-2022 ----
# Rows match up for 2012 and 2022 (!)

# Ethnicity: Charlottesville, 2012-2022 ----
cville_vars_B03003 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "540",
           table = "B03003",
           summary_var = "B03003_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B03003_002", # Not Hispanic or Latino
                                  "B03003_003")) # Hispanic or Latino
         )

# Finalizing table:
cville_ethn_2012_2022 <- cville_vars_B03003 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           variable == "B03003_002" ~ "Not Hispanic or Latino",
           variable == "B03003_003" ~ "Hispanic or Latino"
         )) %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(cville_ethn_2012_2022, "data/cville_ethn_2012_2022.csv")

## ...................................
# Ethnicity: Albemarle, 2012-2022 ----
alb_vars_B03003 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "003",
           table = "B03003",
           summary_var = "B03003_001",
           survey = "acs5", 
           cache = TRUE) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B03003_002", # Not Hispanic or Latino
                                  "B03003_003")) # Hispanic or Latino
         )

# Finalizing table:
alb_ethn_2012_2022 <- alb_vars_B03003 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           variable == "B03003_002" ~ "Not Hispanic or Latino",
           variable == "B03003_003" ~ "Hispanic or Latino"
         )) %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(alb_ethn_2012_2022, "data/alb_ethn_2012_2022.csv")

## ..............................................................
# Ethnicity: Charlottesville, Albemarle Combined Table, 2012-2022 ----
combined_ethn_2012_2022 <- rbind(alb_ethn_2012_2022, cville_ethn_2012_2022)

# Create summary variables for the combined counties
combined_ethn_summarize <- combined_ethn_2012_2022 %>% 
  group_by(label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_pop = sum(total_pop))

# Create percentages from estimates
region_ethn_2012_2022 <- combined_ethn_summarize %>% 
  mutate(percent = round(((estimate / total_pop) * 100), digits = 2),
         locality = region_name) %>% 
  select(locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(region_ethn_2012_2022, "data/region_ethn_2012_2022.csv")

## ............................................
# Sex by Age: Table: B01001, 2012 and 2022 ----
# Rows match up for 2012 and 2022 (!)

# Get Sex by Age table for Charlottesville and Albermale, 2012 and 2022
vars_B01001 <- 
  map_df(c(2022, 2012),
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

# Remove total variables
# vars_B01001_filtered <- vars_B01001 %>% 
#   filter(! variable %in% c("B01001_001", "B01001_002", "B01001_026"))
# 
# # Create combined age groups
# sex_age_groups_df <- vars_B01001_filtered %>% 
#   group_by(GEOID, NAME, year) %>% 
#   

# Mapping male and female variables
# Age groups align with final grouping, not original acs vars
age_groups <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", 
                "15 to 19 years", "20 to 24 years", "20 to 24 years", "20 to 24 years", "25 to 34 years", 
                "25 to 34 years", "35 to 44 years", "35 to 44 years", "45 to 54 years", 
                "45 to 54 years", "55 to 59 years", "60 to 64 years", "60 to 64 years", 
                "65 to 74 years", "65 to 74 years", "65 to 74 years", "75 to 84 years", 
                "75 to 84 years", "85 years and over"),
  male_var = c("B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", 
               "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012", 
               "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_017", 
               "B01001_018", "B01001_019", "B01001_020", "B01001_021", "B01001_022", 
               "B01001_023", "B01001_024", "B01001_025"),
  female_var = c("B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", 
                 "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036", 
                 "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B01001_041", 
                 "B01001_042", "B01001_043", "B01001_044", "B01001_045", "B01001_046", 
                 "B01001_047", "B01001_048", "B01001_049")
)

sex_ages_df <- age_groups %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    vars_B01001 %>%
      filter(variable %in% c(male_var, female_var))  %>%
      group_by(variable, GEOID, NAME, year) %>%
      mutate(age_group = age_group,
             sex = case_when(variable %in% c(male_var) ~ "Male",
                             variable %in% c(female_var) ~ "Female"),
             year = year) %>%
      select(GEOID, NAME, variable, estimate, moe, summary_est, year, sex, age_group)
  })

# Create combined age groups
sex_age_groups_df <- sex_ages_df %>%
  group_by(GEOID, NAME, year, sex, age_group) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe = moe, estimate = estimate),
    summary_est = first(summary_est),
    .groups = 'drop')

# Sex by Age: Charlottesville, Albemarle, and Combined tables, 2022 ----
sex_ages_2022 <- sex_age_groups_df %>% 
  filter(year == 2022) %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         total_pop = summary_est,
         locality = NAME,
         label = age_group) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, sex, label, year)

# Sex by Age: Charlottesville, 2022
cville_sex_age_2022 <- sex_ages_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

# Generating CSV:
write_csv(cville_sex_age_2022, "data/cville_sex_age_2022.csv")

# Sex by Age: Albemarle, 2022
alb_sex_age_2022 <- sex_ages_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

# Generating CSV:
write_csv(alb_sex_age_2022, "data/alb_sex_age_2022.csv")

# Sex by Age: Charlottesville, Albemarle Combined Table, 2022
# Create summary variables and percents for the combined counties
combined_sex_age_2022 <- sex_age_groups_df %>% 
  filter(year == 2022) %>% 
  group_by(sex, age_group, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            summary_est = sum(summary_est),
            .groups = 'drop') %>% 
  mutate(percent = round(((estimate / summary_est) * 100), digits = 2),
         locality = region_name,
         total_pop = summary_est) %>% 
  select(locality, estimate, moe, total_pop, percent, sex, age_group, year)

# Generating CSV:
write_csv(combined_sex_age_2022, "data/region_sex_age_2022.csv")

## ............................................
# Age: Albemarle, Charlottesville, and Combined tables, 2012 and 2022 ----
age_2012_2022 <- sex_age_groups_df %>% 
  group_by(GEOID, NAME, year, age_group) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            summary_est = first(summary_est),
            .groups = 'drop') %>%
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         total_pop = summary_est,
         locality = NAME,
         year = year,
         label = age_group) %>%
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Age: Charlottesville, 2012 and 2022
cville_age_2012_2022 <- age_2012_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

# Generating CSV:
write_csv(cville_age_2012_2022, "data/cville_age_2012_2022.csv")

# Age: Albemarle, 2012 and 2022
alb_age_2012_2022 <- age_2012_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

# Generating CSV:
write_csv(alb_age_2012_2022, "data/alb_age_2012_2022.csv")

# Age: Combined Region, 2012 and 2022
combined_age_2012_2022 <- sex_age_groups_df %>% 
  group_by(year, age_group) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            summary_est = first(summary_est),
            .groups = 'drop') %>%
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         total_pop = summary_est,
         locality = region_name,
         year = year,
         label = age_group) %>%
  select(locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(combined_age_2012_2022, "data/region_age_2012_2022.csv")

## ............................................
# Nativity and Citizenship Status: Table: B05001, 2022 ----

# Get ACS data
vars_B05001 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B05001",
  summary_var = "B05001_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable %in% c("B05001_002", # U.S. citizen, born in the United States
                         "B05001_003", # U.S. citizen, born in Puerto Rico or U.S. Island Areas
                         "B05001_004", # U.S. citizen, born abroad of American parent(s)
                         "B05001_005", # U.S. citizen by naturalization
                         "B05001_006")) # Not a U.S. citizen

# Finalizing table:
nativity_2022 <- vars_B05001 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           variable == "B05001_002" ~ "U.S. citizen, born in the United States",
           variable == "B05001_003" ~ "U.S. citizen, born in Puerto Rico or U.S. Island Areas",
           variable == "B05001_004" ~ "U.S. citizen, born abroad of American parent(s)",
           variable == "B05001_005" ~ "U.S. citizen by naturalization",
           variable == "B05001_006" ~ "Not a U.S. citizen"),
         year = 2022) %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, year, label)

# Nativity and Citizenship Status: Charlottesville, 2022
cville_nativity_2022 <- nativity_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

# Generating CSV:
write_csv(cville_nativity_2022, "data/cville_nativity_2022.csv")

# Nativity and Citizenship Status: Albemarle, 2022
alb_nativity_2022 <- nativity_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

# Generating CSV:
write_csv(alb_nativity_2022, "data/alb_nativity_2022.csv")

# Nativity and Citizenship Status: Combined Region, 2022
combined_nativity_2022 <- nativity_2022 %>% 
  group_by(year, label) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_pop = sum(total_pop),
            .groups = 'drop') %>%
  mutate(percent = round(100 * (estimate / total_pop), digits = 2),
         locality = region_name) %>%
  select(locality, estimate, moe, total_pop, percent, year, label)

# Generating CSV:
write_csv(combined_nativity_2022, "data/region_nativity_2022.csv")

## ............................................
# Place of Birth for Foreign-Born Population: Table: B05006, 2022 ----

# Get ACS data
acs_vars_B05006 <- c(
  "Northern Europe" = "B05006_003",
  "Western Europe" = "B05006_013",
  "Southern Europe" = "B05006_021",
  "Eastern Europe" = "B05006_028",
  "Eastern Asia" = "B05006_048",
  "South Central Asia" = "B05006_056",
  "South Eastern Asia" = "B05006_068",
  "Western Asia" = "B05006_079",
  "Eastern Africa" = "B05006_096",
  "Middle Africa" = "B05006_105",
  "Northern Africa" = "B05006_110",
  "Southern Africa" = "B05006_116",
  "Western Africa" = "B05006_119",
  "Oceania" = "B05006_130",
  "Latin America, Caribbean" = "B05006_140",
  "Latin America, Central America" = "B05006_154",
  "Latin America, South America" = "B05006_164",
  "Latin America, Caribbean" = "B05006_140",
  "Northern America" = "B05006_176"
)

vars_B05006 <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = acs_vars_B05006,
  summary_var = "B05006_001", 
  year = 2022, 
  survey = "acs5")
  
# Finalizing table:
birth_place_foreign_2022 <- vars_B05006 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = variable,
         year = 2022) %>% 
  rename(c("total_foreign_born" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_foreign_born, percent, year, label)

# Place of Birth for Foreign-Born Population: Charlottesville, 2022
cville_birth_place_foreign_2022 <- birth_place_foreign_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

# Generating CSV:
write_csv(cville_birth_place_foreign_2022, "data/cville_birth_place_foreign_2022.csv")

# Place of Birth for Foreign-Born Population: Albemarle, 2022
alb_birth_place_foreign_2022 <- birth_place_foreign_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

# Generating CSV:
write_csv(alb_birth_place_foreign_2022, "data/alb_birth_place_foreign_2022.csv")

# Place of Birth for Foreign-Born Population: Combined Region, 2022
combined_birth_place_foreign_2022 <- birth_place_foreign_2022 %>% 
  group_by(year, label) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_foreign_born = sum(total_foreign_born),
            .groups = 'drop') %>%
  mutate(percent = round(100 * (estimate / total_foreign_born), digits = 2),
         locality = region_name) %>%
  select(locality, estimate, moe, total_foreign_born, percent, year, label)

# Generating CSV:
write_csv(combined_birth_place_foreign_2022, "data/region_birth_place_foreign_2022.csv")

## ............................................
# Household Language, Limited and Non-limited English: Table: B16002, 2022 ----

# Get ACS data
acs_vars_B16002 <- c("English only" = "B16002_002", 
                     "Spanish: Limited English speaking household" =  "B16002_004", 
                     "Spanish: Not a limited English speaking household" = "B16002_005",
                     "French, Haitian, or Cajun: Limited English speaking household" = "B16002_007", 
                     "French, Haitian, or Cajun: Not a limited English speaking household" = "B16002_008", 
                     "German or other West Germanic languages: Limited English speaking household" = "B16002_010",
                     "German or other West Germanic languages: Not a limited English speaking household" = "B16002_011", 
                     "Russian, Polish, or other Slavic languages: Limited English speaking household" = "B16002_013", 
                     "Russian, Polish, or other Slavic languages: Not a limited English speaking household" = "B16002_014", 
                     "Other Indo-European languages: Limited English speaking household" = "B16002_016", 
                     "Other Indo-European languages: Not a limited English speaking household" = "B16002_017", 
                     "Korean: Limited English speaking household" = "B16002_019", 
                     "Korean: Not a limited English speaking household" = "B16002_020", 
                     "Chinese (incl. Mandarin, Cantonese): Limited English speaking household" = "B16002_022", 
                     "Chinese (incl. Mandarin, Cantonese): Not a limited English speaking household" = "B16002_023", 
                     "Vietnamese: Limited English speaking household" = "B16002_025", 
                     "Vietnamese: Not a limited English speaking household" = "B16002_026", 
                     "Tagalog (incl. Filipino): Limited English speaking household" = "B16002_028", 
                     "Tagalog (incl. Filipino): Not a limited English speaking household" = "B16002_029", 
                     "Other Asian and Pacific Island languages: Limited English speaking household" = "B16002_031", 
                     "Other Asian and Pacific Island languages: Not a limited English speaking household" = "B16002_032", 
                     "Arabic: Limited English speaking household" = "B16002_034", 
                     "Arabic: Not a limited English speaking household" = "B16002_035", 
                     "Other and unspecified languages: Limited English speaking household" = "B16002_037",
                     "Other and unspecified languages: Not a limited English speaking household" = "B16002_038")

vars_B16002 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  var = acs_vars_B16002,
  summary_var = "B16002_001",
  survey = "acs5",
  year = 2022)

# Finalizing table:
language_2022 <- vars_B16002 %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = variable,
         year = 2022) %>% 
  rename(c("total_households" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_households, percent, year, label)

# Household Language: Charlottesville, 2022
cville_language_2022 <- language_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

# Generating CSV:
write_csv(cville_language_2022, "data/cville_language_2022.csv")

# Household Language: Albemarle, 2022
alb_language_2022 <- language_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

# Generating CSV:
write_csv(alb_language_2022, "data/alb_language_2022.csv")

# Household Language: Combined Region, 2022
combined_language_2022 <- language_2022 %>% 
  group_by(year, label) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_households = sum(total_households),
            .groups = 'drop') %>%
  mutate(percent = round(100 * (estimate / total_households), digits = 2),
         locality = region_name) %>%
  select(locality, estimate, moe, total_households, percent, year, label)

# Generating CSV:
write_csv(combined_language_2022, "data/region_language_2022.csv")

## ............................................
# Disability Status, by Sex and Age: Table: B18101, 2022 ----

# Get ACS data
vars_B18101 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B18101",
  summary_var = "B18101_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(!variable %in% c("B18101_001", "B18101_002", "B18101_003", "B18101_006", "B18101_009", "B18101_012", "B18101_015", "B18101_018", "B18101_021", "B18101_022", "B18101_025", "B18101_028", "B18101_031", "B18101_034", "B18101_037")
  )

# Wrangle data:
disability_2022 <- vars_B18101 %>% 
  mutate(age_sex_category = case_when(
           variable %in% c("B18101_004", "B18101_007") ~ "Male; 17 and under; With a disability",
           variable %in% c("B18101_005", "B18101_008") ~ "Male; 17 and under; No disability",
           variable %in% c("B18101_010") ~ "Male; 18 to 34 years; With a disability",
           variable %in% c("B18101_011") ~ "Male; 18 to 34 years; No disability",
           variable %in% c("B18101_013") ~ "Male; 35 to 64 years; With a disability",
           variable %in% c("B18101_014") ~ "Male; 35 to 64 years; No disability",
           variable %in% c("B18101_016", "B18101_019") ~ "Male; 65 and over; With a disability",
           variable %in% c("B18101_017", "B18101_020") ~ "Male; 65 and over; No disability",
           variable %in% c("B18101_023", "B18101_026") ~ "Female; 17 and under; With a disability",
           variable %in% c("B18101_024", "B18101_027") ~ "Female; 17 and under; No disability",
           variable %in% c("B18101_029") ~ "Female; 18 to 34 years; With a disability",
           variable %in% c("B18101_030") ~ "Female; 18 to 34 years; No disability",
           variable %in% c("B18101_032") ~ "Female; 35 to 64 years; With a disability",
           variable %in% c("B18101_033") ~ "Female; 35 to 64 years; No disability",
           variable %in% c("B18101_035", "B18101_038") ~ "Female; 65 and over; With a disability",
           variable %in% c("B18101_036", "B18101_039") ~ "Female; 65 and over; No disability",
           TRUE ~ "Other"
         )) %>% 
  group_by(GEOID, NAME, age_sex_category) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            summary_est = first(summary_est),
            .groups = 'drop') %>%
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         year = 2022) %>% 
  separate(age_sex_category,
           c("sex", "age_group", "disability_status"),
           sep = "; ") %>% 
  rename(c("total_pop" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, total_pop, percent, sex, age_group, disability_status, year)

# Disability Status: Charlottesville, 2022
cville_disability_2022 <- disability_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

# Generating CSV:
write_csv(cville_disability_2022, "data/cville_disability_2022.csv")

# Disability Status: Albemarle, 2022
alb_disability_2022 <- disability_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

# Generating CSV:
write_csv(alb_disability_2022, "data/alb_disability_2022.csv")

# Disability Status: Combined Region, 2022
combined_disability_2022 <- disability_2022 %>% 
  group_by(sex, age_group, disability_status, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_pop = sum(total_pop),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / total_pop), digits = 2),
         locality = region_name) %>% 
  select(locality, estimate, moe, total_pop, percent, sex, age_group, disability_status, year)

# Generating CSV:
write_csv(combined_disability_2022, "data/region_disability_2022.csv")

# Total with/without Disability, for Charlottesville, Albermarle, 2022 ----
# Get total with disability from above table
disability_total <- disability_2022 %>% 
  group_by(GEOID, locality, disability_status, year) %>% 
  summarize(total_disability = sum(estimate),
            total_dis_moe = moe_sum(moe = moe, estimate = estimate),
            total_pop = first(total_pop)) %>% 
  mutate(total_dis_per = round(100 * (total_disability / total_pop), digits = 2))

# Generating CSV:
write_csv(disability_total, "data/disability_total_2022.csv")

## ............................................
# Disability by Category
# Disability, Hearing Difficulty: Table: B18102, 2022 ----
# Get ACS data:
vars_B18102 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B18102",
  # summary_var = "B18102_001", # summary var is total with a disability here
  survey = "acs5",
  year = 2022) %>%
  filter(variable %in% c("B18102_004", "B18102_007", "B18102_010", "B18102_013", "B18102_016", "B18102_019", 
                         "B18102_023", "B18102_026", "B18102_029", "B18102_032", "B18102_035", "B18102_038")) 

# Wrangle data:
disability_hearing <- vars_B18102 %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(label = "With a hearing difficulty",
         year = 2022) %>% 
  rename("locality" = "NAME")

# Disability, Vision Difficulty: Table: B18103, 2022 ----
# Get ACS data:
vars_B18103 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B18103",
  survey = "acs5",
  year = 2022) %>%
  filter(variable %in% c("B18103_004", "B18103_007", "B18103_010", "B18103_013", "B18103_016", "B18103_019", 
                         "B18103_023", "B18103_026", "B18103_029", "B18103_032", "B18103_035", "B18103_038")) 

# Wrangle data:
disability_vision <- vars_B18103 %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(label = "With a vision difficulty",
         year = 2022) %>% 
  rename("locality" = "NAME")

# Disability, Cognitive Difficulty: Table: B18104, 2022 ----
# Get ACS data:
vars_B18104 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B18104",
  survey = "acs5",
  year = 2022) %>%
  filter(variable %in% c("B18104_004", "B18104_007", "B18104_010", "B18104_013", "B18104_016",
                         "B18104_020", "B18104_023", "B18104_026", "B18104_029", "B18104_032")) 

# Wrangle data:
disability_cognitive <- vars_B18104 %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(label = "With a cognitive difficulty",
         year = 2022) %>% 
  rename("locality" = "NAME")

# Disability, Ambulatory Difficulty: Table: B18105, 2022 ----
# Get ACS data:
vars_B18105 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B18105",
  survey = "acs5",
  year = 2022) %>%
  filter(variable %in% c("B18105_004", "B18105_007", "B18105_010", "B18105_013", "B18105_016", 
                         "B18105_020", "B18105_023", "B18105_026", "B18105_029", "B18105_032"))

# Wrangle data:
disability_ambulatory <- vars_B18105 %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(label = "With an ambulatory difficulty",
         year = 2022) %>% 
  rename("locality" = "NAME")

# Disability, Self-care Difficulty: Table: B18106, 2022 ----
# Get ACS data:
vars_B18106 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B18106",
  survey = "acs5",
  year = 2022) %>%
  filter(variable %in% c("B18106_004", "B18106_007", "B18106_010", "B18106_013", "B18106_016", 
                         "B18106_020", "B18106_023", "B18106_026", "B18106_029", "B18106_032"))

# Wrangle data:
disability_self_care <- vars_B18106 %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(label = "With a self-care difficulty",
         year = 2022) %>% 
  rename("locality" = "NAME")

# Disability, Independent Living Difficulty: Table: B18107, 2022 ----
# Get ACS data:
vars_B18107 <- get_acs(
  geography = "county",
  county = county_codes,
  cache = TRUE,
  state = "VA",
  table = "B18107",
  survey = "acs5",
  year = 2022) %>%
  filter(variable %in% c("B18107_004", "B18107_007", "B18107_010", "B18107_013", 
                         "B18107_017", "B18107_020", "B18107_023", "B18107_026"))

# Wrangle data:
disability_ind_living <- vars_B18107 %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(label = "With an independent living difficulty",
         year = 2022) %>% 
  rename("locality" = "NAME")


# Binding All Disability Category Total Tables Together ----
disability_all <- rbind(disability_hearing,
                        disability_vision, 
                        disability_cognitive,
                        disability_ambulatory,
                        disability_self_care,
                        disability_ind_living)

# Get total counts and create percents
disability_all <- disability_all %>% 
  group_by(GEOID, locality, year) %>% 
  mutate(total_count_disabled = sum(estimate),
         percent = round(100 * (estimate / total_count_disabled), digits = 2)) %>% 
  select(GEOID, locality, estimate, moe, total_count_disabled, percent, label, year)

# Disability Categories: Charlottesville, 2022 ----
cville_disability_cat_2022 <- disability_all %>% 
  filter(locality == "Charlottesville city, Virginia")

# Generating CSV:
write_csv(cville_disability_cat_2022, "data/cville_disability_cat_2022.csv")

# Disability Categories: Albemarle, 2022 ----
alb_disability_cat_2022 <- disability_all %>% 
  filter(locality == "Albemarle County, Virginia")

# Generating CSV:
write_csv(alb_disability_cat_2022, "data/alb_disability_cat_2022.csv")

# Disability Categories: Combined Region, 2022 ----
combined_disability_cat_2022 <- disability_all %>% 
  group_by(label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_count_disabled = sum(total_count_disabled),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / total_count_disabled), digits = 2),
         locality = region_name) %>% 
  select(locality, estimate, moe, total_count_disabled, percent, label, year)

# Generating CSV:
write_csv(combined_disability_cat_2022, "data/region_disability_cat_2022.csv")
