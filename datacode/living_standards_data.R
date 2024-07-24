# R Script for pulling and examining living standards data
# Authors: Henry DeMarco, Beth Mitchell
# Date Created: June 17, 2024
# Last Updated: July 23, 2024

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

# Living Standards TOC ----
# AHDI MEASURES, COUNTY & TRACT LEVEL
# Median personal earnings
# - Source: ACS Table B20002
# Median personal earnings, method for aggregating combined region
# - Source: ACS Table B20001
# OTHER MEASURES
# Median personal earnings by sex and race (COUNTY)
# - Source: ACS Table B20017A-I
# Median household income (COUNTY & TRACT)
# - Sources: ACS Table B19013 & B19013A-I (BY RACE, COUNTY ONLY)
# - Swap: B19001 & B19001A-I
# ALICE Households (COUNTY) 
# - ALICE Thresholds over time 2012-2022
# - ALICE households, all and by race/ethnicity 2022 
# - Source: https://www.unitedforalice.org/state-overview/Virginia
# Median Gross Rent
# - Tract (2022)
# - County (2012-2022)
# - NEED METHOD FOR REGION
# - Source: ACS Table B25064
# Rent-burdened households, 2022 (COUNTY & TRACT)
# - Source: ACS Table B25070
# Tenure/Home Ownership, 2022 (COUNTY & TRACT)
# - Source: ACS Table B25003
# Tenure/Home Ownership by Race, 2012, 2022 (COUNTY)
# - Source: ACS Table B25003A-I

# Load packages
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
region_name <- "Combined Region"

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

## .................................................
# Median Personal Earnings (AHDI MEASURE): B20002 (NEED TO UPDATE TO B20001) ----
# Table: B20002 (Median Earnings in the Past 12 Months, Inflation-Adjusted)

# Median Personal Earnings: County & tract
# Get ACS data
AHDI_vars_B20002 <- c("Median Earnings; All" = "B20002_001",
                     "Median Earnings; Male" = "B20002_002",
                     "Median Earnings; Female" = "B20002_003")

acs_B20002_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = AHDI_vars_B20002,
  year = year, 
  survey = "acs5")

acs_B20002_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  var = AHDI_vars_B20002,
  year = year, 
  survey = "acs5")

# Wrangle tables:
med_earnings_county <- acs_B20002_county %>% 
  separate(variable, into=c("label","group"), sep="; ", remove=FALSE) %>% 
  mutate(year = year) %>% 
  rename("locality" = "NAME") %>% 
  select(GEOID, locality, label, group, estimate, moe, year)

med_earnings_tract <- acs_B20002_tract %>% 
  separate(variable, into=c("label","group"), sep="; ", remove=FALSE) %>% 
  mutate(year = year) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, label, group, estimate, moe, year)

# Join tract names
med_earnings_tract <- med_earnings_tract %>% 
  left_join(tract_names)

# Median Personal Earnings: Charlottesville, county & tract ----
cville_med_earnings_county <- med_earnings_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_med_earnings_county, paste0("data/cville_med_earnings_county", "_", year, ".csv"))

cville_med_earnings_tract <- med_earnings_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_med_earnings_tract, paste0("data/cville_med_earnings_tract", "_", year, ".csv"))

# Median Personal Earnings: Albemarle, county & tract ----
alb_med_earnings_county <- med_earnings_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_med_earnings_county, paste0("data/alb_med_earnings_county", "_", year, ".csv"))

alb_med_earnings_tract <- med_earnings_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_med_earnings_tract, paste0("data/alb_med_earnings_tract", "_", year, ".csv"))

# Median Personal Earnings: Combined Region, B20001 ----
# ACS Table B20001 Sex by Earning
# Get ACS data
acs_B20001_county <- get_acs(geography = "county",
                             state = "51",
                             county = county_codes,
                             table = "B20001", 
                             summary_var = "B20001_001",
                             year = year,
                             survey = "acs5",
                             cache_table = TRUE) 

# prep data
earning_ranges <- tibble::tibble(
  earning_bin = c("1-2,499", "2,500-4,999", "5,000-7,499", "7,500-9,999", 
                "10,000-12,499", "12,500-14,999", "15,000-17,499", "17,500-19,999", 
                "20,000-22,499", "22,500-24,999", "25,000-29,999", "30,000-34,999", 
                "35,000-39,999", "40,000-44,999", "45,000-49,999", "50,000-54,999", 
                "55,000-64,999", "65,000-74,999", "75,000-99,999", "100,000-300,000"),
  male_var = c("B20001_003", "B20001_004", "B20001_005", "B20001_006", "B20001_007", 
               "B20001_008", "B20001_009", "B20001_010", "B20001_011", "B20001_012", 
               "B20001_013", "B20001_014", "B20001_015", "B20001_016", "B20001_017", 
               "B20001_018", "B20001_019", "B20001_020", "B20001_021", "B20001_022"),
  female_var = c("B20001_024", "B20001_025", "B20001_026","B20001_027", "B20001_028", 
                 "B20001_029", "B20001_030", "B20001_031", "B20001_032", "B20001_033", 
                 "B20001_034", "B20001_035", "B20001_036", "B20001_037", "B20001_038", 
                 "B20001_039", "B20001_040", "B20001_041", "B20001_042", "B20001_043")
)

earnings_sex_df <- earning_ranges %>%
  pmap_dfr(function(earning_bin, male_var, female_var) {
    acs_B20001_county %>%
      filter(variable %in% c(male_var, female_var))  %>%
      group_by(variable, GEOID, NAME) %>%
      mutate(earning_bin = as.factor(earning_bin),
             sex = case_when(variable %in% c(male_var) ~ "Male",
                             variable %in% c(female_var) ~ "Female")) %>%
      select(GEOID, NAME, variable, estimate, moe, summary_est, sex, earning_bin)
  })

# Collapse sex
earnings_df <- earnings_sex_df %>%
  group_by(GEOID, NAME, earning_bin) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe = moe, estimate = estimate),
    summary_est = first(summary_est),
    .groups = 'drop') %>% 
  separate(earning_bin, into = c("bin_start", "bin_end"), 
           sep = "-", remove = FALSE)

# Add bin start and end
earnings_df <- earnings_df  %>% 
  mutate(across(starts_with("bin"), ~as.numeric(str_remove(.x, ","))))

# Derive steps to aggregate and estimate
# choose localities for combined region
choose <- c("51003", "51540")

# Make a function ----
agg_earning_median <- function(df, loc){
  agg_earning_range <- df %>% 
    filter(GEOID %in% loc) %>% 
    group_by(earning_bin, bin_start, bin_end) %>% 
    summarize(estimate = sum(estimate),
              total = sum(summary_est)) %>% 
    ungroup() %>% 
    mutate(cum_sum = cumsum(estimate),
           cum_per = cum_sum/total)
  
  midpoint <- agg_earning_range$total[1]/2
  index_bin <- which(agg_earning_range$cum_sum > midpoint)[1]
  range_reach <- midpoint-agg_earning_range$cum_sum[index_bin-1]
  range_prop <- range_reach / agg_earning_range$estimate[index_bin]
  income_add <- range_prop * (agg_earning_range$bin_end[index_bin] + 1 - agg_earning_range$bin_start[index_bin])
  median_earn <- agg_earning_range$bin_end[index_bin-1] + income_add
  return(median_earn)
}

# apply function 
med_earn_combined <- agg_earning_median(earnings_df, choose)

# Create data frame
region_med_earnings <- data.frame(
  region_fips = paste(choose, collapse = ","),
  locality = region_name,
  med_earnings_est = med_earn_combined,
  year = year
)

# Generating CSV:
write_csv(region_med_earnings, paste0("data/region_med_earnings", "_", year, ".csv"))

## ....................................................
# Median Personal Earnings by Sex and Race: B20017 ----
# Median Personal Earnings by Sex and Race (populations statistically significant): County
# Get ACS data
vars_B20017 <- c("Median Earnings; All; White" = "B20017A_001",
                      "Median Earnings; Male; White" = "B20017A_002",
                      "Median Earnings; Female; White" = "B20017A_005",
                      "Median Earnings; All; Black" = "B20017B_001",
                      "Median Earnings; Male; Black" = "B20017B_002",
                      "Median Earnings; Female; Black" = "B20017B_005",
                      "Median Earnings; All; Asian" = "B20017D_001",
                      "Median Earnings; Male; Asian" = "B20017D_002",
                      "Median Earnings; Female; Asian" = "B20017D_005",
                      "Median Earnings; All; Mutiracial" = "B20017G_001",
                      "Median Earnings; Male; Mutiracial" = "B20017G_002",
                      "Median Earnings; Female; Mutiracial" = "B20017G_005",
                      "Median Earnings; All; White, Not Hispanic or Latino" = "B20017H_001",
                      "Median Earnings; Male; White, Not Hispanic or Latino" = "B20017H_002",
                      "Median Earnings; Female; White, Not Hispanic or Latino" = "B20017H_005",
                      "Median Earnings; All; Hispanic or Latino" = "B20017I_001",
                      "Median Earnings; Male; Hispanic or Latino" = "B20017I_002",
                      "Median Earnings; Female; Hispanic or Latino" = "B20017I_005")

acs_B20017_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = vars_B20017,
  year = year, 
  survey = "acs5")

# Wrangle table:
med_earnings_race_county <- acs_B20017_county %>% 
  separate(variable, into=c("label","sex", "race"), sep="; ", remove=FALSE) %>% 
  mutate(year = year) %>% 
  rename("locality" = "NAME") %>% 
  select(GEOID, locality, label, sex, race, estimate, moe, year)

# Median Personal Earnings by Sex and Race: Charlottesville, county ----
cville_med_earnings_race_county <- med_earnings_race_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_med_earnings_race_county, paste0("data/cville_med_earnings_race_county", "_", year, ".csv"))

# Median Personal Earnings by Sex and Race: Albemarle, county ----
alb_med_earnings_race_county <- med_earnings_race_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_med_earnings_race_county, paste0("data/alb_med_earnings_race_county", "_", year, ".csv"))

## ...................................
# Median Household Income: B19001 (All) & B19001A-I (By Race/Ethnicity) UPDATED METHOD ----
# A good explanation of finding the median across aggregated geographies
# https://dof.ca.gov/wp-content/uploads/sites/352/Forecasting/Demographics/Documents/How_to_Recalculate_a_Median.pdf

# Median Household Income: ACS Table B19001, County ----
# Get ACS data
acs_B19001_county <- get_acs(geography = "county",
                               state = "51",
                               county = county_codes,
                               table = "B19001", 
                               summary_var = "B19001_001",
                               year = year,
                               survey = "acs5",
                               cache_table = TRUE) 

# acs_B19001_tract <- get_acs(geography = "tract",
#                              state = "51",
#                              county = county_codes,
#                              table = "B19001", 
#                              summary_var = "B19001_001",
#                              year = year,
#                              survey = "acs5",
#                              cache_table = TRUE) 
# 

# prep data
# income range function
income_ranges_func <- function(df){
  income_ranges <- df %>% 
    filter(variable != "B19001_001") %>%
    mutate(income_bin = as.factor(variable)) %>%
    mutate(income_bin = fct_recode(income_bin,
                                   "2500_9999" = "B19001_002",
                                   "10000_14999" = "B19001_003",
                                   "15000_19999" = "B19001_004",
                                   "20000_24999" = "B19001_005",
                                   "25000_29999" = "B19001_006",
                                   "30000_34999" = "B19001_007",
                                   "35000_39999" = "B19001_008",
                                   "40000_44999" = "B19001_009",
                                   "45000_49999" = "B19001_010",
                                   "50000_59999" = "B19001_011",
                                   "60000_74999" = "B19001_012",
                                   "75000_99999" = "B19001_013",
                                   "100000_124999" = "B19001_014",
                                   "125000_149999" = "B19001_015",
                                   "150000_199999" = "B19001_016",
                                   "200000_300000" = "B19001_017" 
    )) %>% 
    separate(income_bin, into = c("bin_start", "bin_end"), 
             sep = "_", remove = FALSE) %>% 
    mutate(across(starts_with("bin"), as.numeric))

    return(income_ranges)
}

# Run income ranges function for county and tract
income_ranges_county <- income_ranges_func(acs_B19001_county)
# income_ranges_tract <- income_ranges_func(acs_B19001_tract)

# income_ranges_county <- acs_B19001_county %>% 
#   filter(variable != "B19001_001") %>%
#   mutate(income_bin = as.factor(variable)) %>%
#   mutate(income_bin = fct_recode(income_bin,
#                                  "2500_9999" = "B19001_002",
#                                  "10000_14999" = "B19001_003",
#                                  "15000_19999" = "B19001_004",
#                                  "20000_24999" = "B19001_005",
#                                  "25000_29999" = "B19001_006",
#                                  "30000_34999" = "B19001_007",
#                                  "35000_39999" = "B19001_008",
#                                  "40000_44999" = "B19001_009",
#                                  "45000_49999" = "B19001_010",
#                                  "50000_59999" = "B19001_011",
#                                  "60000_74999" = "B19001_012",
#                                  "75000_99999" = "B19001_013",
#                                  "100000_124999" = "B19001_014",
#                                  "125000_149999" = "B19001_015",
#                                  "150000_199999" = "B19001_016",
#                                  "200000_300000" = "B19001_017" 
#   )) %>% 
#   separate(income_bin, into = c("bin_start", "bin_end"), 
#            sep = "_", remove = FALSE) %>% 
#   mutate(across(starts_with("bin"), as.numeric))


# Derive steps to aggregate and estimate
# Median Household Income, Combined Region ----

# choose localities for combined region
choose <- c("51003", "51540")

# generate aggregate sums
aggregate_range_county <- income_ranges_county %>% 
  filter(GEOID %in% choose) %>% 
  group_by(income_bin, bin_start, bin_end) %>% 
  summarize(estimate = sum(estimate),
            total = sum(summary_est)) %>% 
  ungroup() %>% 
  mutate(cum_sum = cumsum(estimate),
         cum_per = cum_sum/total)

# identify midpoint observation
midpoint <- aggregate_range_county$total[1]/2

# find index of row for which cum_sum contains the midpoint
index_bin <- which(aggregate_range_county$cum_sum > midpoint)[1]

# range_reach = midpoint - cum_sum[x-1]
#   how many observations into the identified cum_sum do we need to reach
range_reach <- midpoint-aggregate_range_county$cum_sum[index_bin-1]

# range_prop = range_reach / estimate[x]
#   what proportion of the total of the identified income_bin is this 
range_prop <- range_reach / aggregate_range_county$estimate[index_bin]

# income_add = range_prop * (bin_end[x] - bin_start[x] + 1) 
#   assuming uniform distribution in the range, how far into the income bin is this proportion
income_add <- range_prop * (aggregate_range_county$bin_end[index_bin] + 1 - aggregate_range_county$bin_start[index_bin])

# median = bin_end[x-1] + income_add 
#   add this income to the top range of the prior income bin
median <- aggregate_range_county$bin_end[index_bin-1] + income_add

# Compare to county estimates from B19013
# Albemarle: estimate 97564, table 97708
# Charlottesville: estimate 67489, table 67177

# get all tables
# get all race-ethnicity specific tables
tables <- c("B19001", "B19001A", "B19001B", "B19001C", "B19001D", "B19001E",
            "B19001F", "B19001G", "B19001H", "B19001I")

acs_B19001_race_county <- map(tables,
                         ~get_acs(geography = "county",
                                  state = "51",
                                  county = county_codes,
                                  table = .x, 
                                  #summary_var = "B19001_001",
                                  year = year,
                                  survey = "acs5",
                                  cache_table = TRUE) %>% 
                           mutate(table = .x)
)

# prep all data ----
# perform processing across list
income_tables_county <- acs_B19001_race_county %>% 
  map(~filter(., !(str_detect(variable, fixed("_001")))) %>% 
        mutate(variable = str_replace(variable, "B19001[:upper:]", "B19001"),
               income_bin = as.factor(variable),
               income_bin = fct_recode(income_bin,
                                       "2500_9999" = "B19001_002",
                                       "10000_14999" = "B19001_003",
                                       "15000_19999" = "B19001_004",
                                       "20000_24999" = "B19001_005",
                                       "25000_29999" = "B19001_006",
                                       "30000_34999" = "B19001_007",
                                       "35000_39999" = "B19001_008",
                                       "40000_44999" = "B19001_009",
                                       "45000_49999" = "B19001_010",
                                       "50000_59999" = "B19001_011",
                                       "60000_74999" = "B19001_012",
                                       "75000_99999" = "B19001_013",
                                       "100000_124999" = "B19001_014",
                                       "125000_149999" = "B19001_015",
                                       "150000_199999" = "B19001_016",
                                       "200000_300000" = "B19001_017")
        ) %>% 
        separate(income_bin, into = c("bin_start", "bin_end"), 
                 sep = "_", remove = FALSE) %>% 
        mutate(across(starts_with("bin"), as.numeric)) %>% 
        group_by(GEOID, NAME) %>% 
        mutate(summary_est = sum(estimate)) %>% 
        ungroup()
  )

# Make a function ----
aggregate_median <- function(aggregate_range, df, loc){
  aggregate_range <- df %>% 
    filter(GEOID %in% loc) %>% 
    group_by(income_bin, bin_start, bin_end) %>% 
    summarize(estimate = sum(estimate),
              total = sum(summary_est)) %>% 
    ungroup() %>% 
    mutate(cum_sum = cumsum(estimate),
           cum_per = cum_sum/total)
  
  midpoint <- aggregate_range$total[1]/2
  index_bin <- which(aggregate_range$cum_sum > midpoint)[1]
  range_reach <- midpoint-aggregate_range$cum_sum[index_bin-1]
  range_prop <- range_reach / aggregate_range$estimate[index_bin]
  income_add <- range_prop * (aggregate_range$bin_end[index_bin] + 1 - aggregate_range$bin_start[index_bin])
  median <- aggregate_range$bin_end[index_bin-1] + income_add
  return(median)
}

# apply function 
median_overall <- aggregate_median(aggregate_range_county,income_tables_county[[1]], choose)
median_whitealone <- aggregate_median(aggregate_range_county, income_tables_county[[2]], choose)
median_blackalone <- aggregate_median(aggregate_range_county, income_tables_county[[3]], choose)
median_aianalone <- aggregate_median(aggregate_range_county, income_tables_county[[4]], choose)
median_asianalone <- aggregate_median(aggregate_range_county, income_tables_county[[5]], choose)
median_nhpialone <- aggregate_median(aggregate_range_county, income_tables_county[[6]], choose)
median_otheralone <- aggregate_median(aggregate_range_county, income_tables_county[[7]], choose)
median_multialone <- aggregate_median(aggregate_range_county, income_tables_county[[8]], choose)
median_whitenhalone <- aggregate_median(aggregate_range_county, income_tables_county[[9]], choose)
median_hispanic <- aggregate_median(aggregate_range_county, income_tables_county[[10]], choose)

# why no nhpi?
# income_tables[[6]] %>% view()

# combine into data frame
medians_bygroup <- data.frame(mget(ls(pattern="median_"))) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "median_hh_inc") %>% 
  mutate(region_FIPS = paste(choose, collapse = ","),
         locality = region_name)


# Median Household Income by Race: B19013 ----

# Median Household Income by Race: County & tract
# Get ACS data
vars_B19013 <- c("Median Household Income; White" = "B19013A_001", 
                 "Median Household Income; Black" = "B19013B_001", 
                 "Median Household Income; American Indian and Alaska Native" = "B19013C_001", 
                 "Median Household Income; Asian" = "B19013D_001", 
                 "Median Household Income; NHPI" = "B19013E_001",
                 "Median Household Income; Other" = "B19013F_001", 
                 "Median Household Income; Multiracial" = "B19013G_001", 
                 "Median Household Income; White, Not Hispanic or Latino" = "B19013H_001", 
                 "Median Household Income; Hispanic or Latino" = "B19013I_001",
                 "Median Household Income; All Households" = "B19013_001")

acs_B19013_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = vars_B19013,
  year = year, 
  survey = "acs5")

acs_B19013_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  var = vars_B19013,
  year = year, 
  survey = "acs5")

# Wrangle tables:
med_hhinc_county <- acs_B19013_county %>% 
  separate(variable, into=c("label","group"), sep="; ", remove=FALSE) %>%
  mutate(year = year) %>% 
  rename("locality" = "NAME") %>% 
  select(GEOID, locality, label, group, estimate, moe, year)

med_hhinc_tract <- acs_B19013_tract %>% 
  separate(variable, into=c("label","group"), sep="; ", remove=FALSE) %>%
  mutate(year = year) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, label, group, estimate, moe, year)

# Join tract names
med_hhinc_tract <- med_hhinc_tract %>% 
  left_join(tract_names)

# Median Household Income: Charlottesville, county & tract ----
cville_med_hhinc_county <- med_hhinc_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_med_hhinc_county, paste0("data/cville_med_hhinc_county", "_", year, ".csv"))

cville_med_hhinc_tract <- med_hhinc_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_med_hhinc_tract, paste0("data/cville_med_hhinc_tract", "_", year, ".csv"))

# Median Household Income: Albemarle, county & tract ----
alb_med_hhinc_county <- med_hhinc_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_med_hhinc_county, paste0("data/alb_med_hhinc_county", "_", year, ".csv"))

alb_med_hhinc_tract <- med_hhinc_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_med_hhinc_tract, paste0("data/alb_med_hhinc_tract", "_", year, ".csv"))

## ..........................................
# ALICE Households & Threshold 2010-2022 -----
# Get ALICE Data (2010-2022): https://www.unitedforalice.org/state-overview/Virginia

url <- "https://www.unitedforalice.org/Attachments/StateDataSheet/2024%20ALICE%20-%20Virginia%20Data%20Sheet.xlsx"

# Download file
download.file(url, destfile="data/tempdata/2024_ALICE_Virginia_Data_Sheet.xlsx", method="libcurl")

# Read data - County (2010-2022)
ALICE_sheet_county <- read_excel("data/tempdata/2024_ALICE_Virginia_Data_Sheet.xlsx", sheet = "County 2010-2022") %>% 
  clean_names()

# ALICE Thresholds, 2010-2022
ALICE_threshold_county <- ALICE_sheet_county %>% 
  filter(str_detect(geo_id2, paste0("51", county_codes, collapse = '|'))) %>% 
  rename(GEOID = geo_id2,
         locality = geo_display_label) %>% 
  select(GEOID, locality, year, alice_threshold_hh_under_65, alice_threshold_hh_65_years_and_over, source_american_community_survey, county )

# ALICE Thresholds, 2010-2022: Charlottesville ----
cville_ALICE_threshold_2010_2022 <- ALICE_threshold_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_ALICE_threshold_2010_2022, paste0("data/cville_ALICE_threshold_2010_2022.csv"))

# ALICE Thresholds, 2010-2022: Albemarle ----
alb_ALICE_threshold_2010_2022 <- ALICE_threshold_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_ALICE_threshold_2010_2022, paste0("data/alb_ALICE_threshold_2010_2022.csv"))

# ALICE households, 2022 ----
ALICE_households_county <- ALICE_sheet_county %>% 
  filter(str_detect(geo_id2, paste0("51", county_codes, collapse = '|')) &
           year == 2022) %>% 
  rename(GEOID = geo_id2,
         locality = geo_display_label) %>% 
  gather(level, number, poverty_households:above_alice_households) %>% 
  mutate(percent = round(100 * (number / households), digits = 2),
         group = "All") %>% 
  select(GEOID, locality, level, group, number, households, percent, year)

# Read in ALICE Race/Ethnicity 
# CSV downloaded after filtering for each county
ALICE_sheet_race_cville <- read_excel("data/tempdata/households-by-race-cville-2022.xlsx") %>% 
  clean_names() %>% 
  gather(level, number, above:poverty) %>% 
  mutate(percent = round(100 * (number / number_households), digits = 2),
         GEOID = "51540",
         locality = "Charlottesville city, Virginia",
         year = 2022) %>% 
  rename(group = name,
         households = number_households) %>% 
  select(GEOID, locality, level, group, number, households, percent, year)

ALICE_sheet_race_alb <- read_excel("data/tempdata/households-by-race-albemarle.xlsx") %>% 
  clean_names() %>% 
  gather(level, number, above:poverty) %>% 
  mutate(percent = round(100 * (number / number_households), digits = 2),
         GEOID = "51003",
         locality = "Albemarle County, Virginia",
         year = 2022) %>% 
  rename(group = name,
         households = number_households) %>% 
  select(GEOID, locality, level, group, number, households, percent, year)

# Combine ALICE household total and race tables
ALICE_households_by_race <- rbind(ALICE_households_county, ALICE_sheet_race_cville, ALICE_sheet_race_alb)

# ALICE household by Race: Charlottesville ----
cville_ALICE_households_by_race_2022 <- ALICE_households_by_race %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_ALICE_households_by_race_2022, paste0("data/cville_ALICE_households_by_race_2022.csv"))

# ALICE household by Race: Albemarle ----
alb_ALICE_households_by_race_2022 <- ALICE_households_by_race %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_ALICE_households_by_race_2022, paste0("data/alb_ALICE_households_by_race_2022.csv"))

# ALICE household by Race: Combined Region ----
region_fips <- as.list(unique(ALICE_households_by_race$GEOID))

region_ALICE_households_by_race_2022 <- ALICE_households_by_race %>% 
  group_by(group, level, year) %>% 
  summarize(number = sum(number),
            households = sum(households),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (number / households), digits = 2),
         locality = region_name,
         region_fips = paste(region_fips, collapse = ";")) %>% 
  select(region_fips, locality, group, level, number, households, percent, year)

write_csv(region_ALICE_households_by_race_2022, "data/region_ALICE_households_by_race_2022.csv")

# Read data - Subcounty (2022) - NOT USED
# ALICE_sheet_subcounty <- read_excel("data/tempdata/2024_ALICE_Virginia_Data_Sheet.xlsx", sheet = "Subcounty, Places, Zip Codes") %>% 
#   clean_names()

# ALICE_data_subcounty <- ALICE_sheet_subcounty %>% 
#   filter(str_detect(geo_id2, paste0("51", county_codes, collapse = '|'))) %>% 
#   separate(geo_display_label, into=c("sub_county","locality", "state"), sep=", ", remove=FALSE) %>% 
#   rename(GEOID = geo_id2) %>% 
#   gather(level, number, poverty_households:above_alice_households) %>% 
#   mutate(percent = round(100 * (number / households), digits = 2)) %>% 
#   select(GEOID, locality, sub_county, year, level, number, households, percent, source_american_community_survey)

## .....................................................................................
# Rent-burdened households: B25070 ----

# get acs data
acs_B25070_county <- get_acs(geography = "county",
                             table = "B25070",
                             state = "VA",
                             county = county_codes,
                             summary_var = "B25070_001",
                             survey = "acs5",
                             year = year,
                             cache_table = TRUE)

acs_B25070_tract <- get_acs(geography = "tract",
                             table = "B25070",
                             state = "VA",
                             county = county_codes,
                             summary_var = "B25070_001",
                             survey = "acs5",
                             year = year,
                             cache_table = TRUE)

# wrangle
rent_burden_county <- acs_B25070_county %>% 
  filter(!variable %in% c("B25070_001", "B25070_011")) %>% 
  mutate(level = case_when(variable %in% c("B25070_002", "B25070_003", "B25070_004", "B25070_005", "B25070_006") ~ "Not burdened; Less than 30% household income",
                           variable %in% c("B25070_007", "B25070_008", "B25070_009") ~ "Burdened; 30% to 49% household income",
                           variable == "B25070_010" ~ "Severely Burdened; Over 50% household income")) %>% 
  group_by(GEOID, NAME, level) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            summary_est = first(summary_est),
            .groups = 'drop') %>% 
  separate(level, into=c("level","rent_burden"), sep="; ", remove=FALSE) %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         total_units = summary_est,
         locality = NAME,
         year = year) %>%
  select(GEOID, locality, estimate, moe, total_units, percent, level, rent_burden, year)

rent_burden_tract <- acs_B25070_tract %>% 
  filter(!variable %in% c("B25070_001", "B25070_011")) %>% 
  mutate(level = case_when(variable %in% c("B25070_002", "B25070_003", "B25070_004", "B25070_005", "B25070_006") ~ "Not burdened; Less than 30% household income",
                           variable %in% c("B25070_007", "B25070_008", "B25070_009") ~ "Burdened; 30% to 49% household income",
                           variable == "B25070_010" ~ "Severely Burdened; Over 50% household income")) %>% 
  group_by(GEOID, NAME, level) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            summary_est = first(summary_est),
            .groups = 'drop') %>% 
  separate(level, into=c("level","rent_burden"), sep="; ", remove=FALSE) %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         total_units = summary_est,
         year = year) %>%
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, estimate, moe, total_units, percent, level, rent_burden, year)

# Join tract names
rent_burden_tract <- rent_burden_tract %>% 
  left_join(tract_names)

# Rent-burdened households: Charlottesville, county & tract ----
cville_rent_burden_county <- rent_burden_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_rent_burden_county, paste0("data/cville_rent_burden_county", "_", year, ".csv"))

cville_rent_burden_tract <- rent_burden_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_rent_burden_tract, paste0("data/cville_rent_burden_tract", "_", year, ".csv"))

# Rent-burdened households: Albemarle, county & tract ----
alb_rent_burden_county <- rent_burden_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_rent_burden_county, paste0("data/alb_rent_burden_county", "_", year, ".csv"))

alb_rent_burden_tract <- rent_burden_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_rent_burden_tract, paste0("data/alb_rent_burden_tract", "_", year, ".csv"))

# Rent-burdened households: Region, Combined Table ----
region_rent_burden <- rent_burden_county %>% 
  group_by(level, rent_burden, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_units = sum(total_units),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / total_units), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>% 
  select(region_fips, locality, estimate, moe, total_units, percent, level, rent_burden, year)

write_csv(region_rent_burden, paste0("data/region_rent_burden", "_", year, ".csv"))

## ...........................................................
# Median Gross Rent, 2012-2022: B25064 (Incomplete) ----
# Table: B25064 (Median Gross Rent (Dollars))

# Get ACS
# County 2012-2022
acs_B25064_county <- map_df(2022:2012,
                            ~ get_acs(geography = "county",
                                      year = .x,
                                      state = "VA",
                                      county = county_codes,
                                      table = "B25064",
                                      survey = "acs5", 
                                      cache = TRUE) %>%
                              mutate(year = .x))

# Tract 2022 only 
acs_B25064_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  table = "B25064",
  year = year, 
  survey = "acs5")

# Tracts have changed during this time frame - NOT USED
# acs_B25064_tract <- map_df(2022:2012,
#                             ~ get_acs(geography = "tract",
#                                       year = .x,
#                                       state = "VA",
#                                       county = county_codes,
#                                       table = "B25064",
#                                       survey = "acs5", 
#                                       cache = TRUE) %>%
#                               mutate(year = .x))

# Finalizing tables
gross_rent_county_2012_2022 <- acs_B25064_county %>% 
  mutate(label = case_when(variable == "B25064_001" ~ "Median Gross Rent"),
         locality = NAME) %>% 
  select(GEOID, locality, label, estimate, moe, year)

gross_rent_tract_2022 <- acs_B25064_tract %>% 
  mutate(label = case_when(variable == "B25064_001" ~ "Median Gross Rent"),
         year = year) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, label, estimate, moe, year)

# Join tract names
gross_rent_tract_2022 <- gross_rent_tract_2022 %>% 
  left_join(tract_names)

# Median Gross Rent: Charlottesville, county (2012-2022) & tract (2022) ----
cville_gross_rent_county_2012_2022 <- gross_rent_county_2012_2022 %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_gross_rent_county_2012_2022, paste0("data/cville_gross_rent_county_2012_2022.csv"))

cville_gross_rent_tract_2022 <- gross_rent_tract_2022 %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_gross_rent_tract_2022, paste0("data/cville_gross_rent_tract_2022.csv"))

# Median Gross Rent: Albemarle, county (2012-2022) & tract (2022) ----
alb_gross_rent_county_2012_2022 <- gross_rent_county_2012_2022 %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_gross_rent_county_2012_2022, paste0("data/alb_gross_rent_county_2012_2022.csv"))

alb_gross_rent_tract_2022 <- gross_rent_tract_2022 %>% 
  filter(locality == "Albemarle County")

write_csv(alb_gross_rent_tract_2022, paste0("data/alb_gross_rent_tract_2022.csv"))

## .....................................
# Tenure (Own & Rent): B25003 ----
# Table: B25003 (Tenure)
# Get ACS data - County and tract
acs_B25003_county <- get_acs(geography = "county",
                            county = county_codes,
                            state = "VA",
                            table = "B25003",
                            summary_var = "B25003_001",
                            survey = "acs5",
                            year = year) %>% 
  filter(variable %in% c("B25003_002", "B25003_003"))

acs_B25003_tract <- get_acs(geography = "tract",
                            county = county_codes,
                            state = "VA",
                            table = "B25003",
                            summary_var = "B25003_001",
                            survey = "acs5",
                            year = year) %>% 
  filter(variable %in% c("B25003_002", "B25003_003"))

# Wrangle data
tenure_county <- acs_B25003_county %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         year = year,
         label = case_when(
           variable == "B25003_002" ~ "Owner occupied",
           variable == "B25003_003" ~ "Renter occupied")) %>% 
  rename(total_households = summary_est,
         locality = NAME) %>% 
  select(GEOID, locality, label, estimate, moe, total_households, percent, year)

tenure_tract <- acs_B25003_tract %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         year = year,
         total_households = summary_est,
         label = case_when(
           variable == "B25003_002" ~ "Owner occupied",
           variable == "B25003_003" ~ "Renter occupied")) %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, label, estimate, moe, total_households, percent, year)

# Join tract names
tenure_tract <- tenure_tract %>% 
  left_join(tract_names)

# Tenure (Own & Rent): Charlottesville, county & tract ----
cville_tenure_county <- tenure_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_tenure_county, paste0("data/cville_tenure_county", "_", year, ".csv"))

cville_tenure_tract <- tenure_tract %>% 
  filter(locality == "Charlottesville city")

write_csv(cville_tenure_tract, paste0("data/cville_tenure_tract", "_", year, ".csv"))

# Tenure (Own & Rent): Albemarle, county & tract ----
alb_tenure_county <- tenure_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_tenure_county, paste0("data/alb_tenure_county", "_", year, ".csv"))

alb_tenure_tract <- tenure_tract %>% 
  filter(locality == "Albemarle County")

write_csv(alb_tenure_tract, paste0("data/alb_tenure_tract", "_", year, ".csv"))

# Tenure (Own & Rent): Combined region ----
region_tenure <- tenure_county %>% 
  group_by(label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_households = sum(total_households),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / total_households), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>% 
  select(region_fips, locality, estimate, moe, total_households, percent, label, year)

write_csv(region_tenure, paste0("data/region_tenure", "_", year, ".csv"))

## ..........................................
# Tenure (Own & Rent) by Race: B25003A-I ----
# Tables: B25003A-B25003I, 2012 & 2022

# Get ACS Data - County by race
# White Alone
acs_B25003A_county <- map_df(c(2022, 2012),
                                  ~ get_acs(geography = "county",
                                            year = .x,
                                            state = "VA",
                                            county = county_codes,
                                            table = "B25003A",
                                            summary_var = "B25003A_001",
                                            survey = "acs5") %>% 
                                    mutate(year = .x) %>% 
                                    filter(variable %in% c("B25003A_002", "B25003A_003")))

# Black Alone
acs_B25003B_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003B",
                                       summary_var = "B25003B_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003B_002", "B25003B_003")))

# American Indian/Native Alaskan
acs_B25003C_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003C",
                                       summary_var = "B25003C_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003C_002", "B25003C_003")))

# Asian
acs_B25003D_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003D",
                                       summary_var = "B25003D_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003D_002", "B25003D_003")))

# Native Hawaiian/Pacific Islander
acs_B25003E_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003E",
                                       summary_var = "B25003E_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003E_002", "B25003E_003")))

# Other Race
acs_B25003F_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003F",
                                       summary_var = "B25003F_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003F_002", "B25003F_003")))

# Multiracial
acs_B25003G_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003G",
                                       summary_var = "B25003G_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003G_002", "B25003G_003")))


# White, Not Hispanic or Latino
acs_B25003H_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003H",
                                       summary_var = "B25003H_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003H_002", "B25003H_003")))

# Hispanic or Latino
acs_B25003I_county <- map_df(c(2022, 2012),
                             ~ get_acs(geography = "county",
                                       year = .x,
                                       state = "VA",
                                       county = county_codes,
                                       table = "B25003I",
                                       summary_var = "B25003I_001",
                                       survey = "acs5") %>% 
                               mutate(year = .x) %>% 
                               filter(variable %in% c("B25003I_002", "B25003I_003")))

# Bind race tables
acs_B25003_race_county <- rbind(acs_B25003A_county,
                                acs_B25003B_county,
                                acs_B25003C_county,
                                acs_B25003D_county,
                                acs_B25003E_county,
                                acs_B25003F_county,
                                acs_B25003G_county,
                                acs_B25003H_county,
                                acs_B25003I_county)

tenure_race_county <- acs_B25003_race_county %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = case_when(
           str_detect(variable, "_002") ~ "Owner occupied",
           str_detect(variable, "_003") ~ "Renter occupied"),
         group = case_when(
           str_detect(variable, "A_") ~ "White",
           str_detect(variable, "B_") ~ "Black",
           str_detect(variable, "C_") ~ "American Indian/Native Alaskan",
           str_detect(variable, "D_") ~ "Asian",
           str_detect(variable, "E_") ~ "Native Hawaiian/Pacific Islander",
           str_detect(variable, "F_") ~ "Other",
           str_detect(variable, "G_") ~ "Mutiracial",
           str_detect(variable, "H_") ~ "White, Not Hispanic or Latino",
           str_detect(variable, "I_") ~ "Hispanic or Latino")
         ) %>% 
  rename(total_households = summary_est,
         locality = NAME) %>% 
  select(GEOID, locality, label, group, estimate, moe, total_households, percent, year)

# Tenure (Own & Rent) by Race: Charlottesville, county ----
cville_tenure_race_county <- tenure_race_county %>% 
  filter(locality == "Charlottesville city, Virginia")

write_csv(cville_tenure_race_county, paste0("data/cville_tenure_race_county_2012_2022.csv"))

# Tenure (Own & Rent) by Race: Albemarle, county ----
alb_tenure_race_county <- tenure_race_county %>% 
  filter(locality == "Albemarle County, Virginia")

write_csv(alb_tenure_race_county, paste0("data/alb_tenure_race_county_2012_2022.csv"))

# Tenure (Own & Rent) by Race: Combined region ----
region_tenure_race <- tenure_race_county %>% 
  group_by(group, label, year) %>% 
  summarize(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_households = sum(total_households),
            .groups = 'drop') %>% 
  mutate(percent = round(100 * (estimate / total_households), digits = 2),
         locality = region_name,
         region_fips = paste(county_codes, collapse = ";")) %>%
  select(region_fips, locality, group, label, estimate, moe, total_households, percent, year)

write_csv(region_tenure_race, "data/region_tenure_race_2012_2022.csv")

## ....................................................
## End