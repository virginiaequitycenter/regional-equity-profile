# Creating AHDI Measures
# Author: Beth Mitchell
# Last Updated: 8/29/2024

# Load packages ----
library(tidyverse)
library(tidycensus)
library(janitor)
library(readxl)

# Gather data ----

# Life Expectancy by county and tract ----
# Albemarle
alb_life_exp_county <- read_csv("data/alb_life_exp_county_2024.csv")
alb_life_exp_county <- alb_life_exp_county %>% 
  filter(group == "all") %>% 
  select(GEOID, lifeexp_est)

alb_life_exp_tract <- read_csv("data/alb_life_exp_tract_2015.csv")
alb_life_exp_tract <- alb_life_exp_tract %>% 
  mutate(GEOID.y = as.character(GEOID.y)) %>% 
  rename(GEOID = GEOID.y) %>% 
  select(GEOID, county, tractnames, lifeexpE) %>% 
  unique()

# Charlottesville
cville_life_exp_county <- read_csv("data/cville_life_exp_county_2024.csv")
cville_life_exp_county <- cville_life_exp_county %>% 
  filter(group == "all") %>% 
  select(GEOID, lifeexp_est)

# cville_life_exp_tract <- read_csv("data/cville_life_exp_tract_2015.csv")
tjhd_lifeexp <- read_csv("data/cville_tjhd_life_exp_tract_2012.csv")
cville_life_exp_tract <- tjhd_lifeexp %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  rename(lifeexpE = life_exp) %>% 
  select(GEOID, county, tractnames, lifeexpE)

# Life Expectancy Region ----
# get populations
cville_pop <- read_csv("data/cville_sex_age_2022.csv") %>%
  group_by(GEOID) %>% 
  summarise(total_pop = first(total_pop))

alb_pop <- read_csv("data/alb_sex_age_2022.csv") %>%
  group_by(GEOID) %>% 
  summarise(total_pop = first(total_pop))

# merge and join
region_pop <- rbind(alb_pop, cville_pop)
region_life_exp <- rbind(alb_life_exp_county, cville_life_exp_county) %>% 
  left_join(region_pop)

# find weighted average
region_life_exp <- region_life_exp %>% 
  summarise(lifeexp_est = round(weighted.mean(lifeexp_est, total_pop, na.rm = T), 2),
            region_fips = paste(str_remove(GEOID, "51"), collapse = ";")) %>% 
  select(region_fips, lifeexp_est)

# School Enrollment (3-24yrs) by county and tract ----
# Albemarle
alb_enroll_county <- read_csv("data/alb_enroll_county_2022.csv")
alb_enroll_county <- alb_enroll_county %>% 
  rename(enrollment_per = percent) %>% 
  select(GEOID, enrollment_per)

alb_enroll_tract <- read_csv("data/alb_enroll_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))
alb_enroll_tract <- alb_enroll_tract %>% 
  select(GEOID, county, tractnames, percent) %>% 
  rename(enrollment_per = percent)

# Charlottesville
cville_enroll_county <- read_csv("data/cville_enroll_county_2022.csv")
cville_enroll_county <- cville_enroll_county %>% 
  rename(enrollment_per = percent) %>% 
  select(GEOID, enrollment_per)

cville_enroll_tract <- read_csv("data/cville_enroll_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))
cville_enroll_tract <- cville_enroll_tract %>% 
  select(GEOID, county, tractnames, percent) %>% 
  rename(enrollment_per = percent)

# Region 
region_enroll <- read_csv("data/region_enroll_2022.csv")%>% 
  rename(enrollment_per = percent) %>% 
  select(region_fips, enrollment_per)

# Degree Attainment 25yrs+ by county and tract ----
# Albemarle
alb_edu_attain_county <- read_csv("data/alb_edu_attain_county_2022.csv")
alb_edu_attain_county <- alb_edu_attain_county %>% 
  select(GEOID, percent, label) %>% 
  mutate(label = case_when(label == "High school graduate or higher" ~ "hs_grad_per",
                           label == "Bachelor's degree or higher" ~ "bac_deg_per",
                           label == "Graduate or professional degree" ~ "grad_deg_per")) %>% 
  pivot_wider(names_from = label, values_from = percent)

alb_edu_attain_tract <- read_csv("data/alb_edu_attain_tract_2022.csv") %>% 
  mutate(GEOID = as.character(GEOID))
alb_edu_attain_tract <- alb_edu_attain_tract %>% 
  select(GEOID, county, tractnames, percent, label) %>% 
  mutate(label = case_when(label == "High school graduate or higher" ~ "hs_grad_per",
                           label == "Bachelor's degree or higher" ~ "bac_deg_per",
                           label == "Graduate or professional degree" ~ "grad_deg_per")) %>% 
  pivot_wider(names_from = label, values_from = percent)

# Charlottesville
cville_edu_attain_county <- read_csv("data/cville_edu_attain_county_2022.csv")
cville_edu_attain_county <- cville_edu_attain_county %>% 
  select(GEOID, percent, label) %>% 
  mutate(label = case_when(label == "High school graduate or higher" ~ "hs_grad_per",
                           label == "Bachelor's degree or higher" ~ "bac_deg_per",
                           label == "Graduate or professional degree" ~ "grad_deg_per")) %>% 
  pivot_wider(names_from = label, values_from = percent)

cville_edu_attain_tract <- read_csv("data/cville_edu_attain_tract_2022.csv") %>% 
  mutate(GEOID = as.character(GEOID))
cville_edu_attain_tract <- cville_edu_attain_tract %>% 
  select(GEOID, county, tractnames, percent, label) %>% 
  mutate(label = case_when(label == "High school graduate or higher" ~ "hs_grad_per",
                           label == "Bachelor's degree or higher" ~ "bac_deg_per",
                           label == "Graduate or professional degree" ~ "grad_deg_per")) %>% 
  pivot_wider(names_from = label, values_from = percent)

# Region
region_edu_attain <- read_csv("data/region_edu_attain_2022.csv")%>% 
  select(region_fips, percent, label) %>% 
  mutate(label = case_when(label == "High school graduate or higher" ~ "hs_grad_per",
                           label == "Bachelor's degree or higher" ~ "bac_deg_per",
                           label == "Graduate or professional degree" ~ "grad_deg_per")) %>% 
  pivot_wider(names_from = label, values_from = percent)

# Median personal earnings by county and tract ----
# Albemarle
alb_med_earnings_county <- read_csv("data/alb_med_earnings_county_2022.csv") %>% 
  filter(group == "All") %>% 
  rename(med_earnings = estimate) %>% 
  select(GEOID, med_earnings)

alb_med_earnings_tract <- read_csv("data/alb_med_earnings_tract_2022.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  filter(group == "All") %>% 
  rename(med_earnings = estimate) %>% 
  select(GEOID, county, tractnames, med_earnings)

# Charlottesville
cville_med_earnings_county <- read_csv("data/cville_med_earnings_county_2022.csv") %>% 
  filter(group == "All") %>% 
  rename(med_earnings = estimate) %>% 
  select(GEOID, med_earnings)

cville_med_earnings_tract <- read_csv("data/cville_med_earnings_tract_2022.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  filter(group == "All") %>% 
  rename(med_earnings = estimate) %>% 
  select(GEOID, county, tractnames, med_earnings)

# Region
region_med_earnings <- read_csv("data/region_med_earnings_2022.csv") %>% 
  rename(med_earnings = med_earnings_est) %>% 
  mutate(region_fips = "003;540") %>% 
  select(region_fips, med_earnings)

# Join tables for county and tract ----
# County table
ahdi_alb <- alb_life_exp_county %>% 
  left_join(alb_edu_attain_county) %>% 
  left_join(alb_enroll_county) %>% 
  left_join(alb_med_earnings_county)

ahdi_cville <- cville_life_exp_county %>% 
  left_join(cville_edu_attain_county) %>% 
  left_join(cville_enroll_county) %>% 
  left_join(cville_med_earnings_county)

ahdi_county <- rbind(ahdi_alb, ahdi_cville) %>% 
  mutate(locality = case_when(GEOID == 51003 ~ "Albemarle",
                              GEOID == 51540 ~ "Charlottesville")) %>% 
  relocate(locality, .after=GEOID)

# Tract tables
ahdi_tract_alb <- alb_life_exp_tract %>% 
  left_join(alb_edu_attain_tract) %>% 
  left_join(alb_enroll_tract) %>% 
  left_join(alb_med_earnings_tract)

ahdi_tract_cville <- cville_life_exp_tract %>% 
  left_join(cville_edu_attain_tract) %>% 
  left_join(cville_enroll_tract) %>% 
  left_join(cville_med_earnings_tract)

# Region tables
ahdi_region <- region_life_exp %>% 
  left_join(region_edu_attain) %>% 
  left_join(region_enroll) %>% 
  left_join(region_med_earnings) %>% 
  rename(GEOID = region_fips) %>% 
  mutate(locality = "Charlottesville & Albemarle Combined") %>% 
  relocate(locality, .after=GEOID)

ahdi_region_counties <- rbind(ahdi_region, ahdi_county)

# Create AHDI values ----

# Goalposts for Calculating AHDI
# AHDI Method Notes, Measuring America: 10 Years and Counting, released 2018: 
# https://ssrc-static.s3.amazonaws.com/moa/10Year_Methods.pdf
#
# Life expectancy at birth (years): 90 years (Maximum Value) / 66 years (Minimum Value)
# Educational attainment score: 2 (Maximum Value) / 0.5 (Minimum Value)
# Combined net enrollment ratio (%): 95% (Maximum Value) / 60% (Minimum Value)

# Earnings goalposts adjusted for inflation (2022)
# Earnings goalposts were originally set at $55,000 and $13,000 in 2005 dollars converted to 2022 currency.
# Using CPI Inflation Calculator: https://www.bls.gov/data/inflation_calculator.htm
# Dates set: July 2005 and July 2022
# Max Value: $55,000 (2005) -> $83,394 (2022)
# Min Value: $13,000 (2005) -> $19,711 (2022)

ahdi_county <- ahdi_county %>% 
  mutate(health_index = ((lifeexp_est - 66) / (90 - 66)) * 10,
         edu_attain_index = (((hs_grad_per/100 + bac_deg_per/100 + grad_deg_per/100) - 0.5)/(2 - 0.5)) * 10,
         enroll_index = ((enrollment_per - 60)/(95 - 60)) * 10,
         education_index = ((2/3)*edu_attain_index) + ((1/3)*enroll_index),
         income_index = ((log10(med_earnings) - log10(19711)) / (log10(83394) - log10(19711))) * 10,
         ahdi = (health_index + education_index + income_index)/3) %>% 
  relocate(ahdi, .after=locality) %>% 
  relocate(hs_grad_per:bac_deg_per, .after=lifeexp_est)

ahdi_tract_alb <- ahdi_tract_alb %>% 
  mutate(health_index = ((lifeexpE - 66) / (90 - 66)) * 10,
         edu_attain_index = (((hs_grad_per/100 + bac_deg_per/100 + grad_deg_per/100) - 0.5)/(2 - 0.5)) * 10,
         enroll_index = ((enrollment_per - 60)/(95 - 60)) * 10,
         education_index = ((2/3)*edu_attain_index) + ((1/3)*enroll_index),
         income_index = ((log10(med_earnings) - log10(19711)) / (log10(83394) - log10(19711))) * 10,
         ahdi = (health_index + education_index + income_index)/3) %>% 
  relocate(ahdi, .after=tractnames) %>% 
  relocate(hs_grad_per:bac_deg_per, .after=lifeexpE)

ahdi_tract_cville <- ahdi_tract_cville %>% 
  mutate(health_index = ((lifeexpE - 66) / (90 - 66)) * 10,
         edu_attain_index = (((hs_grad_per/100 + bac_deg_per/100 + grad_deg_per/100) - 0.5)/(2 - 0.5)) * 10,
         enroll_index = ((enrollment_per - 60)/(95 - 60)) * 10,
         education_index = ((2/3)*edu_attain_index) + ((1/3)*enroll_index),
         income_index = ((log10(med_earnings) - log10(19711)) / (log10(83394) - log10(19711))) * 10,
         ahdi = (health_index + education_index + income_index)/3) %>% 
  relocate(ahdi, .after=tractnames) %>% 
  relocate(hs_grad_per:bac_deg_per, .after=lifeexpE)

ahdi_region_counties <- ahdi_region_counties %>% 
  mutate(health_index = ((lifeexp_est - 66) / (90 - 66)) * 10,
         edu_attain_index = (((hs_grad_per/100 + bac_deg_per/100 + grad_deg_per/100) - 0.5)/(2 - 0.5)) * 10,
         enroll_index = ((enrollment_per - 60)/(95 - 60)) * 10,
         education_index = ((2/3)*edu_attain_index) + ((1/3)*enroll_index),
         income_index = ((log10(med_earnings) - log10(19711)) / (log10(83394) - log10(19711))) * 10,
         ahdi = (health_index + education_index + income_index)/3) %>% 
  relocate(ahdi, .after=locality) %>% 
  relocate(hs_grad_per:bac_deg_per, .after=lifeexp_est)

# Write AHDI CSVs ----
write_csv(ahdi_county, "data/ahdi_county.csv")
write_csv(ahdi_tract_alb, "data/ahdi_tract_alb.csv")
write_csv(ahdi_tract_cville, "data/ahdi_tract_cville.csv")
write_csv(ahdi_region_counties, "data/ahdi_region_counties.csv")

# Benchmark Data - State & Localities ----
# 000 -- Virginia (state)
# Charlottesville Benchmarks:
# 510 -- Alexandria City
# 013 -- Arlington County
# 680 -- Lynchburg City
# 760 -- Richmond City
# 770 -- Roanoke City
# 830 -- Williamsburg City
# Albemarle Benchmarks:
# 015 -- Augusta County
# 029 -- Buckingham County
# 041 -- Chesterfield County
# 059 -- Fairfax County
# 061 -- Fauquier County
# 075 -- Goochland County
# 085 -- Hanover County
# 087 -- Henrico County
# 095 -- James City County
# 107 -- Loudoun County
# 121 -- Montgomery County
# 153 -- Prince William County
# 161 -- Roanoke County
# 165 -- Rockingham County
# 177 -- Spotsylvania County
# 550 -- City of Chesapeake
# 650 -- City of Hampton
# 660 -- City of Harrisonburg
# 680 -- Lynchburg City
# 770 -- Roanoke City
# 760 -- Richmond City
# 810 -- City of Virginia Beach


benchmark_fips <- c("000", "510", "013", "087", "680", "760", "770", "830", "015", "059", "165", "179",
                    "029", "041", "061", "075", "085", "095", "107", "121", "153", "161", "177",
                    "550", "650", "660", "810")

# Year for ACS data (single year)
year <- 2022

# Life Expectancy: Benchmark Cities/Counties & State ----
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
life_exp_benchmarks <- life_exp_sheet %>% 
  filter(fips %in% benchmark_fips) %>% 
  pivot_longer(lifeexp_all_est:lifeexp_asian_moe) %>% 
  mutate(name = str_remove(name, "lifeexp_"),
         year = 2024) %>% 
  separate(name, into = c("group", "name")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  relocate(year, .after = last_col())

names(life_exp_benchmarks) <- c("GEOID", "fips", "locality", "group", "lifeexp_est", "lower_bound", "upper_bound", "lifeexp_moe", "year")

benchmark_health <- life_exp_benchmarks %>% 
  filter(group=="all") %>% 
  mutate(name = case_when(GEOID == "51000" ~ "Virginia",
                          .default = locality),
         GEOID = case_when(GEOID == "51000" ~ "51",
                           .default = GEOID)) %>% 
  select(GEOID, name, lifeexp_est)

# Educational Attainment: Benchmark Cities/Counties & State ----
# Get ACS data
AHDI_vars_S1501 <- c("High school graduate or higher" = "S1501_C01_014",
                     "Bachelor's degree or higher" = "S1501_C01_015",
                     "Graduate or professional degree" = "S1501_C01_013")

acs_S1501_county <- get_acs(
  geography = "county",
  state = "VA",
  county = benchmark_fips,
  var = AHDI_vars_S1501,
  summary_var = "S1501_C01_006", 
  year = year, 
  survey = "acs5")

acs_S1501_state <- get_acs(
  geography = "state",
  state = "VA",
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

edu_attain_state <- acs_S1501_state %>% 
  mutate(percent = round(100 * (estimate / summary_est), digits = 2),
         label = variable,
         year = year) %>% 
  rename(c("pop_25_over" = "summary_est",
           "locality" = "NAME")) %>% 
  select(GEOID, locality, estimate, moe, pop_25_over, percent, label, year)

edu_attain_benchmarks <- rbind(edu_attain_state, edu_attain_county)

edu_attain_benchmarks <- edu_attain_benchmarks %>% 
  select(GEOID, percent, label) %>% 
  mutate(label = case_when(label == "High school graduate or higher" ~ "hs_grad_per",
                           label == "Bachelor's degree or higher" ~ "bac_deg_per",
                           label == "Graduate or professional degree" ~ "grad_deg_per")) %>% 
  pivot_wider(names_from = label, values_from = percent)

# School Enrollment: Benchmark Cities/Counties & State ----
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
  county = benchmark_fips,
  var = AHDI_vars_S1401,
  year = year, 
  survey = "acs5")

acs_S1401_state <- get_acs(
  geography = "state",
  state = "VA",
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

enroll_state <- acs_S1401_state %>% 
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

edu_enroll_benchmarks <- rbind(enroll_state, enroll_county)

edu_enroll_benchmarks <- edu_enroll_benchmarks %>% 
  rename(enrollment_per = percent) %>% 
  select(GEOID, enrollment_per)

# Median Personal Earnings: Benchmark Cities/Counties & State ----
# Get ACS data
AHDI_vars_B20002 <- c("Median Earnings; All" = "B20002_001",
                      "Median Earnings; Male" = "B20002_002",
                      "Median Earnings; Female" = "B20002_003")

acs_B20002_county <- get_acs(
  geography = "county",
  state = "VA",
  county = benchmark_fips,
  var = AHDI_vars_B20002,
  year = year, 
  survey = "acs5")

acs_B20002_state <- get_acs(
  geography = "state",
  state = "VA",
  var = AHDI_vars_B20002,
  year = year, 
  survey = "acs5")

# Wrangle tables:
med_earnings_county <- acs_B20002_county %>% 
  separate(variable, into=c("label","group"), sep="; ", remove=FALSE) %>% 
  mutate(year = year) %>% 
  rename("locality" = "NAME") %>% 
  select(GEOID, locality, label, group, estimate, moe, year)

med_earnings_state <- acs_B20002_state %>% 
  separate(variable, into=c("label","group"), sep="; ", remove=FALSE) %>% 
  mutate(year = year) %>% 
  rename("locality" = "NAME") %>% 
  select(GEOID, locality, label, group, estimate, moe, year)

benchmark_earnings <- rbind(med_earnings_state, med_earnings_county)

benchmark_earnings <- benchmark_earnings %>% 
  filter(group == "All") %>% 
  rename(med_earnings = estimate) %>% 
  select(GEOID, med_earnings)

# Join tables ----
# Join ACS derived tables
ahdi_benchmarks <- benchmark_health %>% 
  left_join(edu_attain_benchmarks) %>% 
  left_join(edu_enroll_benchmarks) %>% 
  left_join(benchmark_earnings)

# Create AHDI values ----
ahdi_benchmarks <- ahdi_benchmarks %>% 
  mutate(health_index = ((lifeexp_est - 66) / (90 - 66)) * 10,
         edu_attain_index = (((hs_grad_per/100 + bac_deg_per/100 + grad_deg_per/100) - 0.5)/(2 - 0.5)) * 10,
         enroll_index = ((enrollment_per - 60)/(95 - 60)) * 10,
         education_index = ((2/3)*edu_attain_index) + ((1/3)*enroll_index),
         income_index = ((log10(med_earnings) - log10(19711)) / (log10(83394) - log10(19711))) * 10,
         ahdi = (health_index + education_index + income_index)/3) %>% 
  relocate(ahdi, .after=name) %>% 
  relocate(hs_grad_per:bac_deg_per, .after=lifeexp_est)

# Add Albemarle and Cville and save CSV ----
ahdi_county <- ahdi_county %>% 
  rename(name = locality)

ahdi_all <- rbind(ahdi_county, ahdi_benchmarks)

write_csv(ahdi_all, "data/ahdi_benchmarks.csv")

