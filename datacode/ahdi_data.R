# Creating AHDI Measures
# Author: Beth Mitchell
# Last Updated: 8/29/2024

# Load packages ----
library(tidyverse)
library(tidycensus)
library(janitor)

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

cville_life_exp_tract <- read_csv("data/cville_life_exp_tract_2015.csv")
cville_life_exp_tract <- cville_life_exp_tract %>% 
  mutate(GEOID.y = as.character(GEOID.y)) %>% 
  rename(GEOID = GEOID.y) %>% 
  select(GEOID, county, tractnames, lifeexpE)

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

# Write AHDI CSVs ----
write_csv(ahdi_county, "data/ahdi_county.csv")
write_csv(ahdi_tract_alb, "data/ahdi_tract_alb.csv")
write_csv(ahdi_tract_cville, "data/ahdi_tract_cville.csv")




