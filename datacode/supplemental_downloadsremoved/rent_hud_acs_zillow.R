# Rental cost data
# HUD's FMR; ACS GMR; Zillow

library(tidyverse)
library(tidycensus)

# Pull HUD FMR file ----
# 2-bedroom file
# https://www.huduser.gov/portal/datasets/fmr.html#history
url <- "https://www.huduser.gov/portal/datasets/FMR/FMR_2Bed_1983_2025.csv"
fmr <- read_csv(url)
fmr_va <- fmr %>% filter(state == 51)
fmr_cvl <- fmr_va %>% filter(str_detect(areaname25, "Charlottesville")) %>% 
  slice(1) %>% select(fips, areaname25, contains("_2")) %>% 
  pivot_longer(cols = contains("_2"),
               names_to = "year", names_prefix = "fmr",
               values_to = "fmr40") %>% 
  mutate(year = str_remove(year, "_2"),
         year = ifelse(year %in% c("83", "84", "85", "86", "87", "88", "89", "90",
                                   "91", "92", "93", "94", "95", "96", "97", "98","99"), 
                       paste0("19", year),
                       paste0("20", year)),
         year = as.integer(year)) %>% 
  filter(year >= 2012)

ggplot(fmr_cvl, aes(x = year, y = fmr40)) + 
  geom_line()

# Pull ACS GMR ----
# B25031, 2 bedroom
year_a <- 2015:2019 
year_b <- 2021:2023
years <- c(year_a, year_b)

gmr <- map_dfr(
  years,
  ~ get_acs(
    geography = "cbsa",
    variables = "B25031_004",
    year = .x,
    survey = "acs1"
  ) %>% 
  mutate(year = .x)) %>% 
  filter(str_detect(NAME, "VA"),
         str_detect(NAME, "Charlottesville"))

ggplot(gmr, aes(x = year, y = estimate)) + 
  geom_line()

# Pull zillow msa estimate ----
# https://www.zillow.com/research/data/

url <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_month.csv?t=1732759750"

zest <- read_csv(url)
zest_cvl <- zest %>% 
  filter(str_detect(RegionName, "Charlottesville")) %>% 
  select(-c(RegionID, SizeRank, RegionType)) %>% 
  pivot_longer(-c(RegionName, StateName), names_to = "date", values_to = "est") %>% 
  mutate(date = ymd(date))

ggplot(zest_cvl, aes(x = date, y = est)) +
  geom_line()

# Compare ----
fmr_cvl_combined <- fmr_cvl %>% 
  select(name = areaname25, year = year, rent = fmr40) %>% 
  mutate(source = "hud_fmr")

gmr_combined <- gmr %>% 
  select(name = NAME, year = year, rent = estimate) %>% 
  mutate(source = "acs_gmr")
  
zest_cvl_combined <- zest_cvl %>% 
  mutate(year = year(date)) %>% 
  group_by(RegionName, year) %>% 
  summarize(rent = median(est)) %>% 
  select(name = RegionName, year = year, rent = rent) %>% 
  mutate(source = "zil_zor")
  
fmr_gmr_zest <- bind_rows(fmr_cvl_combined, gmr_combined, zest_cvl_combined)

ggplot(fmr_gmr_zest, aes(x = year, y = rent, color = source)) +
  geom_line()

# Save ----
write_csv(fmr_gmr_zest, "data/rents_zillow_hud_acs.csv")
# rents <- read_csv("data/rents_zillow_hud_acs.csv")
