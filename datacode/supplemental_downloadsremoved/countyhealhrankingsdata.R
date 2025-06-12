# Regional Equity Profile
# County Health Ranking measures
# 2024-12-10

# Setup ----
library(tidyverse)
library(readxl)
library(janitor)
library(here)

# Source: https://www.countyhealthrankings.org/health-data/virginia/data-and-resources

# variables to save
# exercise access: select measure data (%)
# primary care #: select measure data (number/10oK)
# mental care #: select measure data (number/100K)
# membership org: select measure data (number/10K)
# population: additional measure data

# Download data ----
## create excel file links ----
file_links <- c(
  "https://www.countyhealthrankings.org/sites/default/files/media/document/2024%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xlsx",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/2023%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xlsx",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/2022%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xlsx",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/2021%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2019%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xls",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2018%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xls",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2017%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xls",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2016%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xls",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2015%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xls"
)

## create vector of destination file names ----
dest <- paste0("downloads/chr/", basename(file_links))

## download files ----
if (!dir.exists(here("downloads/chr"))) 
{dir.create(here("downloads/chr"))}

walk2(file_links, dest, download.file, method = "curl", extra = "-k")


# read in files ----
# test <- read_excel(dest[9], sheet = "Ranked Measure Data", skip = 1) %>%
#   clean_names()
# 2024 names table Select Measure Data; all others use Ranked Measure Data
# 2019-2015 doesn't have access to exercise
# 2016-2015 have two mental health provider columns:
#   the first column is corrected; the second column was initially issued (but incorrect)

# can't apply same function throughout; need
#   2024
#   2020-2023
#   2017-2019
#   2016
#   2015
# just going to read them in separately

## Select/Ranked measure data ----
chr24 <- read_excel(dest[1],  sheet = "Select Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2024)

chr23 <- read_excel(dest[2],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2023)

chr22 <- read_excel(dest[3],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2022)

chr21 <- read_excel(dest[4],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2021)

chr20 <- read_excel(dest[5],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2020)

chr19 <- read_excel(dest[6],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         # exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2019)

chr18 <- read_excel(dest[7],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         # exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2018)

chr17 <- read_excel(dest[8],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         # exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2017)

chr16 <- read_excel(dest[9],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         # exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers_76, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2016,
         mh_docs = as.numeric(mh_docs))

chr15 <- read_excel(dest[10],  sheet = "Ranked Measure Data", skip = 1) %>% 
  clean_names() %>% 
  select(fips,
         # exer_access = percent_with_access_to_exercise_opportunities,
         pc_docs = number_primary_care_physicians,
         mh_docs = number_mental_health_providers_80, 
         soc_assoc = number_associations) %>% 
  filter(fips %in% c("51000", "51003", "51540")) %>% 
  mutate(year = 2016,
         mh_docs = as.numeric(mh_docs))

## combine ---- 
chr <- bind_rows(chr24, chr23, chr22, chr21, chr20, chr19, chr18, chr17, chr16, chr15)

rm(chr24, chr23, chr22, chr21, chr20, chr19, chr18, chr17, chr16, chr15)

## Additional measure data ----
# test <- read_excel(dest[1], sheet = "Additional Measure Data", skip = 1) 
# mapdfr these, then select only fips and pop, clean names

add_meas <- map_dfr(dest, ~read_excel(.x, sheet = "Additional Measure Data", skip = 1) %>% 
                      select(any_of(c("FIPS", "Population", "Life Expectancy"))) %>% 
                      mutate(source = .x))

add_meas <- add_meas %>% 
  mutate(year = str_extract(source, "[0-9]{4}"),
         year = as.numeric(year)) %>% 
  select(-source) %>% 
  clean_names() %>% 
  filter(fips %in% c("51000", "51003", "51540"))

# Merge, derive measures ----
chr_final <- chr %>% 
  left_join(add_meas)

chr_final <- chr_final %>% 
  mutate(region = if_else(fips == "51000", "State", "Region"),
  number_exer_access = (exer_access/100)*population)

chr_locality <- chr_final %>% 
  select(fips, life_expectancy, year) %>% 
  mutate(place = case_when(
    fips == "51000" ~ "Virginia",
    fips == "51003" ~ "Albemarle",
    fips == "51540" ~ "Charlottesville"
  ))

# create region 
chr_region <- chr_final %>% 
  group_by(region, year) %>% 
  summarize(pc_docs = sum(pc_docs),
            mh_docs = sum(mh_docs),
            soc_assoc = sum(soc_assoc),
            population = sum(population),
            num_exer_access = sum(number_exer_access),
            year = first(year))

chr_region <- chr_region %>% 
  mutate(exer_access_per = (num_exer_access/population)*100,
         pc_docs_rate = pc_docs/(population/100000),
         mh_docs_rate = mh_docs/(population/100000),
         soc_assoc_rate = soc_assoc/(population/10000))

ggplot(chr_locality, aes(x = year, y = life_expectancy, color = place)) +
  geom_line()

# Save ----
write_csv(chr_region, "data/chr_regional_measures.csv")
write_csv(chr_locality, "data/chr_locality_measures.csv")

