# HUD PIT and HIC and McKinney-Vento data
# Updated 2024-01-08

library(tidyverse)
library(readxl)
library(readxlsb)
library(janitor)

# HUD PIT data ----
# https://www.huduser.gov/portal/datasets/ahar/2024-ahar-part-1-pit-estimates-of-homelessness-in-the-us.html
url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-PIT-Counts-by-CoC.xlsb"
download.file(url, destfile = "downloads/2007-2024-PIT-Counts-by_CoC.xlsb")

yearlist <- as.character(2024:2010) 
pit <- map_dfr(yearlist, 
                ~ read_xlsb("downloads/2007-2024-PIT-Counts-by_CoC.xlsb", sheet = .x) %>% 
                  clean_names() %>% 
                  filter(str_detect(co_c_number, "VA"),
                         str_detect(co_c_name, "Charlottesville")) %>% 
                  select(co_c_number, co_c_name, count_types, overall_homeless, sheltered_es_homeless, sheltered_th_homeless, 
                         sheltered_sh_homeless, sheltered_total_homeless, unsheltered_homeless) %>% 
                  mutate(year = .x))

# data structure is different in 2007-2009: no count_types, sheltered_sh_homeless
yearlistb <- as.character(2009:2007)
pitb <-  map_dfr(yearlistb, 
                 ~ read_xlsb("downloads/2007-2023-PIT-Counts-by_CoC.xlsb", sheet = .x) %>% 
                   clean_names() %>% 
                   filter(str_detect(co_c_number, "VA"),
                          str_detect(co_c_name, "Charlottesville")) %>% 
                   select(co_c_number, co_c_name, overall_homeless, sheltered_es_homeless, sheltered_th_homeless, 
                          sheltered_total_homeless, unsheltered_homeless) %>% 
                   mutate(year = .x))
  
pit <- bind_rows(pit, pitb) %>% 
  mutate(year = as.numeric(year))

# check
ggplot(pit, aes(x  = year, y = overall_homeless)) +
  geom_line() +
  geom_point()

# save data
write_csv(pit, "data/pitcount_2007_2024.csv")
# pit <- read_csv("data/pitcount_2007_2024.csv")
# additional counts of potential interest:
# chronically homeless, unhoused families, unhoused youth
# counts by age groups, sex/gender, race/ethnicity

# identifying data structure
# pit23 <- read_xlsb("2007-2023-PIT-Counts-by_CoC.xlsb", sheet = "2023") %>%
#   clean_names() %>%
#   filter(str_detect(co_c_number, "VA"),
#          str_detect(co_c_name, "Charlottesville")) %>%
#   select(co_c_number, co_c_name, count_types, overall_homeless, sheltered_es_homeless, sheltered_th_homeless,
#          sheltered_sh_homeless, sheltered_total_homeless, unsheltered_homeless)
#   mutate(year = 2023)
# 
# identifying changes in data structure
# pit09 <- read_xlsb("2007-2023-PIT-Counts-by_CoC.xlsb", sheet = "2009") %>% 
#     clean_names() %>% 
#     filter(str_detect(co_c_number, "VA"),
#            str_detect(co_c_name, "Charlottesville")) %>% 
#     select(co_c_number, co_c_name, overall_homeless, sheltered_es_homeless, sheltered_th_homeless, 
#            sheltered_total_homeless, unsheltered_homeless)
#   mutate(year = 2009)


# HUD HIC data ---- 
# https://www.huduser.gov/portal/datasets/ahar/2024-ahar-part-1-pit-estimates-of-homelessness-in-the-us.html
url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-HIC-Counts-by-CoC.xlsx"
download.file(url, destfile = "downloads/2007-2024-HIC-Counts-by_CoC.xlsx")

yearlist <- as.character(2024:2014)
hic <- map_dfr(yearlist, 
               ~ read_excel("downloads/2007-2024-HIC-Counts-by_CoC.xlsx", sheet = .x, skip = 1) %>% 
                 clean_names() %>% 
                 filter(co_c_number == "VA-504") %>% 
                 select(co_c_number, total_yearround_beds = total_year_round_beds_es_th_sh,
                        total_seasonal_beds = total_seasonal_beds_es,
                        total_rrh_beds = total_year_round_beds_rrh, total_psh_beds = total_year_round_beds_psh, total_oph_beds = total_year_round_beds_oph) %>% 
                 mutate(year = .x))

# 2013 has different variable name for total_year_round_beds_es_th_sh, total_seasonal_beds_es
hic13 <- read_excel("downloads/2007-2024-HIC-Counts-by_CoC.xlsx", sheet = "2013", skip = 1) %>% 
  clean_names() %>% 
  filter(co_c_number == "VA-504") %>% 
  select(co_c_number, total_yearround_beds = total_year_round_beds_es_th_rrh_sh,
         total_seasonal_beds = total_seasonal_es_beds,
         total_rrh_beds, total_psh_beds) %>% 
  mutate(year = "2013")

# 2008-2012 has different name for co_c_number, total_seasonal_beds_es
yearlistb <- as.character(2012:2008)
hicb <- map_dfr(yearlistb, 
               ~ read_excel("downloads/2007-2024-HIC-Counts-by_CoC.xlsx", sheet = .x, skip = 1) %>% 
                 clean_names() %>% 
                 filter(co_c == "VA-504") %>% 
                 select(co_c_number = co_c, total_yearround_beds = total_year_round_beds_es_th_sh,
                        total_seasonal_beds = total_seasonal_es_beds,
                        total_psh_beds) %>% 
                 mutate(total_seasonal_beds = as.numeric(total_seasonal_beds),
                        year = .x))

# 2007 has different name for total_year_round_beds_es_th_sh, total_seasonal_beds_es
hic07 <- read_excel("downloads/2007-2024-HIC-Counts-by_CoC.xlsx", sheet = "2007", skip = 1) %>% 
  clean_names() %>% 
  filter(co_c == "VA-504") %>% 
  select(co_c_number = co_c, total_yearround_beds = total_year_round_beds_es_th,
         total_seasonal_beds = total_seasonal_es_beds,
         total_psh_beds) %>% 
  mutate(year = "2007")

hic <- bind_rows(hic, hic13, hicb, hic07) %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_longer(-c(co_c_number, year), names_to = "bedtype", values_to = "beds")

# check
hic %>% 
  filter(bedtype %in% c("total_yearround_beds", "total_seasonal_beds")) %>% 
  ggplot(aes(x = year, y = beds, color = bedtype)) +
  geom_line() +
  geom_point()

hic %>% 
  filter(! bedtype %in% c("total_yearround_beds", "total_seasonal_beds")) %>% 
  ggplot(aes(x = year, y = beds, color = bedtype)) +
  geom_line() +
  geom_point()

# save
write_csv(hic, "data/hiccount_2007_2024.csv")
# hic <- read_csv("data/hiccount_2007_2024.csv")
# additional counts of potential interest (not all year):
# 2023-2014: total year round beds rrh and psh and oph
# 2023-2013 (different names): total year round beds rrh and psh 
# 2023-2007: total year round beds psh

# identifying data structure
# hic23 <- read_excel("2007-2023-HIC-Counts-by-CoC.xlsx", sheet = "2023", skip = 1) %>%
#   clean_names() %>%
#   filter(co_c_number == "VA-504") %>%
#   select(co_c_number, total_yearround_beds = total_year_round_beds_es_th_sh,
#          total_seasonal_beds = total_seasonal_beds_es) %>%
#   mutate(year = 2023)

# Background resource for RRH/PSH/OPH: https://housingforwardva.org/news/fwd-b02-permanent-supportive-housing-1/

# 2023 HIC details ----
url <- "https://www.huduser.gov/portal/sites/default/files/xls/2023-HIC-Counts-by-State.csv"
hic23 <- read_csv(url)
hic23_va <- hic23 %>% filter(CocState == "VA",
                             str_detect(CoC, "Charlottesville"))

write_csv(hic23_va, "data/hic23_detail.csv")
# hic23 <- read_csv("data/hic23_detail.csv")
# not currently used, but potentially useful

hic23 %>% 
  filter(`Project Type` %in% c("OPH", "RRH", "PSH")) %>% 
  select(`Organization Name`, `Project Type`, `Project Name`,
         `Bed Type`, `Inventory Type`, `Target Population`,
         `Year-Round Beds`) %>% 
  view()

# MV data ----

# https://eddataexpress.ed.gov/download/data-library?field_year_target_id=All&field_population_value=&field_data_topic_target_id=All&field_reporting_level_target_id=All&field_program_target_id=42&field_file_spec_target_id=All&field_data_group_id_target_id=All&combine=
url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_11718/SY2122_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "downloads/mv2122.zip")
unzip("dowloads/mv2122.zip", exdir = "downloads/mv2122/")
mv22 <- read_csv("downloads/mv2122/SY2122_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA",
         str_detect(LEA, "ALBEMARLE|CHARLOTTESVILLE"))

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_8321/SY2021_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "downloads/mv2021.zip")
unzip("downloads/mv2021.zip", exdir = "downloads/mv2021/")
mv21 <- read_csv("downloads/mv2021/SY2021_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA",
         str_detect(LEA, "ALBEMARLE|CHARLOTTESVILLE"))

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_6526/SY1920_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "downloads/mv1920.zip")
unzip("downloads/mv1920.zip", exdir = "downloads/mv1920/")
mv20 <- read_csv("downloads/mv1920/SY1920_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA",
         str_detect(LEA, "ALBEMARLE|CHARLOTTESVILLE"))

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_2111/SY1819_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "downloads/mv1819.zip")
unzip("downloads/mv1819.zip", exdir = "downloads/mv1819/")
mv19 <- read_csv("downloads/mv1819/SY1819_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA",
         str_detect(LEA, "ALBEMARLE|CHARLOTTESVILLE")) %>% 
  mutate(`NCES LEA ID` = as.character(`NCES LEA ID`))

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "downloads/mv1018.zip")
unzip("downloads/mv1018.zip", exdir = "downloads/mv1018/")
mv18 <- read_csv("downloads/mv1018/SY1018_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA",
         str_detect(LEA, "ALBEMARLE|CHARLOTTESVILLE"))
# The above file contains 2010-2011 through 2017-2018, but individual files also exist
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2016-17
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2015-16
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2014-15
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2013-14
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2012-13
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2011-12
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2010-11

# generate yearly totals
mv_cvl_alb <- bind_rows(
  mv22 %>% filter(Subgroup == "All Students in LEA") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv21 %>% filter(Subgroup == "All Students in LEA") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv20 %>% filter(Subgroup == "All Students in LEA") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv19 %>% filter(Subgroup == "All Students") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv18 %>% filter(Subgroup == "All Students") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup)
)

# save
write_csv(mv_cvl_alb, "data/mvcounts_2011_2022.csv")

# Add 2023 ----
mv <- read_csv("data/mvcounts_2011_2022.csv") %>% 
  mutate(`NCES LEA ID` = as.character(`NCES LEA ID`))

# 2022-2023 only available as of 2025-01-08 via data download tool
# https://eddataexpress.ed.gov/download/data-builder/data-download-tool?f%5B1%5D=all_students%3AAll%20Students%20in%20SEA&f%5B2%5D=data_group_id%3A655&f%5B3%5D=file_spec%3A118&f%5B4%5D=school_year%3A2022-2023&f%5B5%5D=state_name%3AVIRGINIA&f%5B0%5D=all_students%3AAll%20Students%20in%20LEA
mv23 <- read_csv("downloads/mv2223.csv") %>% 
  filter(str_detect(LEA, "Albemarle|Charlottesville"))
glimpse(mv23)

mv23 <- mv23 %>% 
  select(`School Year`, `NCES LEA ID`, LEA, `Data Description`, Value, Population, Subgroup) %>% 
  mutate(LEA = str_to_upper(LEA),
         `NCES LEA ID` = as.character(`NCES LEA ID`),
         Value = as.numeric(Value))

mv <- mv %>% 
  mutate(LEA = str_replace(LEA, "CO PBLC SCHS", "COUNTY PUBLIC SCHOOLS"),
         LEA = str_replace(LEA, "CTY PBLC SCHS", "CITY PUBLIC SCHOOLS"))

mv_with_23 <- bind_rows(mv23, mv)

# check
ggplot(mv_with_23, aes(x = `School Year`, y = Value)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  facet_wrap(~LEA, ncol = 1, scales = "free_y")

# save
write_csv(mv_with_23, "data/mvcounts_2011_2023.csv")
# mv <- read_csv("data/mvcounts_2011_2023.csv")

mv_with_23 %>% 
  group_by(`School Year`) %>% 
  summarize(Value = sum(Value)) %>% 
  ggplot(aes(x = `School Year`, y = Value, group = 1)) +
  geom_point() +
  geom_line()

# additional potential counts of interest (most years):
# by race/ethnicity, by type of unhoused (doubling up, hotel, shetlers/transitional, unsheltered)
# in later years, there are also separate pre-school files (not used here)