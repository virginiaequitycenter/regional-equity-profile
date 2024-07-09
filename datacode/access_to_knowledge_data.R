# R Script for pulling and examining access to knowledge data
# Author: Henry DeMarco
# Date Created: June 17, 2024
# Last Updated: July 9, 2024
# Progress Tracking Spreadsheet Updated: July 9, 2024 (!)

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

#(!) indicates temporary comments to remove

#Packages
library(tidyverse)
library(tidycensus)
library(readxl)

# Census API Key
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

# Creating basic objects

# Year for all data pull
year <- 2022

# County FIPS codes and name
county_code <- c("003") #Albemarle FIPS Code

# A custom R function that creates a table of all variable codes and metadata
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
View(meta_table)

############################

# Table: B15003 (Educational Attainment for the Population 25 Years and Over)

# Charlottesville (Educational Attainment: 25 and Over), 2023

educ_attain_cville_2022 <- get_acs(
  geography = "tract",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B15003",
  summary_var = "B15003_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable != "B15003_001")

# Finalize the table by creating new labels and calculating percentages
cville_educ_attain_2022 <- educ_attain_cville_2022 %>%
  mutate(
    label = case_when(
      variable %in% c("B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016") ~ "Less than high school diploma",
      variable %in% c("B15003_017", "B15003_018") ~ "High school graduate",
      variable %in% c("B15003_019", "B15003_020", "B15003_021") ~ "Some college or associate's degree",
      variable %in% c("B15003_022", "B15003_023", "B15003_024", "B15003_025") ~ "Bachelor's degree or higher"
    )
  ) %>%
  group_by(GEOID, NAME, summary_est, label) %>%
  summarise(
    estimate = sum(estimate),
    .groups = "drop"
  ) %>%
  mutate(
    percent = 100 * (estimate / summary_est),
    year = "2022"
  )

# Generating CSV:

write.csv(cville_educ_attain_2022, "temp_data/cville_educ_attain_2022.csv")

# Albemarle (Educational Attainment: 25 and Over), 2023

educ_attain_alb_2022 <- get_acs(
  geography = "tract",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B15003",
  summary_var = "B15003_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable != "B15003_001")

# Finalize the table by creating new labels and calculating percentages
alb_educ_attain_2022 <- educ_attain_alb_2022 %>%
  mutate(
    label = case_when(
      variable %in% c("B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016") ~ "Less than high school diploma",
      variable %in% c("B15003_017", "B15003_018") ~ "High school graduate",
      variable %in% c("B15003_019", "B15003_020", "B15003_021") ~ "Some college or associate's degree",
      variable %in% c("B15003_022", "B15003_023", "B15003_024", "B15003_025") ~ "Bachelor's degree or higher"
    )
  ) %>%
  group_by(GEOID, NAME, summary_est, label) %>%
  summarise(
    estimate = sum(estimate),
    .groups = "drop"
  ) %>%
  mutate(
    percent = 100 * (estimate / summary_est),
    year = "2022"
  )

# Generating CSV:

write.csv(alb_educ_attain_2022, "temp_data/alb_educ_attain_2022.csv")


# Table: B14001 (School Enrollment by Level of School for the Population 3 Years and Over)

# Charlottesville (School Enrollment), 2022

school_enrollment_cville <- get_acs(
  geography = "tract",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B14001",
  summary_var = "B14001_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable != "B14001_001")

cville_school_enrollment_2022 <- school_enrollment_cville %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B14001_002" ~ "Enrolled in school",
           variable == "B14001_003" ~ "Enrolled in school: Nursery school, preschool",
           variable == "B14001_004" ~ "Enrolled in school: Kindergarten",
           variable == "B14001_005" ~ "Enrolled in school: Grade 1 to 4",
           variable == "B14001_006" ~ "Enrolled in school: Grade 5 to 8",
           variable == "B14001_007" ~ "Enrolled in school: Grade 9 to 12",
           variable == "B14001_008" ~ "Enrolled in school: College, undergraduate years",
           variable == "B14001_009" ~ "Enrolled in school: Graduate or professional school",
           variable == "B14001_010" ~ "Not enrolled in school",
         ))

# Generating CSV:

write.csv(cville_school_enrollment_2022, "temp_data/cville_school_enrollment_2022.csv")

# Albemarle (School Enrollment), 2022

school_enrollment_alb <- get_acs(
  geography = "tract",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B14001",
  summary_var = "B14001_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable != "B14001_001")

alb_school_enrollment_2022 <- school_enrollment_alb %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B14001_002" ~ "Enrolled in school",
           variable == "B14001_003" ~ "Enrolled in school: Nursery school, preschool",
           variable == "B14001_004" ~ "Enrolled in school: Kindergarten",
           variable == "B14001_005" ~ "Enrolled in school: Grade 1 to 4",
           variable == "B14001_006" ~ "Enrolled in school: Grade 5 to 8",
           variable == "B14001_007" ~ "Enrolled in school: Grade 9 to 12",
           variable == "B14001_008" ~ "Enrolled in school: College, undergraduate years",
           variable == "B14001_009" ~ "Enrolled in school: Graduate or professional school",
           variable == "B14001_010" ~ "Not enrolled in school",
         ))

# Generating CSV:

write.csv(alb_school_enrollment_2022, "temp_data/alb_school_enrollment_2022.csv")

# Looking for percent enrolled in school, ages 3-24 (might need to use S1401)

# Charlottesville (School Enrollment, Ages 3 to 24), 2023

school_enrollment_3to24_cville <- get_acs(
  geography = "tract",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "S1401",
  survey = "acs5",
  year = 2022)

# Define variables for total population and enrolled population (ages 3 to 24)
total_population_vars <- c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")
enrolled_population_vars <- c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")

# Calculate the total and enrolled population for ages 3 to 24
cville_school_enrollment_3to24_2022 <- school_enrollment_3to24_cville %>%
  filter(variable %in% c(total_population_vars, enrolled_population_vars)) %>%
  mutate(
    population_type = case_when(
      variable %in% total_population_vars ~ "total",
      variable %in% enrolled_population_vars ~ "enrolled"
    )
  ) %>%
  group_by(GEOID, NAME, population_type) %>%
  summarise(estimate = sum(estimate), .groups = "drop") %>%
  spread(population_type, estimate) %>%
  mutate(
    percent_enrolled = 100 * (enrolled / total)
  ) %>%
  select(GEOID, NAME, total, enrolled, percent_enrolled)

# Generating CSV:

write.csv(cville_school_enrollment_3to24_2022, "temp_data/cville_school_enrollment_3to24_2022.csv")

# Albemarle (School Enrollment, Ages 3 to 24), 2023

school_enrollment_3to24_alb <- get_acs(
  geography = "tract",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "S1401",
  survey = "acs5",
  year = 2022)

# Define variables for total population and enrolled population (ages 3 to 24)
total_population_vars <- c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")
enrolled_population_vars <- c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")

# Calculate the total and enrolled population for ages 3 to 24
alb_school_enrollment_3to24_2022 <- school_enrollment_3to24_alb %>%
  filter(variable %in% c(total_population_vars, enrolled_population_vars)) %>%
  mutate(
    population_type = case_when(
      variable %in% total_population_vars ~ "total",
      variable %in% enrolled_population_vars ~ "enrolled"
    )
  ) %>%
  group_by(GEOID, NAME, population_type) %>%
  summarise(estimate = sum(estimate), .groups = "drop") %>%
  spread(population_type, estimate) %>%
  mutate(
    percent_enrolled = 100 * (enrolled / total)
  ) %>%
  select(GEOID, NAME, total, enrolled, percent_enrolled)

# Generating CSV:

write.csv(alb_school_enrollment_3to24_2022, "temp_data/alb_school_enrollment_3to24_2022.csv")

# Table: S1501 (Educational Attainment by Race / Ethnicity)

# Charlottesville (Educational Attainment by Race / Ethnicity), 2022

# Pull the data (county level)
educ_attain_by_race_cville_county_2022 <- get_acs(
  geography = "county",
  county = "540",
  state = "VA",
  table = "S1501_C01",
  survey = "acs5",
  year = 2022,
  cache = TRUE
)

# Reshape the data
educ_attain_cville_county_wide <- educ_attain_by_race_cville_county_2022 %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Attainment categories with counts
cville_educ_attain_by_race_county_processed <- educ_attain_cville_county_wide %>%
  mutate(
    White_total = `S1501_C01_028`,
    White_no_high_school = `S1501_C01_028` - `S1501_C01_029`,
    White_high_school_to_no_bachelors = `S1501_C01_029` - `S1501_C01_030`,
    White_bachelors_or_more = `S1501_C01_030`,
    White_non_hisp_total = `S1501_C01_031`,
    White_non_hisp_no_high_school = `S1501_C01_031` - `S1501_C01_032`,
    White_non_hisp_high_school_to_no_bachelors = `S1501_C01_032` - `S1501_C01_033`,
    White_non_hisp_bachelors_or_more = `S1501_C01_033`,
    Black_total = `S1501_C01_034`,
    Black_no_high_school = `S1501_C01_034` - `S1501_C01_035`,
    Black_high_school_to_no_bachelors = `S1501_C01_035` - `S1501_C01_036`,
    Black_bachelors_or_more = `S1501_C01_036`,
    Amer_Indian_total = `S1501_C01_037`,
    Amer_Indian_no_high_school = `S1501_C01_037` - `S1501_C01_038`,
    Amer_Indian_high_school_to_no_bachelors = `S1501_C01_038` - `S1501_C01_039`,
    Amer_Indian_bachelors_or_more = `S1501_C01_039`,
    Asian_total = `S1501_C01_040`,
    Asian_no_high_school = `S1501_C01_040` - `S1501_C01_041`,
    Asian_high_school_to_no_bachelors = `S1501_C01_041` - `S1501_C01_042`,
    Asian_bachelors_or_more = `S1501_C01_042`,
    NHPI_total = `S1501_C01_043`,
    NHPI_no_high_school = `S1501_C01_043` - `S1501_C01_044`,
    NHPI_high_school_to_no_bachelors = `S1501_C01_044` - `S1501_C01_045`,
    NHPI_bachelors_or_more = `S1501_C01_045`,
    Other_total = `S1501_C01_046`,
    Other_no_high_school = `S1501_C01_046` - `S1501_C01_047`,
    Other_high_school_to_no_bachelors = `S1501_C01_047` - `S1501_C01_048`,
    Other_bachelors_or_more = `S1501_C01_048`,
    Two_or_more_total = `S1501_C01_049`,
    Two_or_more_no_high_school = `S1501_C01_049` - `S1501_C01_050`,
    Two_or_more_high_school_to_no_bachelors = `S1501_C01_050` - `S1501_C01_051`,
    Two_or_more_bachelors_or_more = `S1501_C01_051`,
    Hispanic_total = `S1501_C01_052`,
    Hispanic_no_high_school = `S1501_C01_052` - `S1501_C01_053`,
    Hispanic_high_school_to_no_bachelors = `S1501_C01_053` - `S1501_C01_054`,
    Hispanic_bachelors_or_more = `S1501_C01_054`,
  ) %>% 
  select(GEOID, NAME, 
         White_total, White_no_high_school, White_high_school_to_no_bachelors, White_bachelors_or_more,
         White_non_hisp_total, White_non_hisp_no_high_school, White_non_hisp_high_school_to_no_bachelors, White_non_hisp_bachelors_or_more,
         Black_total, Black_no_high_school, Black_high_school_to_no_bachelors, Black_bachelors_or_more,
         Amer_Indian_total, Amer_Indian_no_high_school, Amer_Indian_high_school_to_no_bachelors, Amer_Indian_bachelors_or_more,
         Asian_total, Asian_no_high_school, Asian_high_school_to_no_bachelors, Asian_bachelors_or_more,
         NHPI_total, NHPI_no_high_school, NHPI_high_school_to_no_bachelors, NHPI_bachelors_or_more,
         Other_total, Other_no_high_school, Other_high_school_to_no_bachelors, Other_bachelors_or_more,
         Two_or_more_total, Two_or_more_no_high_school, Two_or_more_high_school_to_no_bachelors, Two_or_more_bachelors_or_more,
         Hispanic_total, Hispanic_no_high_school, Hispanic_high_school_to_no_bachelors, Hispanic_bachelors_or_more,)

# Generating CSV:

write.csv(cville_educ_attain_by_race_county_processed, "temp_data/cville_educ_attain_counts_by_race_2022.csv")

# Attainment categories with percentages

cville_educ_attain_by_race_2022 <- cville_educ_attain_by_race_county_processed %>%
  mutate(White_no_high_school_pct = (`White_no_high_school`) / `White_total` * 100,
         White_high_school_to_no_bachelors_pct = (`White_high_school_to_no_bachelors`) / `White_total` * 100,
         White_bachelors_or_more_pct = (`White_bachelors_or_more`) / `White_total` * 100,
         White_non_hisp_no_high_school_pct = (`White_non_hisp_no_high_school`) / `White_total` * 100,
         White_non_hisp_high_school_to_no_bachelors_pct = (`White_non_hisp_high_school_to_no_bachelors`) / `White_total` * 100,
         White_non_hisp_bachelors_or_more_pct = (`White_non_hisp_bachelors_or_more`) / `White_total` * 100,
         Black_no_high_school_pct = (`Black_no_high_school`) / `Black_total` * 100,
         Black_high_school_to_no_bachelors_pct = (`Black_high_school_to_no_bachelors`) / `Black_total` * 100,
         Black_bachelors_or_more_pct = (`Black_bachelors_or_more`) / `Black_total` * 100,
         Amer_Indian_no_high_school_pct = (`Amer_Indian_no_high_school`) / `Amer_Indian_total` * 100,
         Amer_Indian_high_school_to_no_bachelors_pct = (`Amer_Indian_high_school_to_no_bachelors`) / `Amer_Indian_total` * 100,
         Amer_Indian_bachelors_or_more_pct = (`Amer_Indian_bachelors_or_more`) / `Amer_Indian_total` * 100,
         Asian_no_high_school_pct = (`Asian_no_high_school`) / `Asian_total` * 100,
         Asian_high_school_to_no_bachelors_pct = (`Asian_high_school_to_no_bachelors`) / `Asian_total` * 100,
         Asian_bachelors_or_more_pct = (`Asian_bachelors_or_more`) / `Asian_total` * 100,
         NHPI_no_high_school_pct = (`NHPI_no_high_school`) / `NHPI_total` * 100,
         NHPI_high_school_to_no_bachelors_pct = (`NHPI_high_school_to_no_bachelors`) / `NHPI_total` * 100,
         NHPI_bachelors_or_more_pct = (`NHPI_bachelors_or_more`) / `NHPI_total` * 100,
         Other_no_high_school_pct = (`Other_no_high_school`) / `Other_total` * 100,
         Other_high_school_to_no_bachelors_pct = (`Other_high_school_to_no_bachelors`) / `Other_total` * 100,
         Other_bachelors_or_more_pct = (`Other_bachelors_or_more`) / `Other_total` * 100,
         Two_or_more_no_high_school_pct = (`Two_or_more_no_high_school`) / `Two_or_more_total` * 100,
         Two_or_more_high_school_to_no_bachelors_pct = (`Two_or_more_high_school_to_no_bachelors`) / `Two_or_more_total` * 100,
         Two_or_more_bachelors_or_more_pct = (`Two_or_more_bachelors_or_more`) / `Two_or_more_total` * 100,
         Hispanic_no_high_school_pct = (`Hispanic_no_high_school`) / `Hispanic_total` * 100,
         Hispanic_high_school_to_no_bachelors_pct = (`Hispanic_high_school_to_no_bachelors`) / `Hispanic_total` * 100,
         Hispanic_bachelors_or_more_pct = (`Hispanic_bachelors_or_more`) / `Hispanic_total` * 100,
  ) %>% 
  select(GEOID, NAME, 
         White_no_high_school_pct, White_high_school_to_no_bachelors_pct, White_bachelors_or_more_pct,
         White_non_hisp_no_high_school_pct, White_non_hisp_high_school_to_no_bachelors_pct, White_non_hisp_bachelors_or_more_pct,
         Black_no_high_school_pct, Black_high_school_to_no_bachelors_pct, Black_bachelors_or_more_pct,
         Amer_Indian_no_high_school_pct, Amer_Indian_high_school_to_no_bachelors_pct, Amer_Indian_bachelors_or_more_pct,
         Asian_no_high_school_pct, Asian_high_school_to_no_bachelors_pct, Asian_bachelors_or_more_pct,
         NHPI_no_high_school_pct, NHPI_high_school_to_no_bachelors_pct, NHPI_bachelors_or_more_pct,
         Other_no_high_school_pct, Other_high_school_to_no_bachelors_pct, Other_bachelors_or_more_pct,
         Two_or_more_no_high_school_pct, Two_or_more_high_school_to_no_bachelors_pct, Two_or_more_bachelors_or_more_pct,
         Hispanic_no_high_school_pct, Hispanic_high_school_to_no_bachelors_pct, Hispanic_bachelors_or_more_pct,)

# Generating CSV:

write.csv(cville_educ_attain_by_race_2022, "temp_data/cville_educ_attain_pct_by_race_2022.csv")

# Albemarle (Educational Attainment by Race / Ethnicity), 2022

# Pull the data (county level)
educ_attain_by_race_alb_county_2022 <- get_acs(
  geography = "county",
  county = "003",
  state = "VA",
  table = "S1501_C01",
  survey = "acs5",
  year = 2022,
  cache = TRUE
)

# Reshape the data
educ_attain_alb_county_wide <- educ_attain_by_race_alb_county_2022 %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Attainment categories with counts
alb_educ_attain_by_race_county_processed <- educ_attain_alb_county_wide %>%
  mutate(
    White_total = `S1501_C01_028`,
    White_no_high_school = `S1501_C01_028` - `S1501_C01_029`,
    White_high_school_to_no_bachelors = `S1501_C01_029` - `S1501_C01_030`,
    White_bachelors_or_more = `S1501_C01_030`,
    White_non_hisp_total = `S1501_C01_031`,
    White_non_hisp_no_high_school = `S1501_C01_031` - `S1501_C01_032`,
    White_non_hisp_high_school_to_no_bachelors = `S1501_C01_032` - `S1501_C01_033`,
    White_non_hisp_bachelors_or_more = `S1501_C01_033`,
    Black_total = `S1501_C01_034`,
    Black_no_high_school = `S1501_C01_034` - `S1501_C01_035`,
    Black_high_school_to_no_bachelors = `S1501_C01_035` - `S1501_C01_036`,
    Black_bachelors_or_more = `S1501_C01_036`,
    Amer_Indian_total = `S1501_C01_037`,
    Amer_Indian_no_high_school = `S1501_C01_037` - `S1501_C01_038`,
    Amer_Indian_high_school_to_no_bachelors = `S1501_C01_038` - `S1501_C01_039`,
    Amer_Indian_bachelors_or_more = `S1501_C01_039`,
    Asian_total = `S1501_C01_040`,
    Asian_no_high_school = `S1501_C01_040` - `S1501_C01_041`,
    Asian_high_school_to_no_bachelors = `S1501_C01_041` - `S1501_C01_042`,
    Asian_bachelors_or_more = `S1501_C01_042`,
    NHPI_total = `S1501_C01_043`,
    NHPI_no_high_school = `S1501_C01_043` - `S1501_C01_044`,
    NHPI_high_school_to_no_bachelors = `S1501_C01_044` - `S1501_C01_045`,
    NHPI_bachelors_or_more = `S1501_C01_045`,
    Other_total = `S1501_C01_046`,
    Other_no_high_school = `S1501_C01_046` - `S1501_C01_047`,
    Other_high_school_to_no_bachelors = `S1501_C01_047` - `S1501_C01_048`,
    Other_bachelors_or_more = `S1501_C01_048`,
    Two_or_more_total = `S1501_C01_049`,
    Two_or_more_no_high_school = `S1501_C01_049` - `S1501_C01_050`,
    Two_or_more_high_school_to_no_bachelors = `S1501_C01_050` - `S1501_C01_051`,
    Two_or_more_bachelors_or_more = `S1501_C01_051`,
    Hispanic_total = `S1501_C01_052`,
    Hispanic_no_high_school = `S1501_C01_052` - `S1501_C01_053`,
    Hispanic_high_school_to_no_bachelors = `S1501_C01_053` - `S1501_C01_054`,
    Hispanic_bachelors_or_more = `S1501_C01_054`,
  ) %>% 
  select(GEOID, NAME, 
         White_total, White_no_high_school, White_high_school_to_no_bachelors, White_bachelors_or_more,
         White_non_hisp_total, White_non_hisp_no_high_school, White_non_hisp_high_school_to_no_bachelors, White_non_hisp_bachelors_or_more,
         Black_total, Black_no_high_school, Black_high_school_to_no_bachelors, Black_bachelors_or_more,
         Amer_Indian_total, Amer_Indian_no_high_school, Amer_Indian_high_school_to_no_bachelors, Amer_Indian_bachelors_or_more,
         Asian_total, Asian_no_high_school, Asian_high_school_to_no_bachelors, Asian_bachelors_or_more,
         NHPI_total, NHPI_no_high_school, NHPI_high_school_to_no_bachelors, NHPI_bachelors_or_more,
         Other_total, Other_no_high_school, Other_high_school_to_no_bachelors, Other_bachelors_or_more,
         Two_or_more_total, Two_or_more_no_high_school, Two_or_more_high_school_to_no_bachelors, Two_or_more_bachelors_or_more,
         Hispanic_total, Hispanic_no_high_school, Hispanic_high_school_to_no_bachelors, Hispanic_bachelors_or_more,)

# Generating CSV:

write.csv(alb_educ_attain_by_race_county_processed, "temp_data/alb_educ_attain_counts_by_race_2022.csv")

# Attainment categories with percentages

alb_educ_attain_by_race_2022 <- alb_educ_attain_by_race_county_processed %>%
  mutate(White_no_high_school_pct = (`White_no_high_school`) / `White_total` * 100,
         White_high_school_to_no_bachelors_pct = (`White_high_school_to_no_bachelors`) / `White_total` * 100,
         White_bachelors_or_more_pct = (`White_bachelors_or_more`) / `White_total` * 100,
         White_non_hisp_no_high_school_pct = (`White_non_hisp_no_high_school`) / `White_total` * 100,
         White_non_hisp_high_school_to_no_bachelors_pct = (`White_non_hisp_high_school_to_no_bachelors`) / `White_total` * 100,
         White_non_hisp_bachelors_or_more_pct = (`White_non_hisp_bachelors_or_more`) / `White_total` * 100,
         Black_no_high_school_pct = (`Black_no_high_school`) / `Black_total` * 100,
         Black_high_school_to_no_bachelors_pct = (`Black_high_school_to_no_bachelors`) / `Black_total` * 100,
         Black_bachelors_or_more_pct = (`Black_bachelors_or_more`) / `Black_total` * 100,
         Amer_Indian_no_high_school_pct = (`Amer_Indian_no_high_school`) / `Amer_Indian_total` * 100,
         Amer_Indian_high_school_to_no_bachelors_pct = (`Amer_Indian_high_school_to_no_bachelors`) / `Amer_Indian_total` * 100,
         Amer_Indian_bachelors_or_more_pct = (`Amer_Indian_bachelors_or_more`) / `Amer_Indian_total` * 100,
         Asian_no_high_school_pct = (`Asian_no_high_school`) / `Asian_total` * 100,
         Asian_high_school_to_no_bachelors_pct = (`Asian_high_school_to_no_bachelors`) / `Asian_total` * 100,
         Asian_bachelors_or_more_pct = (`Asian_bachelors_or_more`) / `Asian_total` * 100,
         NHPI_no_high_school_pct = (`NHPI_no_high_school`) / `NHPI_total` * 100,
         NHPI_high_school_to_no_bachelors_pct = (`NHPI_high_school_to_no_bachelors`) / `NHPI_total` * 100,
         NHPI_bachelors_or_more_pct = (`NHPI_bachelors_or_more`) / `NHPI_total` * 100,
         Other_no_high_school_pct = (`Other_no_high_school`) / `Other_total` * 100,
         Other_high_school_to_no_bachelors_pct = (`Other_high_school_to_no_bachelors`) / `Other_total` * 100,
         Other_bachelors_or_more_pct = (`Other_bachelors_or_more`) / `Other_total` * 100,
         Two_or_more_no_high_school_pct = (`Two_or_more_no_high_school`) / `Two_or_more_total` * 100,
         Two_or_more_high_school_to_no_bachelors_pct = (`Two_or_more_high_school_to_no_bachelors`) / `Two_or_more_total` * 100,
         Two_or_more_bachelors_or_more_pct = (`Two_or_more_bachelors_or_more`) / `Two_or_more_total` * 100,
         Hispanic_no_high_school_pct = (`Hispanic_no_high_school`) / `Hispanic_total` * 100,
         Hispanic_high_school_to_no_bachelors_pct = (`Hispanic_high_school_to_no_bachelors`) / `Hispanic_total` * 100,
         Hispanic_bachelors_or_more_pct = (`Hispanic_bachelors_or_more`) / `Hispanic_total` * 100,
  ) %>% 
  select(GEOID, NAME, 
         White_no_high_school_pct, White_high_school_to_no_bachelors_pct, White_bachelors_or_more_pct,
         White_non_hisp_no_high_school_pct, White_non_hisp_high_school_to_no_bachelors_pct, White_non_hisp_bachelors_or_more_pct,
         Black_no_high_school_pct, Black_high_school_to_no_bachelors_pct, Black_bachelors_or_more_pct,
         Amer_Indian_no_high_school_pct, Amer_Indian_high_school_to_no_bachelors_pct, Amer_Indian_bachelors_or_more_pct,
         Asian_no_high_school_pct, Asian_high_school_to_no_bachelors_pct, Asian_bachelors_or_more_pct,
         NHPI_no_high_school_pct, NHPI_high_school_to_no_bachelors_pct, NHPI_bachelors_or_more_pct,
         Other_no_high_school_pct, Other_high_school_to_no_bachelors_pct, Other_bachelors_or_more_pct,
         Two_or_more_no_high_school_pct, Two_or_more_high_school_to_no_bachelors_pct, Two_or_more_bachelors_or_more_pct,
         Hispanic_no_high_school_pct, Hispanic_high_school_to_no_bachelors_pct, Hispanic_bachelors_or_more_pct,)

# Generating CSV:

write.csv(alb_educ_attain_by_race_2022, "temp_data/alb_educ_attain_pct_by_race_2022.csv")

# County School Education Stats (AP Courses, Suspensions, Absenteeism)

# AP Course Enrollment (by Race / Ethnicity)

# Source: www.doe.virginia.gov
  # url: https://www.doe.virginia.gov/home/showpublisheddocument/50006/638319308012100000

ap_enrollment_va <- read_xlsx("temp_data/ap_classes.xlsx", sheet = "Adv Programs by Race", skip = 3)

# Read the Excel file, skipping the first 4 rows
ap_enrollment_va <- read_xlsx("temp_data/ap_classes.xlsx", sheet = "Adv Programs by Race", skip = 4)

# Read the Excel file again to get the correct header row (assuming the header is on the 4th row)
headers <- read_xlsx("temp_data/ap_classes.xlsx", sheet = "Adv Programs by Race", skip = 3, n_max = 1)

# Set the column names of the first dataframe using the second dataframe's header row
colnames(ap_enrollment_va) <- colnames(headers)

# Generating CSV:

write.csv(ap_enrollment_va, "temp_data/ap_enrollment_va.csv")

  # Issue with first row... includes years for some reason?

# Suspensions (by Race / Ethnicity)

# Source: ACPS report, but available at schoolquality.virginia.gov (go to Download Data, Select Division, Learning Climate, Long/Short Term suspensions, most recent year; download)

# Charlottesville (Short Term Suspensions, 2022-2023)

cville_st_suspensions <- read.csv("temp_data/cville_st_suspensions.csv", skip = 3)

# Generating CSV:

write.csv(cville_st_suspensions, "temp_data/cville_st_suspensions.csv")

# Albemarle (Short Term Suspensions, 2022-2023)

alb_st_suspensions <- read.csv("temp_data/alb_st_suspensions.csv", skip = 3)

# Generating CSV:

write.csv(alb_st_suspensions, "temp_data/alb_st_suspensions.csv")

# Chronic Absenteeism (by Race / Ethnicity)

# Source: ACPS report, but available at schoolquality.virginia.gov (go to Download Data, Select Division, Learning Climate, Chronic Absenteeism, most recent year; download)

# Charlottesville (Chronic Absenteeism), 2022-2023)

cville_absenteeism <- read.csv("temp_data/cville_absenteeism.csv", skip = 3)

# Generating CSV:

write.csv(cville_absenteeism, "temp_data/cville_absenteeism.csv")

# Albemarle (Chronic Absenteeism), 2022-2023)

alb_absenteeism <- read.csv("temp_data/alb_absenteeism.csv", skip = 3)

# Generating CSV:

write.csv(alb_absenteeism, "temp_data/alb_absenteeism.csv")