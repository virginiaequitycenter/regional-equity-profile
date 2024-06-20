# R Script for pulling and examining demographic data
# Author: Henry DeMarco
# Date Created: June 4, 2024
# Last Updated: June 20, 2024
      # Progress Tracking Spreadsheet Updated: June 17, 2024 (!)

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

#(!) indicates temporary comments to remove

# Primary year-to-year comparisons will be 2012/2022 (!)

#Packages
library(tidyverse)
library(tidycensus)

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

# Demographic Table: DP05 (ACS Demographic and Housing Characteristics)

# Charlottesville (ACS Demographic and Housing Characteristics), 2012-2022:

demographic_tables_cville <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "540",
           # variables =  race_vars$variable,
           table = "DP05",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x)
  )

# Albemarle (ACS Demographic and Housing Characteristics), 2012-2022:

demographic_tables_alb <-
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "003",
           # variables =  race_vars$variable,
           table = "DP05",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x)
  )

# Generating DP05 Labels

acs5_dp_labels <- 
  map_df(2022:2012,
         ~ load_variables(.x, 
                          "acs5/profile", 
                          cache = TRUE) %>%
           mutate(year = .x)
  ) %>%
  rename(variable = name)

#Wrangling DPO5 Table (Charlottesville)

cville_demographic_profile_2012_2022 <-
  demographic_tables_cville %>%
  left_join(acs5_dp_labels) %>%
  separate(label,
           c("stat", "category", "group", "level", "restriction", "etc"),
           sep = "!!") %>%  
  filter(stat %in% c("Percent", "Percent Estimate"))  %>%
  filter(!is.na(estimate)) %>%
  mutate(
    final_level =
      case_when(
        category == "SEX AND AGE" &
          group == "Total population" & !is.na(level) ~ level,
        category == "SEX AND AGE"  ~ group,
        category == "RACE" & group == "Total population" & level == "One race" & !is.na(restriction) & is.na(etc) ~ restriction,
        category == "RACE" & group == "One race" & !is.na(level) & is.na(restriction) ~ level,
        category == "RACE" & group == "Total population" & level == "Two or more races" & is.na(restriction)  ~ level,
        category == "RACE" & group == "Two or more races" & is.na(level)  ~ group,
        category == "HISPANIC OR LATINO AND RACE" & group == "Total population" & !is.na(level) & is.na(restriction) ~ level,
        category == "HISPANIC OR LATINO AND RACE" & !is.na(group)  & is.na(level) ~ group,
        
        TRUE ~ NA_character_
        
      )
  ) %>%
  filter(
    !final_level %in% c(
      "Under 18 years",
      "16 years and over",
      "18 years and over",
      "21 years and over",
      "62 years and over",
      "65 years and over"
    ),
    !is.na(final_level), #Excluding variables from this list (!)
    !variable %in% c(
      "DP05_0023P",
      "DP05_0024P",
      "DP05_0026P",
      "DP05_0027P"
    )
  ) %>% 
  select(variable, estimate, moe, year, category, final_level ) 

cville_demographic_table_2012_2022 <-
  cville_demographic_profile_2012_2022 %>%
  select(-moe, - variable) %>%
  distinct() %>%
  spread(year, estimate) #%>%
# select(-category) %>%
# distinct()

# Generating CSV:

write.csv(cville_demographic_table_2012_2022, "data/cville_demographic_table_2012_2022.csv")

#Wrangling DPO5 Table (Albemarle)

alb_demographic_profile_2012_2022 <-
  demographic_tables_alb %>%
  left_join(acs5_dp_labels) %>%
  separate(label,
           c("stat", "category", "group", "level", "restriction", "etc"),
           sep = "!!") %>%  
  filter(stat %in% c("Percent", "Percent Estimate"))  %>%
  filter(!is.na(estimate)) %>%
  mutate(
    final_level =
      case_when(
        category == "SEX AND AGE" &
          group == "Total population" & !is.na(level) ~ level,
        category == "SEX AND AGE"  ~ group,
        category == "RACE" & group == "Total population" & level == "One race" & !is.na(restriction) & is.na(etc) ~ restriction,
        category == "RACE" & group == "One race" & !is.na(level) & is.na(restriction) ~ level,
        category == "RACE" & group == "Total population" & level == "Two or more races" & is.na(restriction)  ~ level,
        category == "RACE" & group == "Two or more races" & is.na(level)  ~ group,
        category == "HISPANIC OR LATINO AND RACE" & group == "Total population" & !is.na(level) & is.na(restriction) ~ level,
        category == "HISPANIC OR LATINO AND RACE" & !is.na(group)  & is.na(level) ~ group,
        
        TRUE ~ NA_character_
        
      )
  ) %>%
  filter(
    !final_level %in% c(
      "Under 18 years",
      "16 years and over",
      "18 years and over",
      "21 years and over",
      "62 years and over",
      "65 years and over"
    ),
    !is.na(final_level),
    !variable %in% c(
      "DP05_0023P",
      "DP05_0024P",
      "DP05_0026P",
      "DP05_0027P"
    )
  ) %>% 
  select(variable, estimate, moe, year, category, final_level ) 

alb_demographic_table_2012_2022 <-
  alb_demographic_profile_2012_2022 %>%
  select(-moe, - variable) %>%
  distinct() %>%
  spread(year, estimate) #%>%
# select(-category) %>%
# distinct()

# Generating CSV:

write.csv(alb_demographic_table_2012_2022, "data/alb_demographic_table_2012_2022.csv")

# Table: B02001 (Race)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Race), 2012-2022:

race_cville_2012_2022 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "540",
           table = "B02001",
           summary_var = "B02001_001",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008"))
  )

# Finalizing table:

cville_race_2012_2022 <- race_cville_2012_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         label = case_when(
           variable == "B02001_002" ~ "White alone",
           variable == "B02001_003" ~ "Black or African American alone",
           variable == "B02001_004" ~ "American Indian and Alaska Native alone",
           variable == "B02001_005" ~ "Asian alone",
           variable == "B02001_006" ~ "Native Hawaiian and Other Pacific Islander alone",
           variable == "B02001_007" ~ "Some Other Race alone",
           variable == "B02001_008" ~ "Two or More Races",
         ))

# Generating CSV:

write.csv(cville_race_2012_2022, "data/cville_race_2012_2022.csv")

  # Albemarle (Race), 2012-2022:

race_alb_2012_2022 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "003",
           table = "B02001",
           summary_var = "B02001_001",
           survey = "acs5",
           cache = TRUE
         ) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008"))
  )

# Finalizing table:

alb_race_2012_2022 <- race_alb_2012_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         label = case_when(
           variable == "B02001_002" ~ "White alone",
           variable == "B02001_003" ~ "Black or African American alone",
           variable == "B02001_004" ~ "American Indian and Alaska Native alone",
           variable == "B02001_005" ~ "Asian alone",
           variable == "B02001_006" ~ "Native Hawaiian and Other Pacific Islander alone",
           variable == "B02001_007" ~ "Some Other Race alone",
           variable == "B02001_008" ~ "Two or More Races",
         ))

# Generating CSV:

write.csv(alb_race_2012_2022, "data/alb_race_2012_2022.csv")

# Charlottesville, Albemarle Combined Table

combined_race_2012_2022 <- rbind(alb_race_2012_2022, cville_race_2012_2022)

# Table: B03002 (Hispanic or Latino Origin by Race)

# This might be the better table than DP05 (!)
# Rows match up for 2012 and 2022 (!)

  # Charlottesville (Hispanic or Latino Origin by Race), 2012-2022:

hispanic_origin_by_race_cville_2012_2022 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "540",
           table = "B03002",
           summary_var = "B03002_001",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B03002_003", "B03002_004", "B03002_005", "B03002_006", "B03002_007", "B03002_008", "B03002_009", "B03002_012"))
  )

# Finalizing table:

cville_hispanic_origin_by_race_cville_2012_2022 <- hispanic_origin_by_race_cville_2012_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         label = case_when(
           variable == "B03002_003" ~ "Not Hispanic or Latino: White alone",
           variable == "B03002_004" ~ "Not Hispanic or Latino: Black or African American alone",
           variable == "B03002_005" ~ "Not Hispanic or Latino: American Indian and Alaska Native alone",
           variable == "B03002_006" ~ "Not Hispanic or Latino: Asian alone",
           variable == "B03002_007" ~ "Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone",
           variable == "B03002_008" ~ "Not Hispanic or Latino: Some other race alone",
           variable == "B03002_009" ~ "Not Hispanic or Latino: Two or more races",
           variable == "B03002_012" ~ "Hispanic or Latino",
         ))

# Generating CSV:

write.csv(cville_hispanic_origin_by_race_cville_2012_2022, "data/cville_hispanic_origin_by_race_cville_2012_2022.csv")

# Albemarle (Hispanic or Latino Origin by Race), 2012-2022:

hispanic_origin_by_race_alb_2012_2022 <- 
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
           filter(variable %in% c("B03002_003", "B03002_004", "B03002_005", "B03002_006", "B03002_007", "B03002_008", "B03002_009", "B03002_012"))
  )

# Finalizing table:

alb_hispanic_origin_by_race_2012_2022 <- hispanic_origin_by_race_alb_2012_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         label = case_when(
           variable == "B03002_003" ~ "Not Hispanic or Latino: White alone",
           variable == "B03002_004" ~ "Not Hispanic or Latino: Black or African American alone",
           variable == "B03002_005" ~ "Not Hispanic or Latino: American Indian and Alaska Native alone",
           variable == "B03002_006" ~ "Not Hispanic or Latino: Asian alone",
           variable == "B03002_007" ~ "Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone",
           variable == "B03002_008" ~ "Not Hispanic or Latino: Some other race alone",
           variable == "B03002_009" ~ "Not Hispanic or Latino: Two or more races",
           variable == "B03002_012" ~ "Hispanic or Latino",
         ))

# Generating CSV:

write.csv(alb_hispanic_origin_by_race_2012_2022, "data/alb_hispanic_origin_by_race_2012_2022.csv")

# Charlottesville, Albemarle Combined Table

combined_ethnicity_by_race_2012_2022 <- rbind(alb_hispanic_origin_by_race_2012_2022, cville_hispanic_origin_by_race_cville_2012_2022)

# Table: B03003 (Hispanic or Latino Origin)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Hispanic or Latino Origin), 2012-2022

hispanic_origin_cville_2012_2022 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "540",
           table = "B03003",
           summary_var = "B03003_001",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B03003_002", "B03003_003"))
  )

# Finalizing table:

cville_hispanic_origin_2012_2022 <- hispanic_origin_cville_2012_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         label = case_when(
           variable == "B03003_002" ~ "Not Hispanic or Latino",
           variable == "B03003_003" ~ "Hispanic or Latino"
         ))

# Generating CSV:

write.csv(cville_hispanic_origin_2012_2022, "data/cville_hispanic_origin_2012_2022.csv")

# Albemarle (Hispanic or Latino Origin), 2012-2022

hispanic_origin_alb_2012_2022 <- 
  map_df(2022:2012,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "003",
           table = "B03003",
           summary_var = "B03003_001",
           survey = "acs5", 
           cache = TRUE
         ) %>%
           mutate(year = .x) %>% 
           filter(variable %in% c("B03003_002", "B03003_003"))
  )

# Finalizing table:

alb_hispanic_origin_2012_2022 <- hispanic_origin_alb_2012_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         label = case_when(
           variable == "B03003_002" ~ "Not Hispanic or Latino",
           variable == "B03003_003" ~ "Hispanic or Latino"
         ))

# Generating CSV:

write.csv(alb_hispanic_origin_2012_2022, "data/alb_hispanic_origin_2012_2022.csv")

# Charlottesville, Albemarle Combined Table

combined_ethnicity_2012_2022 <- rbind(alb_hispanic_origin_2012_2022, cville_hispanic_origin_2012_2022)

# Table: B01001 (Sex by Age)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Age Groups), 2022

sex_by_age_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B01001",
  summary_var = "B01001_001",
  survey = "acs5",
  year = 2022
)

# Aggregating sex for age groups:

# Mapping male and female variables
age_groups <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 and 19 years", "20 years", "21 years", "22 to 24 years", "25 to 29 years", 
                "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", 
                "50 to 54 years", "55 to 59 years", "60 and 61 years", "62 to 64 years", 
                "65 and 66 years", "67 to 69 years", "70 to 74 years", "75 to 79 years", 
                "80 to 84 years", "85 years and over"),
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

# Summing estimates for corresponding age groups
cville_age_groups_2022 <- age_groups %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    sex_by_age_cville_2022 %>%
      filter(variable %in% c(male_var, female_var)) %>%
      group_by(GEOID, NAME) %>%
      summarize(
        estimate = sum(estimate),
        summary_est = first(summary_est),
        .groups = 'drop'
      ) %>%
      mutate(
        age_group = age_group,
        percent = 100 * (estimate / summary_est),
        year = "2022"
      )
  })

# Charlottesville (Age Groups), 2012

sex_by_age_cville_2012 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B01001",
  summary_var = "B01001_001",
  survey = "acs5",
  year = 2012
)

# Aggregating sex for age groups:

# Mapping male and female variables
age_groups <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 and 19 years", "20 years", "21 years", "22 to 24 years", "25 to 29 years", 
                "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", 
                "50 to 54 years", "55 to 59 years", "60 and 61 years", "62 to 64 years", 
                "65 and 66 years", "67 to 69 years", "70 to 74 years", "75 to 79 years", 
                "80 to 84 years", "85 years and over"),
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

# Summing estimates for corresponding age groups
cville_age_groups_2012 <- age_groups %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    sex_by_age_cville_2012 %>%
      filter(variable %in% c(male_var, female_var)) %>%
      group_by(GEOID, NAME) %>%
      summarize(
        estimate = sum(estimate),
        summary_est = first(summary_est),
        .groups = 'drop'
      ) %>%
      mutate(
        age_group = age_group,
        percent = 100 * (estimate / summary_est),
        year = "2012"
      )
  })

# Charlottesville Combined Table (2012, 2022)

cville_combined_age_2012_2022 <- rbind(cville_age_groups_2012, cville_age_groups_2022)

# Generating CSV:

write.csv(cville_combined_age_2012_2022, "data/cville_combined_age_2012_2022.csv")

# Albemarle (Age Groups), 2022

sex_by_age_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B01001",
  summary_var = "B01001_001",
  survey = "acs5",
  year = 2022
)

# Aggregating sex for age groups:

# Mapping male and female variables
age_groups <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 and 19 years", "20 years", "21 years", "22 to 24 years", "25 to 29 years", 
                "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", 
                "50 to 54 years", "55 to 59 years", "60 and 61 years", "62 to 64 years", 
                "65 and 66 years", "67 to 69 years", "70 to 74 years", "75 to 79 years", 
                "80 to 84 years", "85 years and over"),
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

# Summing estimates for corresponding age groups
alb_age_groups_2022 <- age_groups %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    sex_by_age_alb_2022 %>%
      filter(variable %in% c(male_var, female_var)) %>%
      group_by(GEOID, NAME) %>%
      summarize(
        estimate = sum(estimate),
        summary_est = first(summary_est),
        .groups = 'drop'
      ) %>%
      mutate(
        age_group = age_group,
        percent = 100 * (estimate / summary_est),
        year = "2022"
      )
  })

# Albemarle (Age Groups), 2012

sex_by_age_alb_2012 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B01001",
  summary_var = "B01001_001",
  survey = "acs5",
  year = 2012
)

# Aggregating sex for age groups:

# Mapping male and female variables
age_groups <- tibble::tibble(
  age_group = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years", 
                "18 and 19 years", "20 years", "21 years", "22 to 24 years", "25 to 29 years", 
                "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", 
                "50 to 54 years", "55 to 59 years", "60 and 61 years", "62 to 64 years", 
                "65 and 66 years", "67 to 69 years", "70 to 74 years", "75 to 79 years", 
                "80 to 84 years", "85 years and over"),
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

# Summing estimates for corresponding age groups
alb_age_groups_2012 <- age_groups %>%
  pmap_dfr(function(age_group, male_var, female_var) {
    sex_by_age_alb_2012 %>%
      filter(variable %in% c(male_var, female_var)) %>%
      group_by(GEOID, NAME) %>%
      summarize(
        estimate = sum(estimate),
        summary_est = first(summary_est),
        .groups = 'drop'
      ) %>%
      mutate(
        age_group = age_group,
        percent = 100 * (estimate / summary_est),
        year = "2012"
      )
  })

# Albemarle Combined Table (2012, 2022)

alb_combined_age_2012_2022 <- rbind(alb_age_groups_2012, alb_age_groups_2022)

# Generating CSV:

write.csv(alb_combined_age_2012_2022, "data/alb_combined_age_2012_2022.csv")

# Table: B05001 (Nativity and Citizenship Status in the United States)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Nativity), 2022

nativity_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B05001",
  summary_var = "B05001_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable %in% c("B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006"))

# Finalizing table:

cville_nativity_2022 <- nativity_cville_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B05001_002" ~ "U.S. citizen, born in the United States",
           variable == "B05001_003" ~ "U.S. citizen, born in Puerto Rico or U.S. Island Areas",
           variable == "B05001_004" ~ "U.S. citizen, born abroad of American parent(s)",
           variable == "B05001_005" ~ "U.S. citizen by naturalization",
           variable == "B05001_006" ~ "Not a U.S. citizen"
         )
  )

# Generating CSV:

write.csv(cville_nativity_2022, "data/cville_nativity_2022.csv")

# Albemarle (Nativity), 2022

nativity_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B05001",
  summary_var = "B05001_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable %in% c("B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006")
  )

# Finalizing table:

alb_nativity_2022 <- nativity_alb_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B05001_002" ~ "U.S. citizen, born in the United States",
           variable == "B05001_003" ~ "U.S. citizen, born in Puerto Rico or U.S. Island Areas",
           variable == "B05001_004" ~ "U.S. citizen, born abroad of American parent(s)",
           variable == "B05001_005" ~ "U.S. citizen by naturalization",
           variable == "B05001_006" ~ "Not a U.S. citizen"
         )
  )

# Generating CSV:

write.csv(alb_nativity_2022, "data/alb_nativity_2022.csv")

# Table: B05002 (Place of Birth by Nativity)
  # 2022 and 2012 rows do not match (!)

# Charlottesville (Place of Birth by Nativity), 2022

place_of_birth_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B05002",
  summary_var = "B05002_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable != "B05002_001")

# Intermediate table:

cville_place_of_birth_2022 <- place_of_birth_cville_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B05002_002" ~ "Native",
           variable == "B05002_003" ~ "Native: Born in state of residence",
           variable == "B05002_004" ~ "Native: Born in other state in the United States",
           variable == "B05002_005" ~ "Native: Born in other state in the United States: Northeast",
           variable == "B05002_006" ~ "Native: Born in other state in the United States: Midwest",
           variable == "B05002_007" ~ "Native: Born in other state in the United States: South",
           variable == "B05002_008" ~ "Native: Born in other state in the United States: West",
           variable == "B05002_009" ~ "Native: Born outside the United States",
           variable == "B05002_010" ~ "Native: Born outside the United States: Puerto Rico",
           variable == "B05002_011" ~ "Native: Born outside the United States: U.S. Island Areas",
           variable == "B05002_012" ~ "Native: Born outside the United States: Born abroad of American parent(s)",
           variable == "B05002_013" ~ "Foreign born",
           variable == "B05002_014" ~ "Foreign born: Naturalized U.S. citizen",
           variable == "B05002_015" ~ "Foreign born: Naturalized U.S. citizen: Europe",
           variable == "B05002_016" ~ "Foreign born: Naturalized U.S. citizen: Asia",
           variable == "B05002_017" ~ "Foreign born: Naturalized U.S. citizen: Africa",
           variable == "B05002_018" ~ "Foreign born: Naturalized U.S. citizen: Oceania",
           variable == "B05002_019" ~ "Foreign born: Naturalized U.S. citizen: Latin America",
           variable == "B05002_020" ~ "Foreign born: Naturalized U.S. citizen: Northern America",
           variable == "B05002_021" ~ "Foreign born: Not a U.S. citizen",
           variable == "B05002_022" ~ "Foreign born: Not a U.S. citizen: Europe",
           variable == "B05002_023" ~ "Foreign born: Not a U.S. citizen: Asia",
           variable == "B05002_024" ~ "Foreign born: Not a U.S. citizen: Africa",
           variable == "B05002_025" ~ "Foreign born: Not a U.S. citizen: Oceania",
           variable == "B05002_026" ~ "Foreign born: Not a U.S. citizen: Latin America",
           variable == "B05002_027" ~ "Foreign born: Not a U.S. citizen: Northern America"
         )
  )

# Filtering out subcategories for 'Foreign born: naturalized U.S. citizen' and 'Native'

cville_place_of_birth_2022 <- cville_place_of_birth_2022 %>% 
  filter(variable %in% c("B05002_002", "B05002_013", "B05002_014", "B05002_021", "B05002_022", "B05002_023", "B05002_024", "B05002_025", "B05002_026", "B05002_027")) %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B05002_002" ~ "Native",
           variable == "B05002_013" ~ "Foreign born",
           variable == "B05002_014" ~ "Foreign born: Naturalized U.S. citizen",
           variable == "B05002_021" ~ "Foreign born: Not a U.S. citizen",
           variable == "B05002_022" ~ "Foreign born: Not a U.S. citizen: Europe",
           variable == "B05002_023" ~ "Foreign born: Not a U.S. citizen: Asia",
           variable == "B05002_024" ~ "Foreign born: Not a U.S. citizen: Africa",
           variable == "B05002_025" ~ "Foreign born: Not a U.S. citizen: Oceania",
           variable == "B05002_026" ~ "Foreign born: Not a U.S. citizen: Latin America",
           variable == "B05002_027" ~ "Foreign born: Not a U.S. citizen: Northern America"
         )
  )

# Generating CSV:

write.csv(cville_place_of_birth_2022, "data/cville_place_of_birth_2022.csv")

# Albemarle (Place of Birth by Nativity), 2022

place_of_birth_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B05002",
  summary_var = "B05002_001",
  survey = "acs5",
  year = 2022
)

# Intermediate table:

alb_place_of_birth_2022 <- place_of_birth_alb_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B05002_001" ~ "Total",
           variable == "B05002_002" ~ "Native",
           variable == "B05002_003" ~ "Native: Born in state of residence",
           variable == "B05002_004" ~ "Native: Born in other state in the United States",
           variable == "B05002_005" ~ "Native: Born in other state in the United States: Northeast",
           variable == "B05002_006" ~ "Native: Born in other state in the United States: Midwest",
           variable == "B05002_007" ~ "Native: Born in other state in the United States: South",
           variable == "B05002_008" ~ "Native: Born in other state in the United States: West",
           variable == "B05002_009" ~ "Native: Born outside the United States",
           variable == "B05002_010" ~ "Native: Born outside the United States: Puerto Rico",
           variable == "B05002_011" ~ "Native: Born outside the United States: U.S. Island Areas",
           variable == "B05002_012" ~ "Native: Born outside the United States: Born abroad of American parent(s)",
           variable == "B05002_013" ~ "Foreign born",
           variable == "B05002_014" ~ "Foreign born: Naturalized U.S. citizen",
           variable == "B05002_015" ~ "Foreign born: Naturalized U.S. citizen: Europe",
           variable == "B05002_016" ~ "Foreign born: Naturalized U.S. citizen: Asia",
           variable == "B05002_017" ~ "Foreign born: Naturalized U.S. citizen: Africa",
           variable == "B05002_018" ~ "Foreign born: Naturalized U.S. citizen: Oceania",
           variable == "B05002_019" ~ "Foreign born: Naturalized U.S. citizen: Latin America",
           variable == "B05002_020" ~ "Foreign born: Naturalized U.S. citizen: Northern America",
           variable == "B05002_021" ~ "Foreign born: Not a U.S. citizen",
           variable == "B05002_022" ~ "Foreign born: Not a U.S. citizen: Europe",
           variable == "B05002_023" ~ "Foreign born: Not a U.S. citizen: Asia",
           variable == "B05002_024" ~ "Foreign born: Not a U.S. citizen: Africa",
           variable == "B05002_025" ~ "Foreign born: Not a U.S. citizen: Oceania",
           variable == "B05002_026" ~ "Foreign born: Not a U.S. citizen: Latin America",
           variable == "B05002_027" ~ "Foreign born: Not a U.S. citizen: Northern America"
         )
  )

# Filtering out subcategories for 'Foreign born: naturalized U.S. citizen' and 'Native'

alb_place_of_birth_2022 <- alb_place_of_birth_2022 %>% 
  filter(variable %in% c("B05002_002", "B05002_013", "B05002_014", "B05002_021", "B05002_022", "B05002_023", "B05002_024", "B05002_025", "B05002_026", "B05002_027")) %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B05002_002" ~ "Native",
           variable == "B05002_013" ~ "Foreign born",
           variable == "B05002_014" ~ "Foreign born: Naturalized U.S. citizen",
           variable == "B05002_021" ~ "Foreign born: Not a U.S. citizen",
           variable == "B05002_022" ~ "Foreign born: Not a U.S. citizen: Europe",
           variable == "B05002_023" ~ "Foreign born: Not a U.S. citizen: Asia",
           variable == "B05002_024" ~ "Foreign born: Not a U.S. citizen: Africa",
           variable == "B05002_025" ~ "Foreign born: Not a U.S. citizen: Oceania",
           variable == "B05002_026" ~ "Foreign born: Not a U.S. citizen: Latin America",
           variable == "B05002_027" ~ "Foreign born: Not a U.S. citizen: Northern America"
         )
  )

# Generating CSV:

write.csv(alb_place_of_birth_2022, "data/alb_place_of_birth_2022.csv")

# Table: B16002 (Household Language by Household Limited English Speaking Status)
  # 2022 and 2012 rows do not match (!)

# Charlottesville (Household Language by Household Limited English Speaking Status), 2022

limited_english_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B16002",
  summary_var = "B16002_001",
  survey = "acs5",
  year = 2022) %>% 
    filter(variable %in% c("B16002_002", "B16002_004", "B16002_007", "B16002_010", "B16002_013", "B16002_016", "B16002_019", "B16002_022", "B16002_025", "B16002_028", "B16002_031", "B16002_031", "B16002_037"))

# Finalizing table:

cville_limited_english_2022 <- limited_english_cville_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B16002_002" ~ "English only",
           variable == "B16002_004" ~ "Spanish: Limited English speaking household",
           variable == "B16002_007" ~ "French, Haitian, or Cajun: Limited English speaking household",
           variable == "B16002_010" ~ "German or other West Germanic languages: Limited English speaking household",
           variable == "B16002_013" ~ "Russian, Polish, or other Slavic languages: Limited English speaking household",
           variable == "B16002_016" ~ "Other Indo-European languages: Limited English speaking household",
           variable == "B16002_019" ~ "Korean: Limited English speaking household",
           variable == "B16002_022" ~ "Chinese (incl. Mandarin, Cantonese): Limited English speaking household",
           variable == "B16002_025" ~ "Vietnamese: Limited English speaking household",
           variable == "B16002_028" ~ "Tagalog (incl. Filipino): Limited English speaking household",
           variable == "B16002_031" ~ "Other Asian and Pacific Island languages: Limited English speaking household",
           variable == "B16002_034" ~ "Arabic: Limited English speaking household",
           variable == "B16002_037" ~ "Other and unspecified languages: Limited English speaking household",
         ))

# Generating CSV:

write.csv(cville_limited_english_2022, "data/cville_limited_english_2022.csv")
  
# Albemarle (Household Language by Household Limited English Speaking Status), 2022

limited_english_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B16002",
  summary_var = "B16002_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(variable %in% c("B16002_002", "B16002_004", "B16002_007", "B16002_010", "B16002_013", "B16002_016", "B16002_019", "B16002_022", "B16002_025", "B16002_028", "B16002_031", "B16002_031", "B16002_037"))


# Finalizing table:

alb_limited_english_2022 <- limited_english_alb_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         label = case_when(
           variable == "B16002_002" ~ "English only",
           variable == "B16002_004" ~ "Spanish: Limited English speaking household",
           variable == "B16002_007" ~ "French, Haitian, or Cajun: Limited English speaking household",
           variable == "B16002_010" ~ "German or other West Germanic languages: Limited English speaking household",
           variable == "B16002_013" ~ "Russian, Polish, or other Slavic languages: Limited English speaking household",
           variable == "B16002_016" ~ "Other Indo-European languages: Limited English speaking household",
           variable == "B16002_019" ~ "Korean: Limited English speaking household",
           variable == "B16002_022" ~ "Chinese (incl. Mandarin, Cantonese): Limited English speaking household",
           variable == "B16002_025" ~ "Vietnamese: Limited English speaking household",
           variable == "B16002_028" ~ "Tagalog (incl. Filipino): Limited English speaking household",
           variable == "B16002_031" ~ "Other Asian and Pacific Island languages: Limited English speaking household",
           variable == "B16002_034" ~ "Arabic: Limited English speaking household",
           variable == "B16002_037" ~ "Other and unspecified languages: Limited English speaking household",
         ))

# Generating CSV:

write.csv(alb_limited_english_2022, "data/alb_limited_english_2022.csv")

# Table: B18101 (Sex by Age by Disability Status)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Sex by Age by Disability Status), 2022

disability_status_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B18101",
  summary_var = "B18101_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(!variable %in% c("B18101_001", "B18101_002", "B18101_003", "B18101_006", "B18101_009", "B18101_012", "B18101_015", "B18101_018", "B18101_021", "B18101_022", "B18101_025", "B18101_028", "B18101_031", "B18101_034", "B18101_037")
  )

cville_disability_status_2022 <- disability_status_cville_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         age_sex_category = case_when(
           variable %in% c("B18101_004", "B18101_007") ~ "Male: 17 and under (with a disability)",
           variable %in% c("B18101_005", "B18101_008") ~ "Male: 17 and under (no disability)",
           variable %in% c("B18101_010") ~ "Male: 18 to 34 years (with a disability)",
           variable %in% c("B18101_011") ~ "Male: 18 to 34 years (no disability)",
           variable %in% c("B18101_013") ~ "Male: 35 to 64 years (with a disability)",
           variable %in% c("B18101_014") ~ "Male: 35 to 64 years (no disability)",
           variable %in% c("B18101_016", "B18101_019") ~ "Male: 65 and over (with a disability)",
           variable %in% c("B18101_017", "B18101_020") ~ "Male: 65 and over (no disability)",
           variable %in% c("B18101_023", "B18101_026") ~ "Female: 17 and under (with a disability)",
           variable %in% c("B18101_024", "B18101_027") ~ "Female: 17 and under (no disability)",
           variable %in% c("B18101_029") ~ "Female: 18 to 34 years (with a disability)",
           variable %in% c("B18101_030") ~ "Female: 18 to 34 years (no disability)",
           variable %in% c("B18101_032") ~ "Female: 35 to 64 years (with a disability)",
           variable %in% c("B18101_033") ~ "Female: 35 to 64 years (no disability)",
           variable %in% c("B18101_035", "B18101_038") ~ "Female: 65 and over (with a disability)",
           variable %in% c("B18101_036", "B18101_039") ~ "Female: 65 and over (no disability)",
           TRUE ~ "Other"
         )
  )

# Generating CSV:

write.csv(cville_disability_status_2022, "data/cville_disability_status_2022.csv")

# Albemarle (Sex by Age by Disability Status), 2022

disability_status_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B18101",
  summary_var = "B18101_001",
  survey = "acs5",
  year = 2022) %>% 
  filter(!variable %in% c("B18101_001", "B18101_002", "B18101_003", "B18101_006", "B18101_009", "B18101_012", "B18101_015", "B18101_018", "B18101_021", "B18101_022", "B18101_025", "B18101_028", "B18101_031", "B18101_034", "B18101_037"))


alb_disability_status_2022 <- disability_status_alb_2022 %>% 
  mutate(percent = 100 * (estimate / summary_est),
         year = "2022",
         age_sex_category = case_when(
           variable %in% c("B18101_004", "B18101_007") ~ "Male: 17 and under (with a disability)",
           variable %in% c("B18101_005", "B18101_008") ~ "Male: 17 and under (no disability)",
           variable %in% c("B18101_010") ~ "Male: 18 to 34 years (with a disability)",
           variable %in% c("B18101_011") ~ "Male: 18 to 34 years (no disability)",
           variable %in% c("B18101_013") ~ "Male: 35 to 64 years (with a disability)",
           variable %in% c("B18101_014") ~ "Male: 35 to 64 years (no disability)",
           variable %in% c("B18101_016", "B18101_019") ~ "Male: 65 and over (with a disability)",
           variable %in% c("B18101_017", "B18101_020") ~ "Male: 65 and over (no disability)",
           variable %in% c("B18101_023", "B18101_026") ~ "Female: 17 and under (with a disability)",
           variable %in% c("B18101_024", "B18101_027") ~ "Female: 17 and under (no disability)",
           variable %in% c("B18101_029") ~ "Female: 18 to 34 years (with a disability)",
           variable %in% c("B18101_030") ~ "Female: 18 to 34 years (no disability)",
           variable %in% c("B18101_032") ~ "Female: 35 to 64 years (with a disability)",
           variable %in% c("B18101_033") ~ "Female: 35 to 64 years (no disability)",
           variable %in% c("B18101_035", "B18101_038") ~ "Female: 65 and over (with a disability)",
           variable %in% c("B18101_036", "B18101_039") ~ "Female: 65 and over (no disability)",
           TRUE ~ "Other"
         )
  )

# Generating CSV:

write.csv(alb_disability_status_2022, "data/alb_disability_status_2022.csv")

# Table: B18102 (Sex by Age by Hearing Difficulty)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Sex by Age by Hearing Difficulty: Total), 2022

total_hearing_difficulty_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B18102",
  summary_var = "B18102_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18102_004", "B18102_007", "B18102_010", "B18102_013", "B18102_016", "B18102_019", "B18102_023", "B18102_026", "B18102_029", "B18102_032", "B18102_035", "B18102_038")) %>%
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Hearing Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Albemarle (Sex by Age by Hearing Difficulty: Total), 2022

total_hearing_difficulty_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B18102",
  summary_var = "B18102_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18102_004", "B18102_007", "B18102_010", "B18102_013", "B18102_016", "B18102_019", "B18102_023", "B18102_026", "B18102_029", "B18102_032", "B18102_035", "B18102_038")) %>%
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Hearing Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Table: B18103 (Sex by Age by Vision Difficulty)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Sex by Age by Vision Difficulty: Total), 2022

total_vision_difficulty_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B18103",
  summary_var = "B18103_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18103_004", "B18103_007", "B18103_010", "B18103_013", "B18103_016", "B18103_019", "B18103_023", "B18103_026", "B18103_029", "B18103_032", "B18103_035", "B18103_038")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Vision Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Albemarle (Sex by Age by Vision Difficulty: Total), 2022

total_vision_difficulty_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B18103",
  summary_var = "B18103_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18103_004", "B18103_007", "B18103_010", "B18103_013", "B18103_016", "B18103_019", "B18103_023", "B18103_026", "B18103_029", "B18103_032", "B18103_035", "B18103_038")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Vision Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Table: B18104 (Sex by Age by Cognitive Difficulty)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Sex by Age by Cognitive Difficulty: Total), 2022

total_cognitive_difficulty_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B18104",
  summary_var = "B18104_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18104_004", "B18104_007", "B18104_010", "B18104_013", "B18104_016", "B18104_020", "B18104_023", "B18104_026", "B18104_029", "B18104_032")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Cognitive Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Albemarle (Sex by Age by Cognitive Difficulty), 2022

total_cognitive_difficulty_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B18104",
  summary_var = "B18104_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18104_004", "B18104_007", "B18104_010", "B18104_013", "B18104_016", "B18104_020", "B18104_023", "B18104_026", "B18104_029", "B18104_032")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Cognitive Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Table: B18105 (Sex by Age by Ambulatory Difficulty)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Sex by Age by Ambulatory Difficulty), 2022

total_ambulatory_difficulty_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B18105",
  summary_var = "B18105_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18105_004", "B18105_007", "B18105_010", "B18105_013", "B18105_016", "B18105_020", "B18105_023", "B18105_026", "B18105_029", "B18105_032")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Ambulatory Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Albemarle (Sex by Age by Ambulatory Difficulty), 2022

total_ambulatory_difficulty_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B18105",
  summary_var = "B18105_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18105_004", "B18105_007", "B18105_010", "B18105_013", "B18105_016", "B18105_020", "B18105_023", "B18105_026", "B18105_029", "B18105_032")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Ambulatory Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Table: B18106 (Sex by Age by Self-care Difficulty)
  # Rows match up for 2012 and 2022 (!)

# Charlottesville (Sex by Age by Self-care Difficulty), 2022

total_self_care_difficulty_cville_2022 <- get_acs(
  geography = "county",
  county = "540",
  cache = TRUE,
  state = "VA",
  table = "B18106",
  summary_var = "B18106_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18106_004", "B18106_007", "B18106_010", "B18106_013", "B18106_016", "B18106_020", "B18106_023", "B18106_026", "B18106_029", "B18106_032")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Self Care Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Albemarle (Sex by Age by Self-care Difficulty), 2022

total_self_care_difficulty_alb_2022 <- get_acs(
  geography = "county",
  county = "003",
  cache = TRUE,
  state = "VA",
  table = "B18106",
  summary_var = "B18106_001",
  survey = "acs5",
  year = 2022
) %>%
  filter(variable %in% c("B18106_004", "B18106_007", "B18106_010", "B18106_013", "B18106_016", "B18106_020", "B18106_023", "B18106_026", "B18106_029", "B18106_032")) %>% 
  summarise(
    GEOID = first(GEOID),
    NAME = first(NAME),
    label = "Total Self Care Difficulty",
    estimate = sum(estimate),
    summary_est = first(summary_est),
    Year = "2022"
  )

# Binding All Disability Total Tables Together (Charlottesville)

cville_combined_disability_total_2022 <- rbind(total_hearing_difficulty_cville_2022, total_vision_difficulty_cville_2022, total_cognitive_difficulty_cville_2022, total_ambulatory_difficulty_cville_2022, total_self_care_difficulty_cville_2022)

  # Generating CSV:

write.csv(cville_combined_disability_total_2022, "data/cville_combined_disability_total_2022.csv")

# Binding All Disability Total Tables Together (Albemarle)

alb_combined_disability_total_2022 <- rbind(total_hearing_difficulty_alb_2022, total_vision_difficulty_alb_2022, total_cognitive_difficulty_alb_2022, total_ambulatory_difficulty_alb_2022, total_self_care_difficulty_alb_2022)

  # Generating CSV:

write.csv(alb_combined_disability_total_2022, "data/alb_combined_disability_total_2022.csv")
