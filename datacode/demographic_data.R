# R Script for pulling and examining demographic data
# Author: Henry DeMarco
# Date Created: June 4, 2024
# Last Updated: June 4, 2024

## County FIPS Codes
# 003 -- Albemarle
# 540 -- Charlottesville

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

# Variables of interest
## - Estimate Total: White alone -- B02001_002
## - Estimate Total: Black or African American alone -- B02001_003

############################

# Table: B02001 (Race)

# Table: 


