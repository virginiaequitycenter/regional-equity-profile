# From https://www.tandfonline.com/doi/full/10.1080/10511482.2021.1981976

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

# libraries ----
# if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(ipumsr)
library(srvyr)
library(dplyr)
library(tidyverse)
library(robsurvey)

# Using 5 year ACS instead
# read in the data ----
# downloaded from IPUMS and check that the variables and number of observations are as expected
# data <- read.csv("~/Downloads/usa_00102.csv",  blank.lines.skip = TRUE)
ddi <- read_ipums_ddi("downloads/usa_00006.xml")
data <- read_ipums_micro(ddi)

# unique ids ----
# unite the STATEFIP with PUMA to create unique IDs 
data$STATEFIP <- as.character(data$STATEFIP)
data$PUMA <- as.character(data$PUMA)
data <- data %>% unite("GEOID", c(STATEFIP,PUMA), remove = FALSE, sep = "") 

# apply hh weights ----
# use the robsurvey pacakge to apply the household weights to the variables. 
# This is to create the median rent index to adjust the poverty variables
area_data <- data %>%
  dplyr::filter(OWNERSHP == 2, BEDROOMS == 3, KITCHEN == 4, 
                PLUMBING == 20, PERNUM == 1) %>% 
  group_by(GEOID, YEAR) %>% # added year
  summarise(AMGR = weighted_median(RENTGRS, HHWT))

# join ----
# join the original dataset with the area_data frame that only has GEOID and the area median gross rent.
data1 <- left_join(data, area_data,  by = c("GEOID", "YEAR"))

# derive variables ----
# construct new variables needed for the doubling up measure. 
# In this sample code, 1086 represents the national median gross rent for 2019 for a 2 bedroom; 
# this should be updated for future or past years (Use Census TableID: B25031)
# 2018-2022: 1255 

data2 <- data1 %>% 
  mutate(
    adjustment = case_when(
      OWNERSHP == 1 & MORTGAGE == 1 ~ .402*(AMGR/1255) + .598,
      OWNERSHP == 1 & MORTGAGE == 2 | MORTGAGE == 3 | MORTGAGE == 4 ~ .504*(AMGR/1255) + .496,
      OWNERSHP == 2 & MORTGAGE == 0 ~ .514*(AMGR/1255) + .486,
      TRUE ~ NA
    ),
    
    # adjustment = ifelse(OWNERSHP == 1 & MORTGAGE == 1, .402*(AMGR/1086) + .598,
    #        ifelse(OWNERSHP == 1 & MORTGAGE == 2 | MORTGAGE == 3 | MORTGAGE == 4, .504*(AMGR/1086) +.496,
    #               ifelse(OWNERSHP == 2 & MORTGAGE == 0, .514*(AMGR/1086) + .486, NA))),
    
    adjustedpoverty_head = POVERTY_HEAD*(1/adjustment), 
    adjustedpoverty = POVERTY*(1/adjustment), 
    roundedadjustedpoverty = round(adjustedpoverty, 0),
    roundedadjustedpoverty_head = round(adjustedpoverty_head,0),
    overcrowded = ifelse((NUMPREC/2)>(BEDROOMS-1), 1, 0),
    DUrelative = ifelse(RELATE  %in% c(5,6,7,8,10)  & AGE < 65, 1,0),
    special1 = ifelse(RELATE == 7 & AGE < 18 & MOMLOC== 0 & POPLOC == 0, 1, 0),
    special2 = ifelse(RELATE == 7 & AGE > 17 & MOMLOC== 0 & POPLOC == 0 & MOMLOC_HEAD == 0  & 
                        POPLOC_HEAD == 0 & SPLOC == 0 & SPLOC_HEAD == 0 & 
                        NCHILD == 0 & NCHILD_HEAD == 0, 1, 0),
    DUrelative = ifelse(special1 == 1 | special2 == 1, 0, DUrelative),
    DUolderrelative = ifelse(RELATE  %in% c(5,6,7,8,10) & AGE >= 65 & overcrowded == 1, 1,0),
    DUmiddlegen = ifelse(RELATE %in% c(3,4) & SFTYPE %in% c(3,4) & AGE >=18, 1,0),
    DUmarriedchild = ifelse(RELATE %in% c(3,4) & SFTYPE %in% c(1,2), 1,0),
    DUgrandchild = ifelse(RELATE !=  9, 0,
                          ifelse(RELATE == 9 & GCRESPON_HEAD == 2 & AGE < 18, 0, 
                                 ifelse(RELATE == 9 & SFTYPE == 4  & AGE_MOM<18, 0,
                                        ifelse(RELATE == 9 & SFTYPE == 3 &  AGE_POP<18, 0, 1)))),
    DUsingadcrowd = ifelse(RELATE %in% c(3,4) & SFTYPE == 0 & AGE >17 & overcrowded == 1, 1, 0),
    DUnonrelative = ifelse(RELATED == 1260, 1, 0),
    special6 = ifelse(RELATED == 1260 & (RELATED_MOM %in% 1114 | RELATED_POP %in% 1114) & 
                        (AGE<18 | (AGE>=18 & overcrowded==0)),1,0), 
    DUnonrelative = ifelse(special6 == 1, 0, DUnonrelative),
    povertylev = ifelse(roundedadjustedpoverty_head <= 125 & roundedadjustedpoverty <= 125, 1, 0),
    DU1 = ifelse( povertylev == 1 & DUmiddlegen == 1, 1, 0),
    DU2 = ifelse( povertylev == 1 & DUrelative == 1, 1, 0),
    DU3 = ifelse( povertylev == 1 & DUgrandchild == 1, 1, 0),
    DU4 = ifelse( povertylev == 1 & DUmarriedchild == 1, 1, 0),
    DU5 = ifelse( povertylev == 1 & DUnonrelative == 1, 1, 0),
    DU6 = ifelse( povertylev == 1 & DUsingadcrowd == 1, 1, 0),
    DU7 = ifelse( povertylev == 1 & DUolderrelative == 1, 1, 0),
    doubledup = ifelse(DU1 == 1 | DU2 == 1 | DU3 == 1 |DU4 == 1 | DU5 == 1 |
                         DU6 == 1 | DU7 == 1, 1,0 ))

# apply person weights ----
# enter the survey design information to compute weighted frequencies and 
# standard errors using ACS person weights (PERWT) and Replicate Weights (REPWTP).

person_weighted <- data2 %>% 
  as_survey_design(weights = PERWT, #repweights = matches("REPWTP[0-9]+"), 
                   type = "JK1", scale = 4/80, #rscales = rep(1, 80 ), mse = TRUE
  )

va_sum <- person_weighted %>% 
  summarise(survey_total(doubledup))
# article shows 69 per 100K for VA in 2019; this shows ~ 51
# with initial adjustment, it shows ~ 56

va_geoid <- person_weighted %>% 
  group_by(GEOID) %>% 
  summarise(survey_total(doubledup))

# Cvl/Alb pumas
# 2012-2021: (51)51089, (51)51090
# 2022-present: 5110901, 5154001

region_sum <- va_geoid %>% 
  filter(GEOID %in% c("5151089", "5151090", "5110901", "5154001")) %>% 
  summarize(estimate = sum(coef))
# 1826

# Save results ----
write_csv(va_geoid, "data/doubledup_5yracs_vapumas.csv")
write_csv(region_sum, "data/doubledup_5yracs_region.csv")
