# Area health resources file

library(tidyverse)
# url <- "https://data.hrsa.gov/DataDownload/DD_Files/AHRF_CSV_2022-2023.zip"
ahrf <- read_csv("~/Downloads/AHRF_CSV_2022-2023/DATA/ahrf2023.csv")
local <- ahrf %>% 
  filter(fips_st_cnty %in% c("51003", "51540"))
# so many variables!
