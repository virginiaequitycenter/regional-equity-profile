library(tidyverse)
library(data.table)

# https://www.census.gov/data/datasets/time-series/demo/cps/cps-supp_cps-repwgt/cps-volunteer.2023.html#list-tab-1048706686
url <- "https://www2.census.gov/programs-surveys/cps/datasets/2023/supp/sep23pub.csv"
# vcl <- read_csv(url)
vcl = fread(url, header = TRUE)

vcl %>% count(GTCBSA) %>% view()
# 16820

vcl %>% 
  filter(GTCBSA == 16820) %>% 
  count(PES1)
# mostly not in supplement

vcl %>% 
  filter(GESTFIPS == 51) %>% 
  count(PES1, wt = PWSRWGT) # still unclear which weights
