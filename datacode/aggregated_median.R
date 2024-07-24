# Generating median household income
# aggregated across geographies
# 2024-03-31
# mpc

# A good explanation of finding the median across aggregated geographies
# https://dof.ca.gov/wp-content/uploads/sites/352/Forecasting/Demographics/Documents/How_to_Recalculate_a_Median.pdf


# .................
# setup ----
library(tidyverse)
library(tidycensus)


# set localities
# starting with: Albemarle, Charlottesville, Fluvanna, Greene, Louisa, Nelson
loc <- c("003", "540", "065", "079", "109", "125")


# .................
# get table ----
# Table B19001 (A-H)
income_ranges_table <- get_acs(geography = "county",
                         state = "51",
                         county = loc,
                         table = "B19001", 
                         summary_var = "B19001_001",
                         year = 2022,
                         survey = "acs5",
                         cache_table = TRUE) 

# .................
# prep data ----
income_ranges <- income_ranges_table %>% 
  filter(variable != "B19001_001") %>%
  mutate(income_bin = as.factor(variable)) %>%
  mutate(income_bin = fct_recode(income_bin,
    "2500_9999" = "B19001_002",
    "10000_14999" = "B19001_003",
    "15000_19999" = "B19001_004",
    "20000_24999" = "B19001_005",
    "25000_29999" = "B19001_006",
    "30000_34999" = "B19001_007",
    "35000_39999" = "B19001_008",
    "40000_44999" = "B19001_009",
    "45000_49999" = "B19001_010",
    "50000_59999" = "B19001_011",
    "60000_74999" = "B19001_012",
    "75000_99999" = "B19001_013",
    "100000_124999" = "B19001_014",
    "125000_149999" = "B19001_015",
    "150000_199999" = "B19001_016",
    "200000_300000" = "B19001_017" 
  )) %>% 
  separate(income_bin, into = c("bin_start", "bin_end"), 
           sep = "_", remove = FALSE) %>% 
  mutate(across(starts_with("bin"), as.numeric))


# .................
# Derive steps to aggregate and estimate ----

# choose localities
choose <- c("51003", "51540")

# generate aggregate sums
aggregate_range <- income_ranges %>% 
  filter(GEOID %in% choose) %>% 
  group_by(income_bin, bin_start, bin_end) %>% 
  summarize(estimate = sum(estimate),
            total = sum(summary_est)) %>% 
  ungroup() %>% 
  mutate(cum_sum = cumsum(estimate),
         cum_per = cum_sum/total)

# identify midpoint observation
midpoint <- aggregate_range$total[1]/2

# find index of row for which cum_sum contains the midpoint
index_bin <- which(aggregate_range$cum_sum > midpoint)[1]

# range_reach = midpoint - cum_sum[x-1]
#   how many observations into the identified cum_sum do we need to reach
range_reach <- midpoint-aggregate_range$cum_sum[index_bin-1]

# range_prop = range_reach / estimate[x]
#   what proportion of the total of the identified income_bin is this 
range_prop <- range_reach / aggregate_range$estimate[index_bin]

# income_add = range_prop * (bin_end[x] - bin_start[x] + 1) 
#   assuming uniform distribution in the range, how far into the income bin is this proportion
income_add <- range_prop * (aggregate_range$bin_end[index_bin] + 1 - aggregate_range$bin_start[index_bin])

# median = bin_end[x-1] + income_add 
#   add this income to the top range of the prior income bin
median <- aggregate_range$bin_end[index_bin-1] + income_add

# Compare to county estimates from B19013
# Albemarle: estimate 97564, table 97708
# Charlottesville: estimate 67489, table 67177
# Fluvanna: estimate 90413, table 90766
# Greene: estimate 81952, table 81338
# Louisa: estimate 76152, table 76594
# Nelson: estimate 66179, table 64028


# .................
# get all tables ----
# get all race-ethnicity specific tables
tables <- c("B19001", "B19001A", "B19001B", "B19001C", "B19001D", "B19001E",
            "B19001F", "B19001G", "B19001H", "B19001I")

income_tables_all <- map(tables,
                       ~get_acs(geography = "county",
                                state = "51",
                                county = loc,
                                table = .x, 
                                #summary_var = "B19001_001",
                                year = 2022,
                                survey = "acs5",
                                cache_table = TRUE) %>% 
                         mutate(table = .x)
)


# .................
# prep all data ----
# perform processing across list
income_tables <- income_tables_all %>% 
  map(~filter(., !(str_detect(variable, fixed("_001")))) %>% 
        mutate(variable = str_replace(variable, "B19001[:upper:]", "B19001"),
               income_bin = as.factor(variable),
               income_bin = fct_recode(income_bin,
                                       "2500_9999" = "B19001_002",
                                       "10000_14999" = "B19001_003",
                                       "15000_19999" = "B19001_004",
                                       "20000_24999" = "B19001_005",
                                       "25000_29999" = "B19001_006",
                                       "30000_34999" = "B19001_007",
                                       "35000_39999" = "B19001_008",
                                       "40000_44999" = "B19001_009",
                                       "45000_49999" = "B19001_010",
                                       "50000_59999" = "B19001_011",
                                       "60000_74999" = "B19001_012",
                                       "75000_99999" = "B19001_013",
                                       "100000_124999" = "B19001_014",
                                       "125000_149999" = "B19001_015",
                                       "150000_199999" = "B19001_016",
                                       "200000_300000" = "B19001_017")
               ) %>% 
        separate(income_bin, into = c("bin_start", "bin_end"), 
                 sep = "_", remove = FALSE) %>% 
        mutate(across(starts_with("bin"), as.numeric)) %>% 
        group_by(GEOID, NAME) %>% 
        mutate(summary_est = sum(estimate)) %>% 
        ungroup()
)

 
# .................
# Make a function ----

aggregate_median <- function(df, loc){
  aggregate_range <- df %>% 
    filter(GEOID %in% loc) %>% 
    group_by(income_bin, bin_start, bin_end) %>% 
    summarize(estimate = sum(estimate),
              total = sum(summary_est)) %>% 
    ungroup() %>% 
    mutate(cum_sum = cumsum(estimate),
           cum_per = cum_sum/total)
  
  midpoint <- aggregate_range$total[1]/2
  index_bin <- which(aggregate_range$cum_sum > midpoint)[1]
  range_reach <- midpoint-aggregate_range$cum_sum[index_bin-1]
  range_prop <- range_reach / aggregate_range$estimate[index_bin]
  income_add <- range_prop * (aggregate_range$bin_end[index_bin] + 1 - aggregate_range$bin_start[index_bin])
  median <- aggregate_range$bin_end[index_bin-1] + income_add
  return(median)
}

# .................
# Notes ----
# we could almost certainly make this more elegant, 
# but in the interest of time, a workable function feels sufficient


# .................
# apply function ---- 
# define vector of fips 
choose <- c("51003", "51540")

median_overall <- aggregate_median(income_tables[[1]], choose)
median_whitealone <- aggregate_median(income_tables[[2]], choose)
median_blackalone <- aggregate_median(income_tables[[3]], choose)
median_aianalone <- aggregate_median(income_tables[[4]], choose)
median_asianalone <- aggregate_median(income_tables[[5]], choose)
median_nhpialone <- aggregate_median(income_tables[[6]], choose)
median_otheralone <- aggregate_median(income_tables[[7]], choose)
median_multialone <- aggregate_median(income_tables[[8]], choose)
median_whitenhalone <- aggregate_median(income_tables[[9]], choose)
median_hispanic <- aggregate_median(income_tables[[10]], choose)

# why no nhpi?
income_tables[[6]] %>% view()

# combine into data frame?
medians_bygroup <- data.frame(mget(ls(pattern="median_"))) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "median_hh_inc") %>% 
  mutate(region = paste(choose, collapse = ","))

# or, adjust function to return median and table name
# and apply/map function across list of income tables...?

# .................
# Notes ----
# or separate each of these into a global environment, to call data frame name...
# https://stackoverflow.com/questions/59169631/split-a-list-into-separate-data-frame-in-r

# could also work on adding moe...
