# Employment/Commuting
# Inflow/outflow from LEHD/LODES
# Among those residing in state only


library(tidyverse)
library(tigris)
library(sf)
library(viridis)


# Pull County shapefiles ----
# identify urban ring
## albemarle county comprehensive plan boundaries
# url <- "https://gisweb.albemarle.org/gisdata/CompPlan/Comp_Plan_Areas.zip?_gl=1*17rtvda*_ga*NTQ4OTg3MjIwLjE3MzYwMTUxODk.*_ga_3P48R6G8ZB*MTczODA5NTg4OS4xMS4xLjE3MzgwOTU5MDkuNDAuMC4xNDAwODcwMjg4"
# download.file(url, destfile = "downloads/alb_comp_boundary.zip")
alb_comp_boundary <- st_read("downloads/alb_comp_boundary/COMP_PLAN_AREAS.shp")   

ggplot(alb_comp_boundary) +
  geom_sf()

alb_comp_boundary %>% 
  filter(str_detect(Name, "Neighborhood|Places")) %>% 
  ggplot() +
  geom_sf()

# Tigris blocks ----
# identify blocks in urban ring
alb_blocks <- blocks(state = "51", county = "003", year = 2020)

alb_dev <- alb_comp_boundary %>% 
  filter(str_detect(Name, "Neighborhood|Places")) %>% 
  st_transform(crs = st_crs(alb_blocks))

ggplot(alb_dev) +
  geom_sf()

# Identify blocks within urban ring
dev_blocks = st_intersection(alb_blocks, alb_dev)

ggplot(dev_blocks) +
  geom_sf()


# Read OD data ----

# Low-income/LODES ----
# https://lehd.ces.census.gov/data/
# https://lehd.ces.census.gov/data/lodes/LODES8/va/
# od: va_od_main_JT01_2022.csv.gz

# url <- "https://lehd.ces.census.gov/data/lodes/LODES8/va/od/va_od_main_JT01_2022.csv.gz"
# download.file(url, destfile = "downloads/od_main_2022.csv.gz")
od_2022 <- read_csv(gzfile("downloads/od_main_2022.csv.gz"))   

# url <- "https://lehd.ces.census.gov/data/lodes/LODES8/va/od/va_od_main_JT01_2012.csv.gz"
# download.file(url, destfile = "downloads/od_main_2012.csv.gz")
od_2012 <- read_csv(gzfile("downloads/od_main_2012.csv.gz"))   

# url <- "https://lehd.ces.census.gov/data/lodes/LODES8/va/od/va_od_main_JT01_2002.csv.gz"
# download.file(url, destfile = "downloads/od_main_2002.csv.gz")
od_2002 <- read_csv(gzfile("downloads/od_main_2002.csv.gz"))   


# Derive ----
od_region_2022 <- od_2022 %>% 
  mutate(h_geocode = as.character(h_geocode),
         w_geocode = as.character(w_geocode),
         h_county = str_sub(h_geocode,1,5),
         w_county = str_sub(w_geocode, 1, 5),
         year = 2022) %>% 
  mutate(live_region = ifelse(h_county %in% c("51003", "51540"), "yes", "no"),
         work_region = ifelse(w_county %in% c("51003", "51540"), "yes", "no")) %>% 
  filter(live_region == "yes" | work_region == "yes")
 
od_region_2012 <- od_2012 %>% 
  mutate(h_geocode = as.character(h_geocode),
         w_geocode = as.character(w_geocode),
         h_county = str_sub(h_geocode,1,5),
         w_county = str_sub(w_geocode, 1, 5),
         year = 2012) %>% 
  mutate(live_region = ifelse(h_county %in% c("51003", "51540"), "yes", "no"),
         work_region = ifelse(w_county %in% c("51003", "51540"), "yes", "no")) %>% 
  filter(live_region == "yes" | work_region == "yes")

od_region_2002 <- od_2002 %>% 
  mutate(h_geocode = as.character(h_geocode),
         w_geocode = as.character(w_geocode),
         h_county = str_sub(h_geocode,1,5),
         w_county = str_sub(w_geocode, 1, 5),
         year = 2002) %>% 
  mutate(live_region = ifelse(h_county %in% c("51003", "51540"), "yes", "no"),
         work_region = ifelse(w_county %in% c("51003", "51540"), "yes", "no")) %>% 
  filter(live_region == "yes" | work_region == "yes")

## Cville/Alb versus outside ----
# Separately by age (SA01, 02, 03)
# by wage (SE01, 02, 03)
od_sums_2022 <- od_region_2022 %>% 
  mutate(status = case_when(
    live_region == "yes" & work_region == "yes" ~  "live_here_work_here",
    live_region == "no" & work_region == "yes" ~ "live_outside_work_here",
    live_region == "yes" & work_region == "no" ~ "live_here_work_outside"
  )) %>% 
  group_by(status) %>% 
  summarize(year = first(year), 
            workers = sum(S000),
            younger = sum(SA01),
            middle_age = sum(SA02),
            older = sum(SA03),
            low_wage = sum(SE01),
            mid_wage = sum(SE02),
            hi_wage = sum(SE03)) %>% 
  pivot_longer(cols = -c(status, year), names_to = "worker_type", values_to = "number")

od_sums_2012 <- od_region_2012 %>% 
  mutate(status = case_when(
    live_region == "yes" & work_region == "yes" ~  "live_here_work_here",
    live_region == "no" & work_region == "yes" ~ "live_outside_work_here",
    live_region == "yes" & work_region == "no" ~ "live_here_work_outside"
  )) %>% 
  group_by(status) %>% 
  summarize(year = first(year), 
            workers = sum(S000),
            younger = sum(SA01),
            middle_age = sum(SA02),
            older = sum(SA03),
            low_wage = sum(SE01),
            mid_wage = sum(SE02),
            hi_wage = sum(SE03)) %>% 
  pivot_longer(cols = -c(status, year), names_to = "worker_type", values_to = "number")

od_sums_2002 <- od_region_2002 %>% 
  mutate(status = case_when(
    live_region == "yes" & work_region == "yes" ~  "live_here_work_here",
    live_region == "no" & work_region == "yes" ~ "live_outside_work_here",
    live_region == "yes" & work_region == "no" ~ "live_here_work_outside"
  )) %>% 
  group_by(status) %>% 
  summarize(year = first(year), 
            workers = sum(S000),
            younger = sum(SA01),
            middle_age = sum(SA02),
            older = sum(SA03),
            low_wage = sum(SE01),
            mid_wage = sum(SE02),
            hi_wage = sum(SE03)) %>% 
  pivot_longer(cols = -c(status, year), names_to = "worker_type", values_to = "number")


## Combine and save ----
od_region <- bind_rows(od_sums_2022, od_sums_2012, od_sums_2002)

write_csv(od_region, "data/inflow_outflow_region.csv")
# od_region <- read_csv("data/inflow_outflow_region.csv")


## Cvl, Alb Urban, Alb Rural, Not ----
od_3sums_2022 <- od_region_2022 %>% 
  mutate(
    live = case_when(
      h_county == "51540" | h_geocode %in% dev_blocks$GEOID20 ~ "urban",
      h_county == "51003" & !(h_geocode %in% dev_blocks$GEOID20) ~ "rural",
      !(h_county %in% c("51003", "51540")) ~ "outside"),
    work = case_when(
      w_county == "51540" | w_geocode %in% dev_blocks$GEOID20 ~ "urban",
      w_county == "51003" & !(w_geocode %in% dev_blocks$GEOID20) ~ "rural",
      !(w_county %in% c("51003", "51540")) ~ "outside")) %>% 
  group_by(live, work) %>% 
  summarize(year = first(year), 
            workers = sum(S000)) %>% 
  ungroup() %>% 
  mutate(percent_all = workers/sum(workers)) %>% 
  group_by(work) %>% 
  mutate(percent_work = workers/sum(workers)) %>% 
  ungroup() %>% 
  group_by(live) %>% 
  mutate(percent_live = workers/sum(workers)) %>% 
  ungroup()
 
od_3sums_2012 <- od_region_2012 %>% 
  mutate(
    live = case_when(
      h_county == "51540" | h_geocode %in% dev_blocks$GEOID20 ~ "urban",
      h_county == "51003" & !(h_geocode %in% dev_blocks$GEOID20) ~ "rural",
      !(h_county %in% c("51003", "51540")) ~ "outside"),
    work = case_when(
      w_county == "51540" | w_geocode %in% dev_blocks$GEOID20 ~ "urban",
      w_county == "51003" & !(w_geocode %in% dev_blocks$GEOID20) ~ "rural",
      !(w_county %in% c("51003", "51540")) ~ "outside")) %>% 
  group_by(live, work) %>% 
  summarize(year = first(year), 
            workers = sum(S000)) %>% 
  ungroup() %>% 
  mutate(percent_all = workers/sum(workers)) %>% 
  group_by(work) %>% 
  mutate(percent_work = workers/sum(workers)) %>% 
  ungroup() %>% 
  group_by(live) %>% 
  mutate(percent_live = workers/sum(workers)) %>% 
  ungroup()

od_3sums_2002 <- od_region_2002 %>% 
  mutate(
    live = case_when(
      h_county == "51540" | h_geocode %in% dev_blocks$GEOID20 ~ "urban",
      h_county == "51003" & !(h_geocode %in% dev_blocks$GEOID20) ~ "rural",
      !(h_county %in% c("51003", "51540")) ~ "outside"),
    work = case_when(
      w_county == "51540" | w_geocode %in% dev_blocks$GEOID20 ~ "urban",
      w_county == "51003" & !(w_geocode %in% dev_blocks$GEOID20) ~ "rural",
      !(w_county %in% c("51003", "51540")) ~ "outside")) %>% 
  group_by(live, work) %>% 
  summarize(year = first(year), 
            workers = sum(S000)) %>% 
  ungroup() %>% 
  mutate(percent_all = workers/sum(workers)) %>% 
  group_by(work) %>% 
  mutate(percent_work = workers/sum(workers)) %>% 
  ungroup() %>% 
  group_by(live) %>% 
  mutate(percent_live = workers/sum(workers)) %>% 
  ungroup()

## Combine and save ----
od_three_region <- bind_rows(od_3sums_2022, od_3sums_2012, od_3sums_2002)

write_csv(od_three_region, "data/inflow_outflow_threeregion.csv")
# od_three_region <- read_csv("data/inflow_outflow_threeregion.csv")

ggplot(od_three_region, aes(x = live, y = work)) +
  geom_tile(aes(fill = percent_work),
            color = "white", size = 0.1) + 
  geom_text(aes(label = round(percent_work, 2)),
            color = "white") +
  scale_fill_viridis() + 
  facet_wrap(~year)
