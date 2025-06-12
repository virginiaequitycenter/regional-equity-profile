# Child care facilities/seats
# https://vachildcare.com/data-2/child-care-supply-data-by-locality/

library(tidyverse)
# install.packages("rJava")
# remotes::install_github(c("ropensci/tabulapdf"))
library(tabulapdf)
library(tidycensus)

# Albemarle ----
alb <- "https://stage.worklifesystems.com/virginia?county=Albemarle"

# pdfarea_centers <- locate_areas(alb, 1)
pdfarea_centers <- list(setNames(c(238, 26, 335, 560), 
                         c("top", "left", "bottom", "right")))
alb_centers <- extract_tables(alb, pages = 1, area = pdfarea_centers, 
                       guess = FALSE, output = "tibble")
# alb_centers[[1]]
alb_centers_df <- alb_centers[[1]][2:6,]
names(alb_centers_df) <- "type"
alb_centers_df <- alb_centers_df %>% 
  mutate(centers = str_extract(type, "[0-9]+"),
         centers = as.numeric(centers),
         type = str_remove(type, "[0-9]+"),
         type = str_trim(type),
         type = if_else(type == "Total Child Care Programs", "Total", type))
  
# pdfarea_seats <- locate_areas(alb, 1)
pdfarea_seats <- list(setNames(c(608, 24, 698, 530), 
                                     c("top", "left", "bottom", "right")))
alb_seats <- extract_tables(alb, pages = 1, area = pdfarea_seats, 
                              guess = FALSE, output = "tibble")
alb_seats_df <- alb_seats[[1]]
names(alb_seats_df) <- c("type", "seats")
alb_seats_df <- alb_seats_df %>% 
  mutate(type = if_else(type == "Centers", "Child Care Centers", type),
         type = if_else(type == "Total Licensed Capacity", "Total", type))

alb_data <- alb_centers_df %>% 
  left_join(alb_seats_df) %>% 
  mutate(location = "Albemarle")

# Charlottesville ----
cvl <- "https://stage.worklifesystems.com/virginia?city=Charlottesville"

cvl_centers <- extract_tables(cvl, pages = 1, area = pdfarea_centers, 
                              guess = FALSE, output = "tibble")
# cvl_centers[[1]]
cvl_centers_df <- cvl_centers[[1]][2:6,]
names(cvl_centers_df) <- "type"
cvl_centers_df <- cvl_centers_df %>% 
  mutate(centers = str_extract(type, "[0-9]+"),
         centers = as.numeric(centers),
         type = str_remove(type, "[0-9]+"),
         type = str_trim(type),
         type = if_else(type == "Total Child Care Programs", "Total", type))

cvl_seats <- extract_tables(cvl, pages = 1, area = pdfarea_seats, 
                            guess = FALSE, output = "tibble")
cvl_seats_df <- cvl_seats[[1]]
names(cvl_seats_df) <- c("type", "seats")
cvl_seats_df <- cvl_seats_df %>% 
  mutate(type = if_else(type == "Centers", "Child Care Centers", type),
         type = if_else(type == "Total Licensed Capacity", "Total", type))

cvl_data <- cvl_centers_df %>% 
  left_join(cvl_seats_df) %>% 
  mutate(location = "Charlottesville")

# Combine ----
child_care <- bind_rows(alb_data, cvl_data)

child_care %>% 
  filter(type != "Total") %>% 
  ggplot(aes(x = location, y = centers, fill = type)) +
  geom_col(position = "dodge")

child_care %>% 
  filter(type != "Total") %>% 
  ggplot(aes(x = location, y = seats, fill = type)) +
  geom_col(position = "dodge")

# regional ---- 
child_care_region <- child_care %>% 
  group_by(type) %>% 
  summarize(centers = sum(centers),
            seats = sum(seats))

# Children ----
# under 6 with working parents
chvar <- c(parentswork = "B23008_004", singledadworks = "B23008_010", singlemomworks = "B23008_013")
ch <- get_acs(geography = "county",
              variables = chvar,
              year = 2022,
              state = "51",
              county = c("003", "540"),
              survey = "acs5")

ch %>% summarize(children = sum(estimate))

# Save ----
# child_care, child_care_region, children_u6_parentswork
write_csv(child_care, "data/child_care_supply_locality.csv")
write_csv(child_care_region, "data/child_care_supply_region.csv")
write_csv(ch, "data/children_under6_parentswork.csv")
