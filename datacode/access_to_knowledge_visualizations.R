# Access to Knowledge Visualizations
# Description: Visualizations for access to knowledge section of profile
# Author: Henry DeMarco, Beth Mitchell
# File Created: 7/9/2024
# Last Updated: 8/8/2024

# Packages
library(tidycensus)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggspatial)
library(tigris)
library(readxl)
library(stringr)
library(sf)

# Education palette ----
educ_colors <- c("#e7dbbc", "#e68026")

# Tract geometries ----
cville_geo <- tracts(state = "VA", county = "540")
alb_geo <- tracts(state = "VA", county = "003")

# School Enrollment: Charlottesville, 2022 ----
cville_enroll_tract <- read_csv("data/cville_enroll_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))

cville_enroll_tract <- cville_enroll_tract %>%
  left_join(cville_geo, by = "GEOID") %>% 
  st_as_sf()

# Geospatial visualization
cville_enroll_map <- cville_enroll_tract %>% 
  mutate(percent = round(percent, 0)) %>% 
  ggplot() +
  geom_sf(aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = educ_colors[1],
    high = educ_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Percent Enrolled in School (Ages 3 - 24)") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(
                           line_width = 1,
                           line_col = "black",
                           fill = "black",
                           text_col = "black",
                           text_family = "",
                           text_face = NULL,
                           text_size = 0
                         )) +
  theme(
    legend.position = "top",
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l = .1, r = .1, t = 1, b = 1, "cm")
    
  )

# View
print(cville_enroll_map)

# School Enrollment: Albemarle, 2022 ----
alb_enroll_tract <- read_csv("data/alb_enroll_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))

alb_enroll_tract <- alb_enroll_tract %>%
  left_join(alb_geo, by = "GEOID") %>% 
  st_as_sf()

# Geospatial visualization
alb_enroll_map <- alb_enroll_tract %>% 
  mutate(percent = round(percent, 0)) %>% 
  ggplot() +
  geom_sf(aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = educ_colors[1],
    high = educ_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Percent Enrolled in School (Ages 3 - 24)") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(
                           # line_width = 1,
                           # line_col = "black",
                           # fill = "black",
                           # text_col = "black",
                           # text_family = "",
                           # text_face = NULL,
                           text_size = 0
                         )
                         ) +
  theme(
    legend.position = "top",
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l = .1, r = .1, t = 1, b = 1, "cm")
  )

# View
print(alb_enroll_map)

# Educational Attainment by Race (25 Years and Over) Charlottesville, 2022 ----
cville_edu_attain_race <- read_csv("data/cville_edu_attain_race_county_2022.csv")

cville_edu_attain_race_plot <- cville_edu_attain_race %>% 
  filter(group_total != 0) %>% 
  mutate(label = factor(edu_level, 
                        levels = c("less_than_hs", 
                                   "hs_only", 
                                   "bachelors_up"), 
                        labels = c("Less than a HS diploma", "High School diploma, no college", "Bachelor's degree or higher")
                        # labels = c("White", "Black", "Asian", "Multiracial", "American Indian/Alaskan Native", "Native Hawaiian/Pacific Islander", "Other Racial Identities", "Hispanic")
                        ),
         text = case_when(percent >= 1 ~ paste0(round(percent, 0), "%"),
                          percent < 1 ~ "")
  )

# Stacked bar
cville_edu_race_plot <- cville_edu_attain_race_plot %>% 
  filter(!race %in% c("White", "Other")) %>% 
  filter(group_total >= 400) %>% 
  # filter(year %in% c("2012", "2017", "2022")) %>%
  # mutate(year = factor(year, levels = c("2012", "2017", "2022"))) %>%
  # mutate(race = factor(race, 
  #                      levels = c("White_non_hisp","Black","Hispanic","Asian","2016",
  #                                 "2017","2018","2019","2020","2021"),
  #                      labels = c("White, Non-Hispanic", "Black", "Hispanic", "Asian", "Multiracial", "American Indian/Alaskan Native", "Native Hawaiian/Pacific Islander")
  #                      
  #                      )) %>%
  ggplot(aes(x = race, y = percent, group = race, fill = label, 
             label = text)) +
  geom_bar(stat = "identity") + # width = 0.6
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     name = "") +
  labs(color = "", title = "Charlottesville: Educational Attainment 2022") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

cville_edu_race_plot


# Educational Attainment, with BA's by tract, Charlottesville, 2022 ----
cville_edu_attain_tract <- read_csv("data/cville_edu_attain_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))

cville_edu_attain_tract <- cville_edu_attain_tract %>%
  left_join(cville_geo, by = "GEOID") %>% 
  st_as_sf()

cville_attain_BAs <- cville_edu_attain_tract %>%
  filter(label == "Bachelor's degree or higher")

# Geospatial visualization
cville_educ_attain_map <- cville_attain_BAs %>% 
  mutate(percent = round(percent, 0)) %>% 
  ggplot() + 
  geom_sf( aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = educ_colors[1],
    high = educ_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Percent with Bachelor's Degree or Higher") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(
                           line_width = 1,
                           line_col = "black",
                           fill = "black",
                           text_col = "black",
                           text_family = "",
                           text_face = NULL,
                           text_size = 0
                         )) +
  theme(
    legend.position = "top",
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
  )

# View
print(cville_educ_attain_map)

# Educational Attainment (25 Years and Over) Albemarle, 2022 ----
alb_edu_attain_tract <- read_csv("data/alb_edu_attain_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))

alb_edu_attain_tract <- alb_edu_attain_tract %>%
  left_join(alb_geo, by = "GEOID") %>% 
  st_as_sf()

alb_attain_BAs <- alb_edu_attain_tract %>%
  filter(label == "Bachelor's degree or higher")

# Geospatial visualization
alb_educ_attain_map <- alb_attain_BAs %>% 
  mutate(percent = round(percent, 0)) %>% 
  ggplot() + 
  geom_sf( aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = educ_colors[1],
    high = educ_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Percent with Bachelor's Degree or Higher") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(
                           line_width = 1,
                           line_col = "black",
                           fill = "black",
                           text_col = "black",
                           text_family = "",
                           text_face = NULL,
                           text_size = 0
                         )) +
  theme(
    legend.position = "top",
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
  )

# View
print(alb_educ_attain_map)

# Advanced Courses, AP & Dual Enrollment: Charlottesville ----

cville_adv_enroll <- read_csv("data/cville_adv_enroll_2022_2023.csv")  

# AP Enrollment
cville_adv_enroll %>% 
  filter(! label %in% c("American Indian or Alaska Native", "Native Hawaiian or Pacific Islander")) %>% 
  mutate(label = factor(label,
                        levels = c("All Students", "Economically Disadvantaged",
                                   "Asian", "Black, not of Hispanic origin", "Hispanic",
                                   "Non-Hispanic, two or more races", "White, not of Hispanic origin"),
                        labels = c("All Students", "Economically \nDisadvantaged",
                                   "Asian", "Black", "Hispanic",
                                   "Multiracial", "White")),
         ap_percent = round(ap_percent, 0),
         text = paste0(ap_percent, "%")) %>% 
  ggplot(aes(x = label, y = ap_percent)) +
  geom_bar(stat = "identity", width = 0.7, fill = "#3B8EA5") + 
  geom_text(aes(label = text), 
            vjust = -1,
            size = 3,
            color = "black") +
  # geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     limits = c(0,100),
                     name = "") +
  labs(color = "", title = "CCS AP Enrollment 2022-2023") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

# AP & Dual Enrollment
cville_adv_enroll %>% 
  filter(! label %in% c("American Indian or Alaska Native", "Native Hawaiian or Pacific Islander")) %>% 
  select(label, ap_percent, dual_percent) %>% 
  pivot_longer(ap_percent:dual_percent, names_to = "type") %>% 
  mutate(label = factor(label,
                        levels = c("All Students", "Economically Disadvantaged",
                                   "Asian", "Black, not of Hispanic origin", "Hispanic",
                                   "Non-Hispanic, two or more races", "White, not of Hispanic origin"),
                        labels = c("All Students", "Economically \nDisadvantaged",
                                   "Asian", "Black", "Hispanic",
                                   "Multiracial", "White")),
         type = factor(type,
                       levels = c("ap_percent", "dual_percent"),
                       labels= c("Students taking 1 or more AP Courses", "Students taking 1 or more Dual Enrollment Courses")),
         value = round(value, 0),
         text = paste0(value, "%")) %>% 
  ggplot(aes(x = label, y = value, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8) + 
  geom_text(aes(label = text), 
              position = position_dodge2(width = 0.9, preserve = "single"), 
              vjust = -1,
              # hjust = -0.2,
              size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     limits = c(0,100),
                     name = "") +
  scale_fill_manual(values = c("#3B8EA5", "#8C96C6")) +
  labs(color = "", title = "CCS AP & Dual Enrollment 2022-2023") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

# Short Term Suspensions: Charlottesville ----
cville_suspensions <- read_csv("data/cville_st_suspensions_2022_2023.csv")  

cville_suspensions %>% 
  filter(! subgroup %in% c("American Indian", "Native Hawaiian")) %>% 
  select(subgroup, percent_of_the_student_population, percent_of_short_term_suspensions) %>% 
  pivot_longer(percent_of_the_student_population:percent_of_short_term_suspensions, names_to = "type") %>% 
  mutate(subgroup = factor(subgroup,
                        levels = c("White", "Multiple Races", "Hispanic", "Black", "Asian")),
         type = factor(type,
                       levels = c("percent_of_the_student_population", "percent_of_short_term_suspensions"),
                       labels= c("Percent of the student population", "Percent of short term suspensions")),
         value = round(value, 0),
         text = paste0(value, "%")) %>% 
  ggplot(aes(x = subgroup, y = value, fill = type)) +  
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8) +
  geom_text(aes(label = text), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0,100),
                     expand = expansion(mult = c(0, 0)),
                     name = "") +
  scale_fill_manual(values = c("#b2b8be", "#ff641e")) + 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.minor.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = "", title = "CCS Short Term Suspension Incidents, by Race/Ethnicity")

# Chronic Absenteeism: Charlottesville ----
cville_absent <- read_csv("data/cville_absenteeism_2022_2023.csv")  

cville_absent %>% 
  filter(! subgroup %in% c("American Indian", "Native Hawaiian")) %>% 
  mutate(subgroup = factor(subgroup,
                        levels = c("All Students", "Economically Disadvantaged", "Students with Disabilities",
                                   "Male", "Female",
                                   "Asian", "Black", "Hispanic", "Multiple Races", "White"),
                        labels = c("All Students", "Economically \nDisadvantaged", "Students with \nDisabilities",
                                   "Male", "Female",
                                   "Asian", "Black", "Hispanic", "Multiracial", "White")),
         percent_above_10 = round(percent_above_10, 0),
         text = paste0(percent_above_10, "%")) %>% 
  ggplot(aes(x = subgroup, y = percent_above_10)) +
  geom_bar(stat = "identity", width = 0.7, fill = "#3B8EA5") + 
  geom_text(aes(label = text), 
            vjust = -1,
            size = 3,
            color = "black") +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     limits = c(0,60),
                     name = "") +
  labs(color = "", title = "CCS Chronic Absenteeism 2022-2023") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

