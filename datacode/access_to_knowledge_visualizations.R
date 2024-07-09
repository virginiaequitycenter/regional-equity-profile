# Access to Knowledge Visualizations
# File Created: 7/9/2024
# Last Updated: 7/9/2024
# Author: Henry DeMarco
# Description: Visualizations for access to knowledge section of profile

################################################

library(tidycensus)
library(tidyverse)
library(scales)
library(RColorBrewer)
library("ggspatial")
library(tigris)

# Table visualizations
# S1401 (School Enrollment)
# B15003 (Educational Attainment for the Population 25 Years and Over)
# Suspensions (by Race / Ethnicity)
# Chronic Absenteeism (by Race / Ethnicity)

# Table: S1401 (School Enrollment)

# Palette for map
educ_colors <- c("#e7dbbc", "#e68026")

# Charlottesville (School Enrollment), 2022:

cville_enroll_tract <- read.csv("temp_data/cville_school_enrollment_3to24_2022.csv")

cville_tract <- tracts(state = "VA", county = "540") %>%
  mutate(GEOID = as.numeric((GEOID)))

cville_enroll_geo_tract <- cville_tract %>%
  left_join(cville_enroll_tract, by = "GEOID")

# Geospatial visualization

cville_enroll_map <-
  ggplot(cville_enroll_geo_tract) +
  geom_sf( aes(fill = percent_enrolled), color = "black") +
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
  labs(fill = "Percent Enrolled in School [Ages 3 - 24]") +
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

# View the map

print(cville_enroll_map)

# Albemarle (School Enrollment), 2022:

alb_enroll_tract <- read.csv("temp_data/alb_school_enrollment_3to24_2022.csv")

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

alb_enroll_geo_tract <- alb_tract %>%
  left_join(alb_enroll_tract, by = "GEOID")

# Geospatial visualization

alb_enroll_map <-
  ggplot(alb_enroll_geo_tract) +
  geom_sf( aes(fill = percent_enrolled), color = "black") +
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
  labs(fill = "Percent Enrolled in School [Ages 3 - 24]") +
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

# View the map

print(alb_enroll_map)

# Table: B15003 (Educational Attainment for the Population 25 Years and Over)

educ_colors <- c("#e7dbbc", "#e68026")

# Charlottesville (Educational Attainment: 25 and Over), 2022

cville_educ_attain_tract <- read.csv("temp_data/cville_educ_attain_2022.csv")

cville_tract <- tracts(state = "VA", county = "540") %>%
  mutate(GEOID = as.numeric((GEOID)))

cville_attain_geo_tract <- cville_tract %>%
  left_join(cville_educ_attain_tract, by = "GEOID")

cville_attain_geo_tract_filtered <- cville_attain_geo_tract %>%
  filter(label == "Bachelor's degree or higher")

# Geospatial visualization

cville_educ_attain_map <-
  ggplot(cville_attain_geo_tract_filtered) + 
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

# View the map

print(cville_educ_attain_map)

# Albemarle (Educational Attainment: 25 and Over), 2022

alb_educ_attain_tract <- read.csv("temp_data/alb_educ_attain_2022.csv")

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

alb_attain_geo_tract <- alb_tract %>%
  left_join(alb_educ_attain_tract, by = "GEOID")

alb_attain_geo_tract_filtered <- alb_attain_geo_tract %>%
  filter(label == "Bachelor's degree or higher")

# Geospatial visualization

alb_educ_attain_map <-
  ggplot(alb_attain_geo_tract_filtered) + 
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

# View the map

print(alb_educ_attain_map)

# County School Education Stats (Enrollment, AP Courses, Suspensions, Absenteeism)

# Enrollment (From VSQ Profiles)

# Charlottesville (Enrollment, 2022-2023)

cville_enrollment <- read.csv("temp_data/cville_enrollment.csv", skip = 3) 

# Enrollment total: 4444

# Albemarle (Enrollment, 2022-2023)

alb_enrollment <- read.csv("temp_data/alb_enrollment.csv", skip = 3) 

# Enrollment total: 13835

# Suspensions (by Race / Ethnicity)

# Charlottesville (Short Term Suspensions, 2022-2023)

cville_st_suspensions <- read.csv("temp_data/cville_st_suspensions.csv")

# Manually add enrollment totals
cville_st_suspensions <- cville_st_suspensions %>%
  mutate(
    Enrollment_Total = 4444,
    Enrollment_Group = Enrollment_Total * (Percent.of.the.Student.Population / 100),
    Percent_Suspended = (Number.Suspended.Short.Term / Enrollment_Group) * 100
  )

cville_st_suspensions <- cville_st_suspensions %>%
  filter(!is.na(Percent_Suspended)) %>%
  mutate(
    Subgroup = factor(Subgroup, levels = rev(unique(Subgroup))),
    Suspended = Percent_Suspended
  )

cville_short_term_suspended_plot <- ggplot(cville_st_suspensions) +
  geom_segment(aes(xend = Suspended, x = 0, y = Subgroup, yend = Subgroup), size = 1) +
  geom_point(aes(x = Suspended, y = Subgroup), color = "blue", size = 3) +
  geom_vline(xintercept = mean(cville_st_suspensions$Suspended, na.rm = TRUE), linetype = "dashed", color = "black", size = .2) +
  annotate("text", x = mean(cville_st_suspensions$Suspended, na.rm = TRUE), y = nrow(cville_st_suspensions), label = "All Students", vjust = -1, hjust = -0.8, color = "black", size = 3.5) +
  geom_text(aes(x = Suspended + .2, y = Subgroup, label = paste0(round(Suspended), "%")), hjust = -0.5) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5), labels = function(x) paste0(x, "%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "Percent of Each Group Suspended Short-Term", y = "", title = "Charlottesville: Short Term Suspensions (By Race / Ethnicity)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b = 1, "cm")
  )

print(cville_short_term_suspended_plot)

# Note: dotted line for 'average' is improperly calculated, need to fix this...

# Albemarle (Short Term Suspensions, 2022-2023)

alb_st_suspensions <- read.csv("temp_data/alb_st_suspensions.csv")

# Manually add enrollment totals
alb_st_suspensions <- alb_st_suspensions %>%
  mutate(
    Enrollment_Total = 13835,
    Enrollment_Group = Enrollment_Total * (Percent.of.the.Student.Population / 100),
    Percent_Suspended = (Number.Suspended.Short.Term / Enrollment_Group) * 100
  )

alb_st_suspensions <- alb_st_suspensions %>%
  filter(!is.na(Percent_Suspended)) %>%
  mutate(
    Subgroup = factor(Subgroup, levels = rev(unique(Subgroup))),
    Suspended = Percent_Suspended
  )

alb_short_term_suspended_plot <- ggplot(alb_st_suspensions) +
  geom_segment(aes(xend = Suspended, x = 0, y = Subgroup, yend = Subgroup), size = 1) +
  geom_point(aes(x = Suspended, y = Subgroup), color = "blue", size = 3) +
  geom_vline(xintercept = mean(alb_st_suspensions$Suspended, na.rm = TRUE), linetype = "dashed", color = "black", size = .2) +
  annotate("text", x = mean(alb_st_suspensions$Suspended, na.rm = TRUE), y = nrow(alb_st_suspensions), label = "All Students", vjust = -1, hjust = -0.8, color = "black", size = 3.5) +
  geom_text(aes(x = Suspended + .2, y = Subgroup, label = paste0(round(Suspended), "%")), hjust = -0.5) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5), labels = function(x) paste0(x, "%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "Percent of Each Group Suspended Short-Term", y = "", title = "Albemarle: Short Term Suspensions (By Race / Ethnicity)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b = 1, "cm")
  )

print(alb_short_term_suspended_plot)

# Note: dotted line for 'average' is improperly calculated, need to fix this...

# Chronic Absenteeism (by Race / Ethnicity)

# Charlottesville (Chronic Absenteeism), 2022-2023

cville_absenteeism <- read.csv("temp_data/cville_absenteeism.csv")

# Manually add enrollment totals
cville_absenteeism <- cville_absenteeism %>%
  mutate(
    Enrollment_Total = 4444
  )

cville_absenteeism <- cville_absenteeism %>%
  filter(!is.na(Percent.above.10)) %>%
  mutate(
    Subgroup = factor(Subgroup, levels = rev(unique(Subgroup)))
  )

cville_absenteeism <- ggplot(cville_absenteeism) +
  geom_segment(aes(xend = Percent.above.10, x = 0, y = Subgroup, yend = Subgroup), size = 1) +
  geom_point(aes(x = Percent.above.10, y = Subgroup), color = "blue", size = 3) +
  geom_vline(xintercept = mean(cville_absenteeism$Percent.above.10, na.rm = TRUE), linetype = "dashed", color = "black", size = .2) +
  annotate("text", x = mean(cville_absenteeism$Percent.above.10, na.rm = TRUE), y = nrow(cville_absenteeism), label = "All Students", vjust = -1, hjust = -0.8, color = "black", size = 3.5) +
  geom_text(aes(x = Percent.above.10 + .2, y = Subgroup, label = paste0(round(Percent.above.10), "%")), hjust = -0.5) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), labels = function(x) paste0(x, "%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "Percent of Each Group Chronically Absent", y = "", title = "Charlottesville: Chronic Absenteeism (By Race / Ethnicity)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b = 1, "cm")
  )

print(cville_absenteeism)

# Note: dotted line for 'average' is improperly calculated, need to fix this...

# Albemarle (Chronic Absenteeism), 2022-2023

alb_absenteeism <- read.csv("temp_data/alb_absenteeism.csv")

# Manually add enrollment totals
alb_absenteeism <- alb_absenteeism %>%
  mutate(
    Enrollment_Total = 13835
  )

alb_absenteeism <- alb_absenteeism %>%
  filter(!is.na(Percent.above.10)) %>%
  mutate(
    Subgroup = factor(Subgroup, levels = rev(unique(Subgroup)))
  )

alb_absenteeism <- ggplot(alb_absenteeism) +
  geom_segment(aes(xend = Percent.above.10, x = 0, y = Subgroup, yend = Subgroup), size = 1) +
  geom_point(aes(x = Percent.above.10, y = Subgroup), color = "blue", size = 3) +
  geom_vline(xintercept = mean(alb_absenteeism$Percent.above.10, na.rm = TRUE), linetype = "dashed", color = "black", size = .2) +
  annotate("text", x = mean(alb_absenteeism$Percent.above.10, na.rm = TRUE), y = nrow(alb_absenteeism), label = "All Students", vjust = -1, hjust = -0.8, color = "black", size = 3.5) +
  geom_text(aes(x = Percent.above.10 + .2, y = Subgroup, label = paste0(round(Percent.above.10), "%")), hjust = -0.5) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), labels = function(x) paste0(x, "%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "Percent of Each Group Chronically Absent", y = "", title = "Albemarle: Chronic Absenteeism (By Race / Ethnicity)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b = 1, "cm")
  )

print(alb_absenteeism)

# Note: 'all students' dashed line and 'all students' percentage are misaligned...
