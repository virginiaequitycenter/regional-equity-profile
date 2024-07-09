# Living Standards Visualizations
# File Created: 7/9/2024
# Last Updated: 7/9/2024
# Author: Henry DeMarco
# Description: Visualizations for living standards section of profile

################################################

library(tidycensus)
library(tidyverse)
library(scales)
library(RColorBrewer)
library("ggspatial")
library(tigris)

# Table visualizations
# B20002 (Median Earnings in the Past 12 Months, Inflation-Adjusted)
# ALICE Households
# B19013 (Median Household Income: By Tract)
# B25064 (Median Gross Rent (Dollars)) - Tract Level, 2022
# B25003 (Home Ownership) - Tract Level, 2022

# Table: B20002 (Median Earnings in the Past 12 Months, Inflation-Adjusted)

# Palette for map
inc_colors <- c("#b1c5be", "#106449")

# Charlottesville (Median Earnings), 2022

cville_med_hh_inc <- read.csv("temp_data/cville_median_earnings_2022.csv") 

cville_tract <- tracts(state = "VA", county = "540") %>%
  mutate(GEOID = as.numeric((GEOID)))

cville_med_hhinc_tract_map <-
  cville_tract %>%
  left_join(cville_med_hh_inc, by = "GEOID")

# Geospatial visualization

cville_med_hhinc_map  <-
  ggplot(cville_med_hhinc_tract_map) +
  geom_sf( aes(fill = estimate), color = "black", alpha = .9) +
  scale_fill_steps(
    low = inc_colors[1],
    high = inc_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 8,
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Median Household Income") +
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
    legend.text = element_text(angle = -45, hjust = 0),
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
  )

# View the map

print(cville_med_hhinc_map)

# Alternate visualization (change in county-level median earnings over time):

cville_median_earnings_2012_2022 <- read.csv("temp_data/cville_median_earnings_2012_2022.csv") 

# Line graph: changes in median income over time (county-level)

cville_median_earnings_2012_2022 %>% 
  filter(label == "Median Earnings in Past 12 Months (Inflation Adjusted): Total") %>% 
  ggplot(aes(x=year, y=estimate)) +
  geom_line() +
  geom_point() +
  labs(title = "Median Income Change over Time: Charlottesville") +
  ylim(20000, 40000) +
  scale_x_continuous(breaks = pretty_breaks())

# Adding Lines for Total, Male, and Female

cville_median_earnings_2012_2022 %>% 
  filter(label %in% c(
    "Median Earnings in Past 12 Months (Inflation Adjusted): Total",
    "Median Earnings in Past 12 Months (Inflation Adjusted): Male",
    "Median Earnings in Past 12 Months (Inflation Adjusted): Female"
  )) %>%
  ggplot(aes(x=year, y=estimate, color=label)) +
  geom_line() +
  geom_point() +
  ylim(20000, 45000) +
  labs(title = "Median Income Change over Time: Charlottesville") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_color_discrete(name = "Label Type")

# Stacked Bar Chart

cville_median_earnings_2012_2022 %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = estimate, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::dollar(estimate)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = 1.2,
            size = 3,
            color = "white") +
  labs(
    x = "Categories",
    y = "Median Earnings",
    fill = "Year",
    title = "Charlottesville Median Earnings by Year"
  ) +
  scale_fill_manual(values = c("2012" = "#3B8EA5", "2022" = "#AB3428")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# Albemarle (Median Earnings), 2022

alb_med_hh_inc <- read.csv("temp_data/alb_median_earnings_2022.csv") 

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

alb_med_hhinc_tract_map <-
  alb_tract %>%
  left_join(alb_med_hh_inc, by = "GEOID")

# Geospatial visualization

alb_med_hhinc_map  <-
  ggplot(alb_med_hhinc_tract_map) +
  geom_sf( aes(fill = estimate), color = "black", alpha = .9) +
  scale_fill_steps(
    low = inc_colors[1],
    high = inc_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 8,
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Median Personal Income") +
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
    legend.text = element_text(angle = -45, hjust = 0),
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
  )

# View the map

print(alb_med_hhinc_map)

# Alternate visualization (change in county-level median earnings over time):

alb_median_earnings_2012_2022 <- read.csv("temp_data/alb_median_earnings_2012_2022.csv") 

# Line graph: changes in median income over time (county-level)

alb_median_earnings_2012_2022 %>% 
  filter(label == "Median Earnings in Past 12 Months (Inflation Adjusted): Total") %>% 
  ggplot(aes(x=year, y=estimate)) +
  geom_line() +
  geom_point() +
  ylim(20000, 60000) +
  labs(title = "Median Income Change over Time: Albemarle") +
  scale_x_continuous(breaks = pretty_breaks())

# Adding Lines for Total, Male, and Female

alb_median_earnings_2012_2022 %>% 
  filter(label %in% c(
    "Median Earnings in Past 12 Months (Inflation Adjusted): Total",
    "Median Earnings in Past 12 Months (Inflation Adjusted): Male",
    "Median Earnings in Past 12 Months (Inflation Adjusted): Female"
  )) %>%
  ggplot(aes(x=year, y=estimate, color=label)) +
  geom_line() +
  geom_point() +
  labs(title = "Median Income Change over Time: Albemarle") +
  ylim(20000, 60000) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_color_discrete(name = "Label Type")

# Stacked Bar Chart

alb_median_earnings_2012_2022 %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = estimate, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::dollar(estimate)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = 1.2,
            size = 3,
            color = "white") +
  labs(
    x = "Categories",
    y = "Median Earnings",
    fill = "Year",
    title = "Albemarle Median Earnings by Year"
  ) +
  scale_fill_manual(values = c("2012" = "#3B8EA5", "2022" = "#AB3428")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

inc_pal <- function(x) rgb(colorRamp(inc_colors)(x), maxColorValue = 255) 

# ALICE Households

# Charlottesville (ALICE Households), 2010-2022

cville_alice_2010_2022 <- read.csv("temp_data/cville_alice_2010_2022.csv")

# Plugging into old script:

alice_cville_hhs_graph <- cville_alice_2010_2022 %>% 
  mutate(level = case_when(
    level == "poverty_households" ~ "Poverty",
    level == "alice_households" ~ "ALICE",
    level == "above_alice_households" ~ "Above ALICE"
  )
  ) %>%
  group_by(year) %>%
  arrange(year, desc(level)) %>%
  mutate(height = cumsum(pct) - pct/2) %>%
  mutate(display = ifelse(pct > 2, paste0(round(pct),"%" ), ""))

alice_pal <- inc_pal( seq(0, 1, length = 3))

alice_cville_hhs_graph<-
  ggplot(alice_cville_hhs_graph, aes(x = year, y = pct, fill = level, group = level)) +
  geom_area(alpha=0.6 , size=.5, colour = "white") +
  # end label
  geom_text(data = alice_cville_hhs_graph %>%
              group_by( level) %>%
              arrange(desc(year)) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1.2, hjust = 1, size = 4) +
  # front Label
  geom_text(data = alice_cville_hhs_graph %>%
              group_by( level) %>%
              arrange(year) %>%
              slice(3),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 4) +
  # middle label
  geom_text(data = alice_cville_hhs_graph %>%
              group_by( level) %>%
              arrange(year) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = .5, size = 4) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(0, 100), breaks=seq(0,100, 25)) +
  scale_x_continuous( breaks=seq(2010,2022, 2), limits = c( 2010, 2022) )  +
  scale_fill_manual(values = alice_pal) +
  guides(fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)) +
  coord_cartesian(clip = 'off') +
  labs(x="Year", y="Population %", fill = "Income Status", title = "Asset Limited, Income Constrained, Employed Population in Charlottesville")  +
  theme_bw()+
  theme(
    plot.title = element_text( face="bold", hjust = .5, size = 12),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(face = "bold", vjust=-2.5, size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x=element_text(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line =  element_line(color  = "white"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face="bold", size = 10),
    panel.border = element_blank(),
    plot.margin=unit(c(t = .25, r = 1, b = 1.5, l = .1),"cm")
  )

# Alternate attempt:
# Adjust the data

alice_cville_hhs_graph <- cville_alice_2010_2022 %>% 
  mutate(level = case_when(
    level == "poverty_households" ~ "Poverty",
    level == "alice_households" ~ "ALICE",
    level == "above_alice_households" ~ "Above ALICE"
  )) %>%
  group_by(year) %>%
  arrange(year, desc(level)) %>%
  mutate(height = cumsum(pct) - pct / 2) %>%
  mutate(display = ifelse(pct > 2, paste0(round(pct), "%"), ""))

alice_pal <- c("#d73027", "#fc8d59", "#fee08b")

alice_cville_hhs_graph_plot <- ggplot(alice_cville_hhs_graph, aes(x = year, y = pct, fill = level, group = level)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  geom_text(aes(x = year, y = height, label = display), color = "black", size = 4) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_x_continuous(breaks = seq(2010, 2022, 1), limits = c(2010, 2022)) +
  scale_fill_manual(values = alice_pal) +
  guides(fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)) +
  coord_cartesian(clip = 'off') +
  labs(x = "Year", y = "Population %", fill = "Income Status", title = "Asset Limited, Income Constrained, Employed Population in Charlottesville") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(face = "bold", vjust = -2.5, size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "white"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.border = element_blank(),
    plot.margin = unit(c(t = 0.25, r = 1, b = 1.5, l = 0.1), "cm")
  )

# Still not quite right, might be an issue with scale... (!)

# Albemarle (ALICE Households), 2010-2022

alb_alice_2010_2022 <- read.csv("temp_data/alb_alice_2010_2022.csv")

# Plugging into old script:

alice_alb_hhs_graph <- alb_alice_2010_2022 %>% 
  mutate(level = case_when(
    level == "poverty_households" ~ "Poverty",
    level == "alice_households" ~ "ALICE",
    level == "above_alice_households" ~ "Above ALICE"
  )
  ) %>%
  group_by(year) %>%
  arrange(year, desc(level)) %>%
  mutate(height = cumsum(pct) - pct/2) %>%
  mutate(display = ifelse(pct > 2, paste0(round(pct),"%" ), ""))

alice_pal <- inc_pal( seq(0, 1, length = 3))

alice_alb_hhs_graph<-
  ggplot(alice_alb_hhs_graph, aes(x = year, y = pct, fill = level, group = level)) +
  geom_area(alpha=0.6 , size=.5, colour = "white") +
  # end label
  geom_text(data = alice_alb_hhs_graph %>%
              group_by( level) %>%
              arrange(desc(year)) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1.2, hjust = 1, size = 4) +
  # front Label
  geom_text(data = alice_alb_hhs_graph %>%
              group_by( level) %>%
              arrange(year) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 4) +
  # middle label
  geom_text(data = alice_alb_hhs_graph %>%
              group_by( level) %>%
              arrange(year) %>%
              slice(3),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = .5, size = 4) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(0, 100), breaks=seq(0,100, 25)) +
  scale_x_continuous( breaks=seq(2010,2022, 2), limits = c( 2010, 2022) )  +
  scale_fill_manual(values = alice_pal) +
  guides(fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)) +
  coord_cartesian(clip = 'off') +
  labs(x="Year", y="Population %", fill = "Income Status", title = "Asset Limited, Income Constrained, Employed Population in Albemarle")  +
  theme_bw()+
  theme(
    plot.title = element_text( face="bold", hjust = .5, size = 12),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(face = "bold", vjust=-2.5, size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x=element_text(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line =  element_line(color  = "white"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face="bold", size = 10),
    panel.border = element_blank(),
    plot.margin=unit(c(t = .25, r = 1, b = 1.5, l = .1),"cm")
  )

# Alternate graph

# Adjust the data
alice_alb_hhs_graph <- alb_alice_2010_2022 %>% 
  mutate(level = case_when(
    level == "poverty_households" ~ "Poverty",
    level == "alice_households" ~ "ALICE",
    level == "above_alice_households" ~ "Above ALICE"
  )) %>%
  group_by(year) %>%
  arrange(year, desc(level)) %>%
  mutate(height = cumsum(pct) - pct / 2) %>%
  mutate(display = ifelse(pct > 2, paste0(round(pct), "%"), ""))

alice_pal <- c("#d73027", "#fc8d59", "#fee08b")

# Create the plot
alice_alb_hhs_graph_plot <- ggplot(alice_alb_hhs_graph, aes(x = year, y = pct, fill = level, group = level)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  geom_text(aes(x = year, y = height, label = display), color = "black", size = 4) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_x_continuous(breaks = seq(2010, 2022, 1), limits = c(2010, 2022)) +
  scale_fill_manual(values = alice_pal) +
  guides(fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)) +
  coord_cartesian(clip = 'off') +
  labs(x = "Year", y = "Population %", fill = "Income Status", title = "Asset Limited, Income Constrained, Employed Population in Albemarle") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(face = "bold", vjust = -2.5, size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "white"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.border = element_blank(),
    plot.margin = unit(c(t = 0.25, r = 1, b = 1.5, l = 0.1), "cm")
  )

# Scales are wrong or something, this graph isn't working... (!)

# Table: B19013 (Median Household Income: By Tract)

# Charlottesville (Median Household Income: By Tract), 2022

cville_median_household_income_2022 <- read.csv("temp_data/cville_median_household_income_2022.csv")

# Median income visualization:

income_colors <- c("#B48291", "#A5243D")

cville_income_tract <- read.csv("temp_data/cville_median_household_income_2022.csv")

cville_tract <- tracts(state = "VA", county = "540") %>%
  mutate(GEOID = as.numeric((GEOID)))

cville_income_geo_tract <- cville_tract %>%
  left_join(cville_income_tract, by = "GEOID")

# Geospatial visualization

cville_income_map <-
  ggplot(cville_income_geo_tract) +
  geom_sf(aes(fill = estimate), color = "black") +
  scale_fill_steps(
    low = income_colors[1],
    high = income_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 4,
    labels = scales::dollar_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Median Household Income") +
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

# Albemarle (Median Household Income: By Tract), 2022

alb_median_household_income_2022 <- read.csv("temp_data/alb_median_household_income_2022.csv")

# Median income visualization:

income_colors <- c("#EDBF85", "#A5243D")

alb_income_tract <- read.csv("temp_data/alb_median_household_income_2022.csv")

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

alb_income_geo_tract <- alb_tract %>%
  left_join(alb_income_tract, by = "GEOID")

# Geospatial visualization

alb_income_map <-
  ggplot(alb_income_geo_tract) +
  geom_sf(aes(fill = estimate), color = "black") +
  scale_fill_steps(
    low = income_colors[1],
    high = income_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 4,
    labels = scales::dollar_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Median Household Income") +
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

# Table: B25064 (Median Gross Rent (Dollars)) - Tract Level, 2022

# Charlottesville (Gross Rent: Tracts), 2022

cville_gross_rent_tract_2022 <- read.csv("temp_data/cville_gross_rent_tract_2022.csv")

# Gross rent visualization:

rent_colors <- c("#FFB49A", "#FF4F79")

cville_rent_tract <- read.csv("temp_data/cville_gross_rent_tract_2022.csv")

cville_tract <- tracts(state = "VA", county = "540") %>%
  mutate(GEOID = as.numeric((GEOID)))

cville_rent_geo_tract <- cville_tract %>%
  left_join(cville_rent_tract, by = "GEOID")

# Geospatial visualization

cville_rent_map <-
  ggplot(cville_rent_geo_tract) +
  geom_sf(aes(fill = estimate), color = "black") +
  scale_fill_steps(
    low = rent_colors[1],
    high = rent_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 6,
    labels = scales::dollar_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Median Gross Rent") +
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

# Albemarle (Gross Rent: Tracts), 2022

alb_gross_rent_tract_2022 <- read.csv("temp_data/alb_gross_rent_tract_2022.csv")

# Gross rent visualization:

rent_colors <- c("#FFB49A", "#FF4F79")

alb_rent_tract <- read.csv("temp_data/alb_gross_rent_tract_2022.csv")

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

alb_rent_geo_tract <- alb_tract %>%
  left_join(alb_rent_tract, by = "GEOID")

# Geospatial visualization

alb_rent_map <-
  ggplot(alb_rent_geo_tract) +
  geom_sf(aes(fill = estimate), color = "black") +
  scale_fill_steps(
    low = rent_colors[1],
    high = rent_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 6,
    labels = scales::dollar_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Median Gross Rent") +
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

# Table: B25003 (Home Ownership) - Tract Level, 2022

# Charlottesville (Home Ownership), 2022

cville_home_ownership_2022 <- read.csv("temp_data/cville_home_ownership_2022.csv")

# Home owneship visualization

home_colors <- c("#FEFFA5", "#7B886F")

cville_home_tract <- read.csv("temp_data/cville_home_ownership_2022.csv")

cville_tract <- tracts(state = "VA", county = "540") %>%
  mutate(GEOID = as.numeric((GEOID)))

cville_home_geo_tract <- cville_tract %>%
  left_join(cville_home_tract, by = "GEOID") %>% 
  filter(label == "Owner occupied") %>%
  mutate(percent = percent / 100)  # Convert percent to fractions if necessary

# Geospatial visualization

cville_home_geo_tract_map <-
  ggplot(cville_home_geo_tract) +
  geom_sf(aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = home_colors[1],
    high = home_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 6,
    labels = scales::percent_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Owner Occupied (%)") +
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

# Renter visualization

home_colors <- c("#FEFFA5", "#7B886F")

cville_renter_tract <- read.csv("temp_data/cville_home_ownership_2022.csv")

cville_tract <- tracts(state = "VA", county = "540") %>%
  mutate(GEOID = as.numeric((GEOID)))

cville_renter_geo_tract <- cville_tract %>%
  left_join(cville_renter_tract, by = "GEOID") %>% 
  filter(label == "Renter occupied") %>%
  mutate(percent = percent / 100)  # Convert percent to fractions if necessary

# Geospatial visualization

cville_renter_geo_tract_map <-
  ggplot(cville_renter_geo_tract) +
  geom_sf(aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = home_colors[1],
    high = home_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 6,
    labels = scales::percent_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Renter Occupied (%)") +
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

# Albemarle (Home Ownership), 2022

alb_home_ownership_2022 <- read.csv("temp_data/alb_home_ownership_2022.csv")

# Home owneship visualization

home_colors <- c("#FEFFA5", "#7B886F")

alb_home_tract <- read.csv("temp_data/alb_home_ownership_2022.csv")

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

alb_home_geo_tract <- alb_tract %>%
  left_join(alb_home_tract, by = "GEOID") %>% 
  filter(label == "Owner occupied") %>%
  mutate(percent = percent / 100)  # Convert percent to fractions if necessary

# Geospatial visualization

alb_home_geo_tract_map <-
  ggplot(alb_home_geo_tract) +
  geom_sf(aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = home_colors[1],
    high = home_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 6,
    labels = scales::percent_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Owner Occupied (%)") +
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

# Renter visualization

home_colors <- c("#FEFFA5", "#7B886F")

alb_renter_tract <- read.csv("temp_data/alb_home_ownership_2022.csv")

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

alb_renter_geo_tract <- alb_tract %>%
  left_join(alb_renter_tract, by = "GEOID") %>% 
  filter(label == "Renter occupied") %>%
  mutate(percent = percent / 100)  # Convert percent to fractions if necessary

# Geospatial visualization

alb_renter_geo_tract_map <-
  ggplot(alb_renter_geo_tract) +
  geom_sf(aes(fill = percent), color = "black") +
  scale_fill_steps(
    low = home_colors[1],
    high = home_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 6,
    labels = scales::percent_format()
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Renter Occupied (%)") +
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
