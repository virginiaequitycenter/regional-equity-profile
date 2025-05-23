# Living Standards Visualizations
# Description: Visualizations for living standards section of profile
# Authors: Beth Mitchell, Henry DeMarco
# File Created: 7/9/2024
# Last Updated: 8/15/2024


# Load packages ----
library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggspatial)
library(tigris)
library(readxl)
library(stringr)
library(sf)
library(ggthemes)
library(rcartocolor)
library(patchwork)

# Education palette ----
pal_inc <- carto_pal(7, "Emrld")

# Tract geometries ----
alb_geo <- tracts(state = "VA", county = "003")

# Common Items ----
locality_name <- "Albemarle County"
source_acs <- "Data Source: American Community Survey, 2022"

## ..............................................................
# Median personal earnings, by sex and race: Albemarle, 2022 ----
alb_med_earnings_race_county <- read_csv("../data/alb_med_earnings_race_county_2022.csv")

alb_med_earnings_race_county %>% 
  filter(race != "White") %>% 
  mutate(race = factor(race, levels = c("White, Not Hispanic or Latino", "Mutiracial", "Hispanic or Latino", "Black", "Asian"),
                       labels = c("White", "Mutiracial", "Hispanic", "Black", "Asian")),
         text = paste0("$", prettyNum(estimate, big.mark=",", preserve.width="none"))) %>%
  ggplot(aes(x = race, y = estimate, fill = sex)) +  
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8) +
  geom_text(aes(label = text), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            # vjust = 0.5,
            hjust = -0.15,
            size = 3) +
  scale_x_discrete(expand = expansion(mult = c(0, 0)),
                   name = "") +
  scale_y_continuous(labels = scales::label_currency(scale = 1, scale_cut = cut_short_scale()),
                     limits = c(0,70000),
                     expand = expansion(mult = c(0, 0)),
                     name = "") +
  scale_fill_manual(values = c("#b2b8be","#3B8EA5", "#6CC08B")) + 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.minor.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = "",
       title = "Median Personal Earnings, by Sex and Race", 
       subtitle = locality_name, 
       caption = source_acs) 

# Median household income by tract: Albemarle, 2022 ----
alb_med_hhinc_tract <- read_csv("../data/alb_med_hhinc_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))

alb_med_hhinc_tract <- alb_med_hhinc_tract %>%
  left_join(alb_geo, by = "GEOID") %>% 
  st_as_sf()

# Geospatial visualization
alb_med_hhinc_tract %>%
  filter(group == "All Households") %>%
  mutate(text = paste0("$", prettyNum(estimate, big.mark=",", preserve.width="none"))) %>%
  ggplot() +
  annotation_map_tile(zoomin = 1, progress = "none", cachedir = "../data/tempdata/") +
  geom_sf(aes(fill = estimate), color = "white") +
  geom_sf_text(aes(label = text), size = 2.5, color = "black") +
  scale_fill_stepsn(colors = pal_inc,
                    labels = scales::label_currency(scale = 1),
                    n.breaks = 6,
                    guide = guide_colourbar(title = "Median Household Income",
                                            title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5,
                                            # nbin = 6,
                                            theme = theme(legend.key.height  = unit(0.5, "lines"),
                                                          legend.key.width = unit(20, "lines"),
                                                          text = element_text(size = 11)) 
                    )
  )+
  labs(title = "Median Household Income", 
       subtitle = locality_name, 
       caption = source_acs) +
  theme_map()+
  theme(legend.position.inside=c(1, 1.04),
        legend.justification="right",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))

## ....................................................
# Median household income by Race: Albemarle, 2022 ----
alb_med_hhinc_county <- read_csv("../data/alb_med_hhinc_county_2022.csv")

alb_med_hhinc_county %>%
  filter(!group %in% c("White", "American Indian and Alaska Native", "NHPI", "Other")) %>% 
  mutate(group = factor(group, levels = c("White, Not Hispanic or Latino", "Multiracial", "Hispanic or Latino", "Black", "Asian", "All Households"),
                        labels = c("White", "	Multiracial", "Hispanic", "Black", "Asian", "All Households")),
         text = paste0("$", prettyNum(estimate, big.mark=",", preserve.width="none"))) %>%
  ggplot(aes(x = group, y = estimate, fill = group)) +
  coord_flip() +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = text), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3,
            color = "black") +
  scale_y_continuous(labels = scales::label_currency(scale = 1, scale_cut = cut_short_scale()),
                     breaks = c(0, 25000, 50000, 75000, 100000, 125000, 150000),
                     limits = c(0,150000),
                     expand = expansion(mult = c(0, .1))) +
  scale_fill_manual(values = c(rep("#6CC08B",5),"#b2b8be"))+
  labs(
    x = "",
    y = "Median Household Income",
    title = "Median Household Income by Race",
    subtitle = locality_name, 
    caption = source_acs
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

## ..........................................
# ALICE thresholds: Albemarle, 2010-2022 ----
alb_ALICE_threshold <- read_csv("../data/alb_ALICE_threshold_2010_2022.csv")

alb_ALICE_threshold %>% 
  pivot_longer(alice_threshold_hh_under_65:alice_threshold_hh_65_years_and_over, names_to = "level", values_to = "threshold") %>% 
  mutate(pos = case_when(level == "alice_threshold_hh_under_65" ~ 1,
                         level == "alice_threshold_hh_65_years_and_over" ~ -1
  ),
  text = paste0("$", prettyNum(threshold, big.mark=",", preserve.width="none"))) %>% 
  ggplot(aes(x = year, y = threshold, color = level, label = text)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_text(aes(y = (threshold + sign(pos)*6000)), size = 3, show.legend = FALSE, color = "black") +
  scale_x_continuous(
    breaks = seq(from = 2010, to = 2022, by = 1),
    name = "") +
  scale_y_continuous(labels = label_currency(scale = 1, scale_cut = cut_short_scale()),
                     breaks = c(0, 20000, 40000, 60000, 80000, 100000),
                     limits = c(0,100000),
                     name = "") +
  scale_color_manual(values = c("#b2b8be", "#3B8EA5")) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = "", 
       title = "ALICE Thresholds 2010-2022", 
       subtitle = locality_name, 
       caption = "Data Sources: ALICE Threshold, 2010–2022; U.S. Census Bureau, American Community Survey, 2010–2022") +  
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

## .......................................................
# ALICE households by race/ethnicity: Albemarle, 2022 ----
alb_ALICE_households_by_race <- read_csv("../data/alb_ALICE_households_by_race_2022.csv")

alb_ALICE_households_by_race <- alb_ALICE_households_by_race %>% 
  mutate(level = case_when(level == "above_alice_households" ~ "above",
                           level == "alice_households" ~ "alice",
                           level == "poverty_households" ~ "poverty",
                           .default = level))

alb_ALICE_households_by_race <- alb_ALICE_households_by_race[order(alb_ALICE_households_by_race$level),]

alb_ALICE_households_by_race %>% 
  arrange(desc(level)) %>% 
  mutate(group = factor(group,
                        levels = c("Asian", "Black", "Hispanic", "2+ Races", "White", "All"),
                        labels = c("Asian", "Black", "Hispanic", "Multiracial", "White", "All Households")),
         text = case_when(percent >= 1 ~ paste0(round(percent, 0), "%"),
                          percent < 1 ~ paste0(round(percent, 1), "%"))) %>%
  ggplot(aes(x = group, y = percent, group = group, fill = level, 
             label = text)) +
  geom_bar(stat = "identity") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#b2b8be", "#3B8EA5", "#97E196")) +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     name = "") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(color = "", 
       title = "ALICE households by race/ethnicity", 
       subtitle = locality_name, 
       caption = "Data Sources: ALICE Threshold, 2022; U.S. Census Bureau, American Community Survey, 2022") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

## .............................................
# Rent Burdened Households: Albemarle, 2022 ----

alb_rent_burden_county <- read_csv("../data/alb_rent_burden_county_2022.csv")  
alb_rent_burden_tract <- read_csv("../data/alb_rent_burden_tract_2022.csv")  

alb_rent_burden_tract <- alb_rent_burden_tract %>% 
  mutate(label = paste0(level, "\n", rent_burden),
         text = case_when(percent >= 1 ~ paste0(round(percent, 0), "%"),
                          percent < 1 ~ ""),
         rentallabel = prettyNum(total_units, big.mark=",", preserve.width="none")
         # rentallabel = paste0("Total Units: ", as.character(alb_rent_burden_tract$total_units))
  )
alb_rent_burden_tract <- alb_rent_burden_tract[order(alb_rent_burden_tract$tractnames, decreasing=TRUE),]
rentallabel <- as.list(alb_rent_burden_tract$rentallabel) %>% unique()

unburdened_plot <- alb_rent_burden_tract %>% 
  filter(level %in% c("Not burdened")) %>% 
  ggplot(aes(y = tractnames, x = percent, fill = factor(label), label= text)) +  
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.3), 
            # vjust = 0.5,
            # hjust = 0.5,
            size = 3) +
  scale_y_discrete(position = "left",
                   limits=rev,
                   name = "") +
  scale_x_reverse(labels = scales::percent_format(scale = 1),
                  breaks = seq(from = 25, to = 100, by = 25),
                  limits = c(100,0),
                  expand = expansion(mult = c(0, 0)),
                  name = "") +
  scale_fill_manual(values = c("#b2b8be")) + 
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.justification="right",
        legend.position = c(0.65, 1.04),
        legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "line"),
        axis.text = element_text(color = "black"))

burdened_plot <- alb_rent_burden_tract %>% 
  filter(level %in% c("Burdened", "Severely Burdened")) %>% 
  ggplot(aes(y = tractnames, x = percent, fill = factor(label), group = tractnames, label = text)) +  
  geom_bar(stat = "identity", color = "white") +
  geom_text(position = position_stack(vjust = 0.5),
            # vjust = 0.5,
            hjust = 0.001,
            size = 3) +
  scale_y_discrete(position = "right",
                   labels = rentallabel,
                   limits=rev,
                   name = "Total Rental Units by Census Tract") +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(from = 25, to = 100, by = 25),
                     limits = c(0,100),
                     expand = expansion(mult = c(0.005, 0)),
                     name = "") +
  scale_fill_manual(values = c("#ff641e", "#3B8EA5")) + 
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.justification="right",
        legend.position = c(1.3, 1.04),
        legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(1.6,0,0,0), "line"),
        axis.text = element_text(color = "black"))


(unburdened_plot + burdened_plot) + 
  plot_annotation(title = 'Rent Burdened Households by Tract',
                  subtitle = locality_name,
                  caption = source_acs)

## ............................................
# Home Ownership by tract: Albemarle, 2022 ----
alb_tenure_tract <- read_csv("../data/alb_tenure_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))

alb_tenure_tract <- alb_tenure_tract %>%
  left_join(alb_geo, by = "GEOID") %>% 
  st_as_sf()

# Geospatial visualization
alb_tenure_tract %>%
  filter(label == "Owner occupied") %>%
  mutate(text = paste0(round(percent,0), "%")) %>%
  ggplot() +
  annotation_map_tile(zoomin = 1, progress = "none", cachedir = "../data/tempdata/") +
  geom_sf(aes(fill = percent), color = "white") +
  geom_sf_text(aes(label = text), size = 2.5, color = "black") +
  scale_fill_stepsn(colors = pal_inc,
                    labels = scales::label_percent(scale = 1),
                    n.breaks = 8,
                    guide = guide_colourbar(title = "Percent Home Ownership",
                                            title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5,
                                            # nbin = 6,
                                            theme = theme(legend.key.height  = unit(0.5, "lines"),
                                                          legend.key.width = unit(18, "lines"),
                                                          text = element_text(size = 11)) 
                    )
  )+
  labs(title = "Home Ownership", 
       subtitle = locality_name, 
       caption = source_acs) +
  theme_map()+
  theme(legend.position.inside=c(1, 1.04),
        legend.justification="right",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))

## .................................................
# Home Ownership by Race, Albemarle 2012 & 2022 ----
alb_tenure_race_county <- read_csv("../data/alb_tenure_race_county_2012_2022.csv")

alb_homeowner_race <- alb_tenure_race_county %>% 
  filter(label == "Owner occupied") %>% 
  filter(!group %in% c("White", "American Indian/Native Alaskan", "Native Hawaiian/Pacific Islander", "Other")) %>% 
  select(group, percent, year) %>% 
  pivot_wider(names_from = year, values_from = percent) %>% 
  mutate(difference = `2022` - `2012`) %>% 
  pivot_longer(`2022`:`2012`) %>% 
  mutate(year = as.numeric(name))

# anno_list = as.list(alb_homeowner_race$difference) %>% unique()

alb_homeowner_race %>% 
  mutate(text = paste0(round(value, 0), "%"),
         group = factor(group, levels = c("Asian", "Black", "Hispanic or Latino", "Multiracial", "White, Not Hispanic or Latino"),
                        labels = c("Asian", "Black", "Hispanic", "Multiracial", "White"))) %>% 
  ggplot(aes(x = year, y = value, label = text)) +
  geom_line(linewidth = 1, color = "#3B8EA5") +
  geom_point(color = "#3B8EA5") +
  geom_text(size = 4, nudge_y = 15, color = "#3B8EA5", fontface = "bold") +
  # geom_text(data = annot_list, size = 4, nudge_y = -25, color = "#3B8EA5", fontface = "bold") +
  # annotate("text", x = 2017, y = 25, label = c("1", "2", "3", "4", "5"), color = "#3B8EA5", fontface = "bold") +
  scale_x_continuous(breaks = c(2012,2022),
                     expand = expansion(mult = c(0.1, .1)),
                     name = "") +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     breaks = c(0,100),
                     limits = c(0,100),
                     name = "") + 
  facet_wrap(~group, scales = "free") +
  labs(color = "", 
       title = "Home Ownership by Race: 2012 & 2022", 
       subtitle = locality_name, 
       caption = source_acs) +  
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 12))

## .........................................
# Gross Rent by tract: Albemarle, 2022 ----
alb_gross_rent_tract <- read_csv("../data/alb_gross_rent_tract_2022.csv") %>%
  mutate(GEOID = as.character(GEOID))

alb_gross_rent_tract <- alb_gross_rent_tract %>%
  left_join(alb_geo, by = "GEOID") %>% 
  st_as_sf()

# Geospatial visualization
alb_gross_rent_tract %>%
  mutate(text = case_when(is.na(estimate) ~ "",
                          !is.na(estimate) ~ paste0("$", prettyNum(estimate, big.mark=",", preserve.width="none")))) %>%
  ggplot() +
  annotation_map_tile(zoomin = 1, progress = "none", cachedir = "../data/tempdata/") +
  geom_sf(aes(fill = estimate), color = "white") +
  geom_sf_text(aes(label = scales::label_currency(scale = 1)(estimate)), size = 2.5, color = "black") +
  scale_fill_stepsn(colors = pal_inc,
                    labels = scales::label_currency(scale = 1),
                    n.breaks = 6,
                    guide = guide_colourbar(title = "Gross Rent",
                                            title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5,
                                            # nbin = 6,
                                            theme = theme(legend.key.height  = unit(0.5, "lines"),
                                                          legend.key.width = unit(20, "lines"),
                                                          text = element_text(size = 11)) 
                    )
  )+
  labs(title = "Gross Rent 2022", 
       subtitle = locality_name, 
       caption = source_acs) +
  theme_map()+
  theme(legend.position.inside=c(1, 1.04),
        legend.justification="right",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))

## ....................................................
## End