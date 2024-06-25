# Demographic Visualizations
# File Created: 6/25/2024
# Last Updated: 6/25/2024
# Author: Henry DeMarco
# Description: Visualizations for demographic section of profile

################################################

library(tidycensus)
library(tidyverse)
library(scales)
library(RColorBrewer)
library("ggspatial")
library(tigris)

# Table visualizations
# B02001 (Race)
# B03003 (Hispanic or Latino Origin)
# B03002 (Hispanic or Latino Origin by Race)
# B01001 (Sex by Age)
# B05001 (Nativity and Citizenship Status in the United States)
# B16002 (Household Language by Household Limited English Speaking Status)
# B18102-B18106 (Combined Disability Status Tables)

# Table: B02001 (Race)

# Charlottesville (Race), 2012-2022:

cville_race_2012_2022 <- read.csv("data/per_race_cville_2012_2022.csv")

# Line graph with all ACS-defined races

cville_race_2012_2022 %>% 
  ggplot(aes(x = year, y = percent, color = label)) +
  geom_line() +
  geom_point() +
  ylim(0, 85) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(color = "Race", title = "Racial Composition over Time (Charlottesville)")

# Line graph filtered for white, black, asian

cville_race_2012_2022 %>% 
  filter(label %in% c("Asian alone", "Black or African American alone", "White alone")) %>% 
  ggplot(aes(x = year, y = percent, color = label)) +
  geom_line() +
  geom_point() +
  ylim(0, 85) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(color = "Race", title = "Racial Composition over Time (Charlottesville)")

# Simple bar graph

cville_race_2012_2022 %>% 
  filter(year %in% c("2012", "2022")) %>% 
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = "dodge2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Race", y = "Percentage", fill = "Year", title = "Charlottesville Racial Composition by Year") +
  scale_fill_gradientn(colors = c("#3B8EA5","#AB3428")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Improved bar graph

cville_race_2012_2022 %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 80)) +
  labs(
    x = "Race",
    y = "Percentage",
    fill = "Year",
    title = "Charlottesville Racial Composition by Year"
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

# Albemarle (Race), 2012-2022:

alb_race_2012_2022 <- read.csv("data/per_race_alb_2012_2022.csv")

# Line graph with all ACS-defined races

alb_race_2012_2022 %>% 
  ggplot(aes(x = year, y = percent, color = label)) +
  geom_line() +
  geom_point() +
  ylim(0, 85) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(color = "Race", title = "Racial Composition over Time (Albemarle)")

# Line graph filtered for white, black, asian

alb_race_2012_2022 %>% 
  filter(label %in% c("Asian alone", "Black or African American alone", "White alone")) %>% 
  ggplot(aes(x = year, y = percent, color = label)) +
  geom_line() +
  geom_point() +
  ylim(0, 85) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(color = "Race", title = "Racial Composition over Time (Albemarle)")

# Simple bar graph

alb_race_2012_2022 %>% 
  filter(year %in% c("2012", "2022")) %>% 
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = "dodge2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Race", y = "Percentage", fill = "Year", title = "Albemarle Racial Composition by Year") +
  scale_fill_gradientn(colors = c("#3B8EA5","#AB3428")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Improved bar graph

alb_race_2012_2022 %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 90)) +
  labs(
    x = "Race",
    y = "Percentage",
    fill = "Year",
    title = "Albemarle Racial Composition by Year"
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

# Table: B03003 (Hispanic or Latino Origin)

# Charlottesville (Hispanic or Latino Origin), 2012-2022:

cville_hispanic_origin_2012_2022 <- read.csv("data/cville_hispanic_origin_2012_2022.csv")

# Simple line graph (change over time)

cville_hispanic_origin_2012_2022 %>% 
  filter(label == "Hispanic or Latino") %>% 
  ggplot(aes(x=year, y=percent)) +
  geom_line() +
  geom_point() +
  labs(title = "Hispanic / Latino Change over Time: Charlottesville") +
  ylim(0, 10) +
  scale_x_continuous(breaks = pretty_breaks())

# Alternate visualization (stacked bar charts)

cville_hispanic_origin_2012_2022 %>% 
  filter(label == "Hispanic or Latino") %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 20)) +
  labs(
    x = "Hispanic or Latino",
    y = "Percentage",
    fill = "Year",
    title = "Charlottesville Ethnic Composition by Year"
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

# Albemarle (Hispanic or Latino Origin), 2012-2022:

alb_hispanic_origin_2012_2022 <- read.csv("data/alb_hispanic_origin_2012_2022.csv")

# Simple line graph (change over time)

alb_hispanic_origin_2012_2022 %>% 
  filter(label == "Hispanic or Latino") %>% 
  ggplot(aes(x=year, y=percent)) +
  geom_line() +
  geom_point() +
  labs(title = "Hispanic / Latino Change over Time: Albemarle") +
  ylim(0, 10) +
  scale_x_continuous(breaks = pretty_breaks())

# Alternate visualization (stacked bar charts)

alb_hispanic_origin_2012_2022 %>% 
  filter(label == "Hispanic or Latino") %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 20)) +
  labs(
    x = "Hispanic or Latino",
    y = "Percentage",
    fill = "Year",
    title = "Albemarle Ethnic Composition by Year"
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

# Table: B03002 (Hispanic or Latino Origin by Race)

# Charlottesville (Hispanic or Latino Origin by Race), 2012-2022:

cville_hispanic_origin_by_race_2012_2022 <- read.csv("data/cville_hispanic_origin_by_race_2012_2022.csv")

# Simple line graph (change over time)

cville_hispanic_origin_by_race_2012_2022 %>% 
  ggplot(aes(x = year, y = percent, color = label)) +
  geom_line() +
  geom_point() +
  ylim(0, 85) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(color = "Race/Ethnicity", title = "Racial/Ethnic Composition over Time (Charlottesville)")

# Alternate visualization: stacked bar chart

cville_hispanic_origin_by_race_2012_2022 %>% 
  filter(label %in% c("Hispanic or Latino", "Not Hispanic or Latino: White alone", "Not Hispanic or Latino: Asian alone", "Not Hispanic or Latino: Black or African American alone")) %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 80)) +
  labs(
    x = "Race / Ethnicity",
    y = "Percentage",
    fill = "Year",
    title = "Charlottesville Ethnic Composition by Year"
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

# Albemarle (Hispanic or Latino Origin by Race), 2012-2022:

alb_hispanic_origin_by_race_2012_2022 <- read.csv("data/alb_hispanic_origin_by_race_2012_2022.csv")

# Simple line graph (change over time)

alb_hispanic_origin_by_race_2012_2022 %>% 
  ggplot(aes(x = year, y = percent, color = label)) +
  geom_line() +
  geom_point() +
  ylim(0, 85) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(color = "Race/Ethnicity", title = "Racial/Ethnic Composition over Time (Charlottesville)")

# Alternate visualization: stacked bar charts

alb_hispanic_origin_by_race_2012_2022 %>% 
  filter(label %in% c("Hispanic or Latino", "Not Hispanic or Latino: White alone", "Not Hispanic or Latino: Asian alone", "Not Hispanic or Latino: Black or African American alone")) %>% 
  filter(year %in% c("2012", "2022")) %>% 
  mutate(year = factor(year, levels = c("2012", "2022"))) %>%
  ggplot(aes(x = label, y = percent, fill = year)) +
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.7) +
  geom_text(aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
            position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = 0.5,
            hjust = -0.2,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 90)) +
  labs(
    x = "Race / Ethnicity",
    y = "Percentage",
    fill = "Year",
    title = "Albemarle Ethnic Composition by Year"
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

# Table: B01001 (Sex by Age)

# Charlottesville (Age Groups), 2012-2022

cville_combined_age_2012_2022 <- read.csv("data/cville_combined_age_2012_2022.csv")

cville_combined_age_2012_2022 %>% 
  ggplot(aes(x = age_group, y = percent, fill = factor(year))) +  # Convert year to factor here
  geom_bar(stat = "identity", position = "dodge2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Age Group", y = "Percentage", fill = "Year", title = "Charlottesville Age Groups by Year") +
  scale_fill_manual(values = c("#C84630", "#FBBA72")) +  # Apply custom colors here
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Albemarle (Age Groups), 2012-2022

alb_combined_age_2012_2022 <- read.csv("data/alb_combined_age_2012_2022.csv")

alb_combined_age_2012_2022 %>% 
  ggplot(aes(x = age_group, y = percent, fill = factor(year))) +  # Convert year to factor here
  geom_bar(stat = "identity", position = "dodge2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Age Group", y = "Percentage", fill = "Year", title = "Albemarle Age Groups by Year") +
  scale_fill_manual(values = c("#C84630", "#FBBA72")) +  # Apply custom colors here
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Table: B05001 (Nativity and Citizenship Status in the United States)

# Charlottesville (Nativity and Citizenship Status in the United States), 2022:

cville_nativity_2022 <- read.csv("data/cville_nativity_2022.csv")

# Compute cumulative percentages for the donut chart
cville_nativity_2022 <- cville_nativity_2022 %>%
  mutate(
    fraction = percent / 100,
    ymax = cumsum(fraction), 
    ymin = c(0, head(ymax, n = -1)),
    status = fct_recode(label, 
                        "US citizen, born in US" = "U.S. citizen, born in the United States",
                        "US citizen, born in\nunincorporated territories" = "U.S. citizen, born in Puerto Rico or U.S. Island Areas",
                        "US citizen, born abroad" = "U.S. citizen, born abroad of American parent(s)",
                        "US citizen, naturalized" = "U.S. citizen by naturalization",
                        "Not US citizen" = "Not a U.S. citizen"
    )
  )

# Create a color palette for the donut chart
donut_pal <- sample(brewer.pal(7, "BuPu")[-1], 5, replace = FALSE)

# Plot the donut chart
cville_nativity_chart_2022 <- ggplot(cville_nativity_2022, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = status)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1.5, 4)) +
  scale_fill_manual(values = donut_pal) +
  geom_text(aes(label = paste0(round(percent, 1), "%"), x = c(3.75, 3.5, 3.75, 3.75, 3.75), y = (ymin + ymax) / 2), color = "white") +
  theme_void() +
  theme(legend.position = c(.525, .5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 8)) +
  labs(title = "Nativity (Charlottesville), 2022")

# View the chart

print(cville_nativity_chart_2022)

# Albemarle (Nativity and Citizenship Status in the United States), 2022:

alb_nativity_2022 <- read.csv("data/alb_nativity_2022.csv")

# Compute cumulative percentages for the donut chart
alb_nativity_2022 <- alb_nativity_2022 %>%
  mutate(
    fraction = percent / 100,
    ymax = cumsum(fraction), 
    ymin = c(0, head(ymax, n = -1)),
    status = fct_recode(label, 
                        "US citizen, born in US" = "U.S. citizen, born in the United States",
                        "US citizen, born in\nunincorporated territories" = "U.S. citizen, born in Puerto Rico or U.S. Island Areas",
                        "US citizen, born abroad" = "U.S. citizen, born abroad of American parent(s)",
                        "US citizen, naturalized" = "U.S. citizen by naturalization",
                        "Not US citizen" = "Not a U.S. citizen"
    )
  )

# Create a color palette for the donut chart
donut_pal <- sample(brewer.pal(7, "BuPu")[-1], 5, replace = FALSE)

# Plot the donut chart
alb_nativity_chart_2022 <- ggplot(alb_nativity_2022, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = status)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1.5, 4)) +
  scale_fill_manual(values = donut_pal) +
  geom_text(aes(label = paste0(round(percent, 1), "%"), x = c(3.75, 3.5, 3.75, 3.75, 3.75), y = (ymin + ymax) / 2), color = "white") +
  theme_void() +
  theme(legend.position = c(.525, .5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 8)) +
  labs(title = "Nativity (Albemarle), 2022")

# View the chart

print(alb_nativity_chart_2022)

# Table: B05002 (Place of Birth by Nativity) - not yet added

# Charlottesville (Place of Birth by Nativity), 2022:

# Albemarle (Place of Birth by Nativity), 2022:

# Table: B16002 (Household Language by Household Limited English Speaking Status)

# Charlottesville (Limited English), 2022:

cville_limited_english_2022 <- read.csv("data/cville_limited_english_2022.csv")

# Shortening variable labels:

cville_limited_english_2022 <- cville_limited_english_2022 %>%
  mutate(label = str_replace(label, "speaking", "")) %>%
  mutate(label = str_squish(label))

# Bar graph, filtering out estimates with value of zero:

cville_limited_english_2022 %>%
  filter(label != "English only") %>% 
  filter(estimate != 0) %>% 
  ggplot(aes(x = label, y = estimate, fill = label)) +
  geom_bar(stat = "identity") +
  labs(title = "Limited English by Language (Charlottesville)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Albemarle (Limited English), 2022:

alb_limited_english_2022 <- read.csv("data/alb_limited_english_2022.csv")

alb_limited_english_2022 <- alb_limited_english_2022 %>%
  mutate(label = str_replace(label, "speaking", "")) %>%
  mutate(label = str_squish(label))

# Bar graph, filtering out estimates with value of zero:

alb_limited_english_2022 %>%
  filter(label != "English only") %>% 
  filter(estimate != 0) %>% 
  ggplot(aes(x = label, y = estimate, fill = label)) +
  geom_bar(stat = "identity") +
  labs(title = "Limited English by Language (Albemarle)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Table: Combined Disability Status (B18102-B18106), 2022:

# Charlottesville (Combined Disability Status), 2022:

cville_combined_disability_total_2022 <- read.csv("data/cville_combined_disability_total_2022.csv")

# Stacked bar chart

cville_combined_disability_total_2022 %>%
  ggplot(aes(x = reorder(label, -estimate), y = estimate, fill = label)) +
  coord_flip() +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = scales::comma(estimate)), 
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  labs(
    x = "Categories",
    y = "Population",
    fill = "Category",
    title = "Charlottesville Disability Status for 2022"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# Donut chart

# Calculate the percentage of each category
cville_combined_disability_total_2022 <- cville_combined_disability_total_2022 %>%
  mutate(percentage = estimate / sum(estimate) * 100)

# Create the donut chart
ggplot(cville_combined_disability_total_2022, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +  # Adjust to create the donut hole
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(
    fill = "Category",
    title = "Charlottesville Disability Status for 2022"
  ) +
  theme_void() +  # Remove background, gridlines, and axis texts
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# Albemarle (Comblined Disability Status), 2022:

alb_combined_disability_total_2022 <- read.csv("data/alb_combined_disability_total_2022.csv")

# Stacked bar chart

alb_combined_disability_total_2022 %>%
  ggplot(aes(x = reorder(label, -estimate), y = estimate, fill = label)) +
  coord_flip() +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = scales::comma(estimate)), 
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  labs(
    x = "Categories",
    y = "Population",
    fill = "Category",
    title = "Albemarle Disability Status for 2022"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# Donut chart

# Calculate the percentage of each category
alb_combined_disability_total_2022 <- alb_combined_disability_total_2022 %>%
  mutate(percentage = estimate / sum(estimate) * 100)

# Create the donut chart
ggplot(alb_combined_disability_total_2022, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +  # Adjust to create the donut hole
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(
    fill = "Category",
    title = "Albemarle Disability Status for 2022"
  ) +
  theme_void() +  # Remove background, gridlines, and axis texts
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

