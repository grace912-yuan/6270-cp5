# data_exploration.R
# Check Point 05
# Explore CDC vaccination in pregnancy dataset

# Load packages
library(tidyverse)

# Read data
cdc_data <- read_csv("data/CDC_vaccination_pregnancy_20250120.csv")

# View data structure
glimpse(cdc_data)
names(cdc_data)
dim(cdc_data)

# Summary of variables
summary(cdc_data)

# Count missing values
missing_summary <- cdc_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "missing_n")

print(missing_summary)

# Example numeric summaries
numeric_summary <- cdc_data %>%
  select(where(is.numeric)) %>%
  summary()

print(numeric_summary)

# Convert estimate to numeric
cdc_data <- cdc_data %>%
  mutate(
    estimate_pct = as.numeric(`Estimate (%)`)
  )

# Check conversion
summary(cdc_data$estimate_pct)

# Average estimate by vaccine and year
vaccine_year_summary <- cdc_data %>%
  group_by(Vaccine, `Survey Year/Influenza Season`) %>%
  summarise(
    mean_estimate = mean(estimate_pct, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(vaccine_year_summary)

# Plot trends over time by vaccine
library(ggplot2)

ggplot(vaccine_year_summary,
       aes(x = `Survey Year/Influenza Season`,
           y = mean_estimate,
           color = Vaccine)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average vaccination estimate over time by vaccine",
    x = "Survey year",
    y = "Average estimate (%)",
    color = "Vaccine"
  ) +
  theme_minimal()

# Save plot
ggsave("output/vaccine_trend_plot.png", width = 8, height = 5)

unique(cdc_data$Vaccine)
unique(cdc_data$`Dimension Type`)

# Average estimate by vaccine and race/ethnicity
race_summary <- cdc_data %>%
  filter(`Dimension Type` == "Race and Ethnicity") %>%
  group_by(Vaccine, Dimension) %>%
  summarise(
    mean_estimate = mean(estimate_pct, na.rm = TRUE),
    .groups = "drop"
  )

print(race_summary)

# Plot average estimate by race/ethnicity and vaccine
ggplot(race_summary,
       aes(x = Dimension, y = mean_estimate, fill = Vaccine)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average vaccination estimate by race and ethnicity",
    x = "Race and ethnicity",
    y = "Average estimate (%)",
    fill = "Vaccine"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot
ggsave("output/vaccine_race_plot.png", width = 9, height = 5)
