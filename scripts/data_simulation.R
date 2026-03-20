# data_simulation.R
# Check Point 05
# Simulate vaccination-related data for pregnant adults

library(tidyverse)

set.seed(6270)

sim_data <- tibble(
  id = 1:200,
  year = sample(2012:2022, size = 200, replace = TRUE),
  vaccine = sample(c("Influenza", "Tdap"), size = 200, replace = TRUE),
  race_ethnicity = sample(
    c("Black, Non-Hispanic",
      "Hispanic",
      "Other or Multiple Races, Non-Hispanic",
      "White, Non-Hispanic"),
    size = 200, replace = TRUE
  ),
  estimate_pct = round(rnorm(200, mean = 65, sd = 10), 1),
  sample_size = sample(50:500, size = 200, replace = TRUE)
)

# Keep estimates within a realistic range
sim_data <- sim_data %>%
  mutate(
    estimate_pct = pmin(pmax(estimate_pct, 0), 100)
  )

# Inspect simulated data
glimpse(sim_data)
summary(sim_data)

# Save simulated dataset
write_csv(sim_data, "output/simulated_vaccination_data.csv")
