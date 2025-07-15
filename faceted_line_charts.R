library(tidyverse)
library(ggplot2)
library(scales)
theme_minimal()

# Q: What is the overral trend of age_standardised NCD prevalences?
noncom_std_prev <- noncom |>
  filter(str_detect(tolower(indicator_name), "std") & 
           str_detect(tolower(indicator_name), "preval")) |> 
  arrange(year)

plot_data <- noncom_std_prev |>
  select(year, indicator_name, dimension_name, indicator_value_) |>
  rename(sex = dimension_name) |>
  filter(sex == "Male" | sex == "Female") |> 
  arrange(year)


male_color <- "#0d6efd"    
female_color <- "#d63384"

# All 6 indicators
indicators_to_plot <- c(
  "adult_overwt_std_preval",
  "adult_underwt_std_preval", 
  "curr_cig_smoke_std_preval",
  "insuff_activity_adult_std_preval",
  "std_adult_obesity_preval",
  "std_diabetes_preval"
)

# Process all data at once
all_plot_data <- plot_data %>%
  filter(indicator_name %in% indicators_to_plot) %>%
  distinct(year, indicator_name, sex, .keep_all = TRUE) %>%
  pivot_wider(names_from = sex, values_from = indicator_value_) %>%
  mutate(
    min_val = pmin(Male, Female, na.rm = TRUE),
    max_val = pmax(Male, Female, na.rm = TRUE),
    male_higher = Male > Female,
    female_higher = Female > Male
  ) %>%
  group_by(indicator_name) %>%
  mutate(
    # Increase ylim by 3 for each indicator
    max_y_limit = max(max_val, na.rm = TRUE) + 3
  ) %>%
  ungroup()

# Create clean labels matching your JavaScript formatIndicatorName function
indicator_labels <- c(
  "adult_overwt_std_preval" = "Adult Overweight Prevalence",
  "adult_underwt_std_preval" = "Adult Underweight Prevalence", 
  "curr_cig_smoke_std_preval" = "Cigarette Smoking Prevalence",
  "insuff_activity_adult_std_preval" = "Adult Insufficient Physical Activity Prevalence",
  "std_adult_obesity_preval" = "Adult Obesity Prevalence",
  "std_diabetes_preval" = "Diabetes Prevalence"
)

# Create the faceted plot in 3×2 grid
ggplot(all_plot_data, aes(x = year)) +
  # Area when Male > Female (blue)
  geom_ribbon(aes(ymin = min_val, ymax = max_val), 
              data = filter(all_plot_data, male_higher),
              fill = "#bae1ff", alpha = 0.7) +
  # Area when Female > Male (pink)  
  geom_ribbon(aes(ymin = min_val, ymax = max_val), 
              data = filter(all_plot_data, female_higher),
              fill = "#ffb3ba", alpha = 0.7) +
  # Add horizontal line at y = 0 (x-axis baseline)
  geom_hline(yintercept = 0, color = "#e8e8e8", size = 2) +
  # Add the lines on top
  geom_line(aes(y = Female), color = female_color, size = 2) +
  geom_line(aes(y = Male), color = male_color, size = 2) +
  # Facet with 3 columns (3×2 grid)
  facet_wrap(~ indicator_name, 
             ncol = 3, 
             scales = "free_y",
             labeller = labeller(indicator_name = indicator_labels)) +
  scale_y_continuous(
    labels = percent_format(scale = 1, accuracy = 1),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#e8e8e8", size = 1),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.spacing.x = unit(1.5, "cm"),  # Horizontal spacing between columns
    panel.spacing.y = unit(1.2, "cm")   # Vertical spacing between rows
  ) +
  labs(
    title = "Health Indicators Prevalence",
    x = "Year",
    y = "Prevalence (%)"
  )
