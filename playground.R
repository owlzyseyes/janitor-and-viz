# Curious about the effectivness of tobacco use and prevention 
# monitoring policies
library(dplyr)

of_interest <- c(
  "premature_ncd_death_prop",
  "ncd_deaths_total",
  "suicide_crude_rate_100k",
  "std_diabetes_trtmt_rate",
  "death_risk_30_70_ncd",
  "women_30to49_cerv_cancer_screen_preval"
)

others <- noncom |>
  filter(indicator_name %in% of_interest)

# NCD Deaths trend?
ggplot(data = others, aes(x=year, y=indicator_value_, color=indicator_name)) +
  geom_line()
