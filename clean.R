library(janitor)
library(tidyverse)
library(readr)

noncom <- read_csv("data/noncommunicable_diseases_indicators_ken.csv",
                   skip=1, show_col_types = FALSE)

noncom <- noncom |> 
  clean_names()

n_distinct(noncom$number_indicator_name)

# Quick check of counts per indicator
stuff <- noncom |> 
  group_by(number_indicator_name) |> 
  summarize(count = n()) |> 
  arrange(desc(count))

stuff_2 <- noncom |> 
  group_by(number_indicator_name) |> 
  summarize(count = n()) |> 
  arrange(desc(count))

# Cleaning the indicators column
noncom <- noncom |> 
  mutate(number_indicator_name = case_when(
    number_indicator_name == "NCD country capacity survey response" ~ "ncd_capacity_survey_resp",
    number_indicator_name == "Prevalence of obesity among adults, BMI &GreaterEqual; 30 (age-standardized estimate) (%)" ~ "std_adult_obesity_preval",
    number_indicator_name == "Prevalence of overweight among children and adolescents, BMI > +1 standard deviations above the median (crude estimate) (%)" ~ "youth_overwt_crude_preval",
    number_indicator_name == "Prevalence of thinness among children and adolescents, BMI < -2 standard deviations below the median (crude estimate) (%)" ~ "youth_thinness_crude_preval",
    number_indicator_name == "Prevalence of diabetes, age-standardized" ~ "std_diabetes_preval",
    number_indicator_name == "Prevalence of insufficient physical activity among adults aged 18+ years (crude estimate) (%)" ~ "insuff_activity_adult_crude_preval",
    number_indicator_name == "Premature deaths due to noncommunicable diseases (NCD) as a proportion of all NCD deaths" ~ "premature_ncd_death_prop",
    number_indicator_name == "Prevalence of insufficient physical activity among adults aged 18+ years (age-standardized estimate) (%)" ~ "insuff_activity_adult_std_preval",
    number_indicator_name == "Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol), by beverage type" ~ "alcohol_recorded_type_ltr_by_type",
    number_indicator_name == "Mean HDL cholesterol, crude" ~ "hdl_chol_crude_mean",
    number_indicator_name == "Mean Non-HDL cholesterol, crude" ~ "nonhdl_chol_crude_mean",
    number_indicator_name == "Prevalence of obesity among children and adolescents, BMI > +2 standard deviations above the median (crude estimate) (%)" ~ "youth_obesity_crude_preval",
    number_indicator_name == "Mean HDL cholesterol, age-standardized" ~ "hdl_chol_std_mean",
    number_indicator_name == "Prevalence of obesity among adults, BMI &GreaterEqual; 30 (crude estimate) (%)" ~ "adult_obesity_crude_preval",
    number_indicator_name == "Insufficiently active (crude estimate)" ~ "insuff_activity_crude_rate",
    number_indicator_name == "Prevalence of underweight among adults, BMI < 18 (age-standardized estimate) (%)" ~ "adult_underwt_std_preval",
    number_indicator_name == "Prevalence of overweight among adults, BMI &GreaterEqual; 25 (crude estimate) (%)" ~ "adult_overwt_crude_preval",
    number_indicator_name == "Mean total cholesterol, crude" ~ "total_chol_crude_mean",
    number_indicator_name == "Prevalence of underweight among adults, BMI < 18 (crude estimate) (%)" ~ "adult_underwt_crude_preval",
    number_indicator_name == "Estimate of current tobacco smoking prevalence (%) (age-standardized rate)" ~ "curr_tobacco_smoke_std_preval",
    number_indicator_name == "Prevalence of overweight among adults, BMI &GreaterEqual; 25 (age-standardized estimate) (%)" ~ "adult_overwt_std_preval",
    number_indicator_name == "Total NCD Deaths" ~ "ncd_deaths_total",
    number_indicator_name == "Crude suicide rates (per 100 000 population)" ~ "suicide_crude_rate_100k",
    number_indicator_name == "Prevalence of diabetes, crude" ~ "crude_diabetes_preval",
    number_indicator_name == "Diabetes treatment coverage, age-standardized" ~ "std_diabetes_trtmt_rate",
    number_indicator_name == "Estimate of current cigarette smoking prevalence (%) (age-standardized rate)" ~ "curr_cig_smoke_std_preval",
    number_indicator_name == "Alcohol, unrecorded per capita (15+) consumption (in litres of pure alcohol), three-year average" ~ "unrec_alcohol_consum_Ltr_3yr_avg",
    number_indicator_name == "Mean Non-HDL cholesterol, age-standardized" ~ "nonhdl_chol_std_mean",
    number_indicator_name == "Mean total cholesterol, age-standardized" ~ "total_chol_std_mean",
    number_indicator_name == "Estimate of current tobacco use prevalence (%) (age-standardized rate)" ~ "curr_tobacco_use_std_preval",
    number_indicator_name == "Probability (%) of dying between age 30 and exact age 70 from any of cardiovascular disease, cancer, diabetes, or chronic respiratory disease" ~ "death_risk_30_70_ncd",
    number_indicator_name == "Estimate of current tobacco smoking prevalence (%)" ~ "curr_tobacco_smoke_preval",
    number_indicator_name == "Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol), three-year average" ~ "rec_alcohol_consum_Ltr_3yr_avg",
    number_indicator_name == "Alcohol, total per capita (15+) consumption (in litres of pure alcohol) (SDG Indicator 3.5.2), three-year average" ~ "total_alcohol_consum_Ltr_3yr_avg",
    number_indicator_name == "Estimate of current cigarette smoking prevalence (%)" ~ "curr_cig_smoke_preval",
    number_indicator_name == "Estimate of current tobacco use prevalence (%)" ~ "curr_tobacco_use_preval",
    number_indicator_name == "Vision and eyecare: Effective refractive error coverage (eREC) (%)" ~ "vision_erec_coverage",
    number_indicator_name == "Prevalence of cervical cancer screening among women aged 30-49 years (%)" ~ "fem_30_49_cervical_screen",
    number_indicator_name == "Monitoring tobacco use and prevention policies" ~ "tobacco_policy_monitor",
    number_indicator_name == "Mean total cholesterol,  age-standardized" ~ "std_mean_total_chol",
    number_indicator_name == "Mean total cholesterol,  crude" ~ "crude_mean_total_chol",
    number_indicator_name == "Prevalence of cervical cancer screening  among women aged 30-49 years (%)" ~ "women_30to49_cerv_cancer_screen_preval"
  ))

#Remove unnecessary columns
noncom <- noncom |> 
  select(-c(number_indicator_code, number_indicator_url,
            number_region_code, number_region_name,
            number_country_code, number_country_name))
noncom <- noncom |> 
  rename(indicator_name = number_indicator_name,
         year = number_date_year,
         year_start = number_date_year_start,
         year_end = number_date_year_end,
         dimension_type = number_dimension_type,
         dimension_code = number_dimension_code,
         dimension_name = number_dimension_name,
         indicator_value_ = number_indicator_value_num,
         ci_lower = number_indicator_value_low,
         ci_upper = number_indicator_value_high
         )

unique(noncom$dimension_type)

# Sex dimension
sex_dim <- noncom |> 
  filter(dimension_type == "SEX") |> 
  rename(sex = dimension_name) |> 
  mutate(sex = case_when(
    sex == "Male" ~ "Male",
    sex == "Female" ~ "Female",
    sex == "Both sexes" ~ "Both"
  )) |> 
  select(-c(dimension_type, dimension_code, number_indicator_value))
summary(sex_dim)

# Missing confidence intervals for 22 years
missings <- sex_dim |> 
  filter(is.na(ci_lower) & is.na(ci_upper)) |> 
  arrange(year)

# Q:For which indicators were these C.Is not reported?
unique(missings$indicator_name)
#A: premature_ncd_death_prop and suicide_crude_rate_100k

summary(missings)

years <- sex_dim |> 
  group_by(year) |> 
  summarize(count = n()) |> 
  arrange(year)

# Alcohol Type dimension
alcohol <- noncom |> 
  filter(dimension_type == "ALCOHOLTYPE") |>
  arrange(year) |> 
  select(-c(dimension_type, dimension_code, number_indicator_value))
unique(alcohol$indicator_name)


calc_type <- noncom |> 
  filter(dimension_type == "CALCULATIONTYPE") |> 
  select(-number_indicator_value)


filtered_sex_dim <- sex_dim |>
  filter(
    # Keep rows where indicator name is a standardised prevalence
    str_detect(indicator_name, "std.*preval|preval.*std") |
      # OR indicators that ain't crude
      (str_detect(indicator_name, "preval") & !str_detect(indicator_name, "crude"))
  )

# Verifying
filtered_sex_dim |>
  distinct(indicator_name) |>
  arrange(indicator_name)




