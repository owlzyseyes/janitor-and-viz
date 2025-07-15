library(tidyverse)


all_accidents <- readr::read_csv("data/ma3route_crashes_algorithmcode.csv")

str(all_accidents)

all_accidents <- all_accidents |> 
  rename(
    crash_time = crash_datetime,
  is_fatal = contains_fatality_words,
pedestrians_involved = contains_pedestrian_words,
ma3_involved = contains_matatu_words,
boda_involved = contains_motorcycle_words) |> 
  mutate(year = lubridate::year(crash_date))

readr::write_csv(all_accidents, "data/all_accidents.csv")
