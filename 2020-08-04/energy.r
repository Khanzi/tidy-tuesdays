
# Libraries ---------------------------------------------------------------
setwd("~/Documents/tidy-tuesdays/2020-08-04")
library(tidyverse); theme_set(theme_minimal());
library(gganimate)
library(tidytuesdayR)
library(lubridate)
library(janitor)


# Import and Export -------------------------------------------------------

energy_types <- read_csv("energy_types.csv") %>% clean_names()

country_totals <- read_csv("country_totals.csv") %>% clean_names()


# What are levels? --------------------------------------------------------

energy_types$level <- energy_types$level %>% factor() 
energy_types$type <- energy_types$type %>% factor() 


# Data Wrangling ----------------------------------------------------------

energy_types %>% pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "total") -> energy_types
energy_types %>% mutate(year = as.integer(substring(year,2))) -> energy_types


# Visualization -----------------------------------------------------------

energy_types %>% 
  mutate(cy = paste0(country_name," ",year)) %>% 
  ggplot() +
  geom_col(aes(x = type, y = total, fill = type)) +
  transition_states(
    cy,
    transition_length = 2,
    state_length = 3
  ) +
  enter_grow() + exit_shrink() +aes('linear') +
  labs(title = "{closest_state}") -> p

animate(p, nframes=888)
