
# Libraries ---------------------------------------------------------------
library(tidyverse); theme_set(theme_minimal());
library(gganimate)
library(tidytuesdayR)
library(lubridate)
library(janitor)


# Import and Export -------------------------------------------------------

energy_types <- read_csv("energy_types.csv") %>% clean_names()

# Data Wrangling ----------------------------------------------------------

# Change levels and type to factor
energy_types$level <- energy_types$level %>% factor() 
energy_types$type <- energy_types$type %>% factor() 

# Pivoting
energy_types %>% pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "total") -> energy_types
# Fixing year type
energy_types %>% mutate(year = as.integer(substring(year,2))) -> energy_types

# Cleaning up NA's
energy_types$country_name <- energy_types$country_name %>% replace_na("United Kingdom")


# Visualization -----------------------------------------------------------

energy_types %>% 
  mutate(cy = paste0(country_name," ",year)) %>% 

  ggplot() +
  geom_col(aes(x = type, y = total, fill = type)) +

  # Flipping Coordinates  
  coord_flip() +

  # Themeing
  theme(

    legend.position = "none",
    text = element_text(family = "Roboto", color = "white"),

    axis.text = element_text(color = "#7889DE"),

    plot.title = element_text(size = 30),

    panel.background = element_rect(fill = "#2B2E4F"),

    plot.background = element_rect(fill = "#212138"),

    axis.title.x = element_blank(),

    panel.grid = element_blank(),

    panel.grid.major.x = element_line(color = "#393750")
  ) +

  scale_fill_brewer(palette="Spectral") +

  scale_y_continuous(labels = scales::comma) +

  # Labels and Captions
  ## For some reason the X label get's ignored ?
  xlab("Energy Source") +
  ylab("Ouput (GWh)") +
  labs(caption = "@Khanzi_w on Twitter for #TidyTuesdays | Data: Eurostat") +
  
  # Animation
  transition_states(
    cy,
    transition_length = 4,
    state_length = 2
  ) +
  enter_grow() + exit_shrink() +aes('cubic-in-out') +
  
  # Title
  labs(title = "{closest_state}") -> p

# A this point you'd render `p` 