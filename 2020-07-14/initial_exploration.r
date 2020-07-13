# Libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse); theme_set(theme_minimal())
library(janitor)


# Importing the data ------------------------------------------------------

tt <- tt_load('2020-07-14')
astronauts <- tt$astronauts

# This week they also recommended that we use the 2019-01-15 data
tt <- tt_load('2019-01-15')
agencies <- tt$agencies
launches <- tt$launches



# Inspecting Data ---------------------------------------------------------

View(astronauts)
View(agencies)


# Space Dominance ---------------------------------------------------------
# Which country has the most prevalent space programs

astronauts %>% group_by(nationality) %>% 
  summarise(missions = sum(total_number_of_missions)) %>% 
  arrange(desc(missions)) %>% head(7) %>% 
  ggplot(aes(x = nationality, y = missions)) + geom_col() + 
  geom_text(aes(label = missions),hjust = -0.25,  position = position_dodge(0.9)) + coord_flip() +
  labs(title = 'Countries with Most Launches')

# Compositions
astronauts %>% group_by(nationality, occupation) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  spread(occupation, n, fill=0) %>% clean_names() -> occupations_counts_by_country
# I will get back on this
astronauts$occupation <- str_to_lower(astronauts$occupation)
astronauts %>% ggplot(aes(occupation, fill = nationality)) + 
  geom_bar() + coord_flip() +
  labs(title = 'Occupations by Country') + 
  xlab('Counts') + ylab('Occupations')

# EVA ---------------------------------------------------------------------
astronauts %>% group_by(nationality) %>% summarise(EVA_Hours = sum(eva_hrs_mission)) %>% 
  arrange(desc(EVA_Hours)) %>% head(10) %>% 
  ggplot(aes(x = nationality, y = EVA_Hours)) + geom_col() +
  labs(title = 'Top EVA Hours' ) + xlab('Country') + ylab('Hours')


# Mission by year ---------------------------------------------------------
astronauts %>% group_by(nationality, year_of_mission) %>% summarise(n = n()) -> missions_by_year_country

total_missions <- astronauts %>% group_by(nationality) %>% summarise(total_missions = sum(total_number_of_missions))

total_missions %>% arrange(desc(total_missions)) %>% head(7) %>% select(nationality) -> top_countries 

missions_by_year_country %>% filter(nationality %in% top_countries$nationality) %>% 
  ggplot(aes(x = year_of_mission, y = n, color = factor(nationality))) + geom_line() +
  guides(color=guide_legend(title='Country')) + 
  labs(title = 'Missions by Year', subtitle = 'for Top Countries') +
  xlab('Year') + ylab('Number of missions')


# Most popular  vehicle ---------------------------------------------
astronauts %>% filter(nationality %in% top_countries$nationality) %>% 
  mutate(a_shuttle = str_extract(ascend_shuttle, '^[a-zA-Z0-9]*')) %>% 
  mutate(orbit_shuttle = str_extract(in_orbit, '^[a-zA-Z0-9]*')) %>% 
  mutate(d_shuttle = str_extract(descend_shuttle, '^[a-zA-Z0-9]*')) -> astronauts 

astronauts %>% mutate(same_vehicle = 
                        (orbit_shuttle == d_shuttle) & (d_shuttle == a_shuttle)) -> astronauts
astronauts %>% 
  ggplot(aes(same_vehicle)) + 
  geom_bar() + 
  geom_text(stat = 'count',aes(label=..count..), vjust=-1) +
  labs(title = 'Same Vehicles for Mission') +
  ylab('Count')

astronauts %>% 
  ggplot(aes(a_shuttle)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) +
  labs(title = 'Overall Most Popular Vehicles', caption = 'Assuming most common case of same vehicle') +
  xlab('Vehicle Name') + ylab('Astronauts Carried')


