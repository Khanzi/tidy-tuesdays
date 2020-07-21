
# Libraries ---------------------------------------------------------------

library(tidyverse); theme_set(theme_minimal())
library(janitor)
library(patchwork)



# Data Import -------------------------------------------------------------

tidy_tuesday <- tidytuesdayR::tt_load('2020-07-21')

animal_outcomes <- tidy_tuesday$animal_outcomes
rm(tidy_tuesday)


# Animal Outcomes ---------------------------------------------------------
animal_outcomes %>% clean_names() -> animal_outcomes

# Cats and Dog Outcomes over the years
animal_outcomes %>% 
  
  # Filtering for only cats and dogs
  filter(animal_type %in% c('Cats', 'Dogs')) %>% 
  group_by(year, outcome) %>% 
  summarise(year = mean(year), total = sum(total)) %>% 
  
  ggplot(aes(x = year, y = total, color = outcome)) +
  geom_line() + # Drawing line
  geom_point(aes(size = total, alpha = 0.001)) + # Adding count based circles(points)
  
  # Curved arrow pointing to interesting datapoint about rehoming/euthanasia
  annotate('curve',
           x = 2005.8, xend = 2010.8,
           y = 45000, yend = 39300,
           curvature = -0.3,
           size = 0.3,
           arrow = arrow(length = unit(2,'mm'), type = 'closed')) +
  annotate('text',
           x = 2004, y = 45000,
           label= 'More pets started getting \n rehomed opposed to euthanasia',
           size = 3) +
  
  theme(legend.position = 'none') + # Getting rid of the legend
  
  # Labels
  labs(subtitle = 'Cats and Dogs', color = NULL) +
  xlab('Year') + ylab('Count') -> p1
  

# Animal outcomes by type
animal_outcomes %>%
  # Filtering for only cats and dogs
  filter(animal_type %in% c('Dogs', 'Cats')) %>% 
  group_by(animal_type, outcome) %>% 
  summarise(total = sum(total)) %>% 
  ggplot(aes(x = animal_type, y = (total), fill = outcome)) +
  geom_bar(position='dodge', stat = 'identity') + 
  coord_flip() + # Flipping the bar chart on the side
  
  # Cats are getting euthanized more than any other animal
  # Drawing a curve to point at cat euthanasia number
  annotate('curve', x = 2.7, y = 4e+05, yend = 4.9e+05, xend = 0.9,
           curvature = -0.3,
           size = 0.3,
           arrow = arrow(length = unit(2, 'mm'), type = 'closed')) +
  
  annotate('text', x = 3, y = 4e+05,size = 3,
           label = ("\nCat's are the most frequently euthanized animal.")) + 
  
  
  
  # Not many cats get reclaimed
  # Curved arrow pointing at cats reclaimation number
  annotate('curve', x = -0.1, y = 3e+4, yend = -5e+3, xend = 1.1,
           curvature = -0.5,
           size = 0.3,
           arrow = arrow(length = unit(2, 'mm'))) +
  
  annotate('text', x = -0.2, y = 1.3e+5,size = 3,
           label = 'Not many cats are reclaimed \n') + 
  
  # Labels and title
  labs(subtitle = 'Cats and Dogs', fill = "Outcome") +
  xlab('Animal Type') + ylab('Count') -> p2


animal_outcomes %>% 
  # Filtering out cats and dogs to look at outcomes for other animals
  filter(animal_type !='Cats' & animal_type != 'Dogs') %>% 
  group_by(animal_type, outcome) %>% 
  summarise(total = sum(total)) %>% 
  ggplot(aes(x = animal_type, y = (total), fill = outcome)) +
  geom_bar(position='dodge', stat = 'identity') + 
  coord_flip() + # Flipping the coordinates of the bar chart
  
  # So much wildlife is euthanized
  # Curved arrow pointing at the euthanasia numbers for wildlife :(
  annotate('curve',
           x = 3.3, xend = 3.6,
           y = 150000, yend = 150500,
           curvature = 0.1,
           arrow = arrow(length = unit(1, 'mm'), type = 'closed')) + 
  annotate('text',
           size = 3,
           y = 130000, x = 3,
           label = "A disproportionate amount \nof wildlife is euthanized") +
  
  # Legend
  theme(legend.position = 'none') + # Hiding the legend
  
  # Labels
  labs(subtitle = 'Other Animals') +
  xlab('Animal Types') + ylab('Count') -> p3
  

# Playing with patchwork

patch <- ((p1) / (p2 + p3)) + # Defining pathwork layout
  
  plot_layout(guides = 'collect') + # Collecting guides on the right side
  
  # Annotations
  plot_annotation(
    title = "Animal Outcomes",
    caption = "@Khanzi_W on #TidyTuesday. Data from RSPCA Australia") &
  
  # Small theme tweaks
  theme(plot.title = element_text(size = 30, hjust=0.5), # Centering title and changing size
        panel.grid = element_line(color = 'white'), # Hiding gridlines
        text = element_text(family = 'Avenir Book')) # Changing font
 
patch