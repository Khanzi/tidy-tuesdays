
# Libraries and Directory Setting -----------------------------------------
setwd("~/Documents/tidy-tuesdays/2020-09-15")

library(tidyverse); theme_set(theme_minimal())


# Data Import -------------------------------------------------------------

kids <- read_csv('kids.csv')


# Wrangling ---------------------------------------------------------------

# Changing the different programs to factors
kids$variable <- kids$variable %>% factor()

# This is a simple function to calculate the raw differences
# Probably not the most efficient way to do this but I cba to make it better
var_diffs <- function(var) {
  kids %>% filter(variable == var) %>% 
    filter(year == 1997) %>% 
    pivot_wider(names_from = year, values_from = raw) -> b

  kids %>% filter(variable == var) %>% 
    filter(year == 2016) %>% 
    pivot_wider(names_from = year, values_from = raw) -> a
  
  
  left_join(a,b, by = "state") %>% select(state, `1997`, `2016`) %>% 
    mutate(raw_change = `2016` - `1997`,
           percent_change = ((`2016` - `1997`)/`1997`)) %>% return()
  
}

# Dirty little piece of code for creating an indicator var for positive or negative
# changes in funding. Used for coloring lines in the final graph
edservs_change <- var_diffs('edservs')
edservs_change %>% 
  mutate(pn = case_when(
    raw_change > 0 ~ 1,
    raw_change <= 0 ~ 0
  )) -> edservs_change


kids <- left_join(kids,edservs_change, by = "state")
# The starting year doesn't have a raw change. So this is just cleaning up the join.
kids[kids$year == 1997,]$raw_change <- 0


# Graph -------------------------------------------------------------------


kids %>% filter(variable == 'edservs') %>% 
  
  filter(year == 2016 | year == 1997) %>% 
  ggplot() +
  
  geom_line(aes(x = raw_change, y = state, color = factor(pn)),size = 1.5) +
  
  geom_point(aes(x = raw_change, y = state, color = factor(year)), size = 2.4) +
  
  scale_color_manual(values=c("firebrick","palegreen3","darkgray","black")) +
  
  labs(title = "Change In Public Spending on Education Special Services",
       subtitle  = "Adjusted for Inflation - From 1997 to 2016",
       caption = "@Khanzi_w on Twitter for #TidyTuesday | Data Urban Institute -Joshua Rosenberg") +
  
  # X Axis
  xlab("Change in $1,000's") +
  scale_x_continuous(labels = scales::comma) +
  
  # Y Axis
  ylab("State") +
  
  
  # Theming
  theme(
    
    # Gridlines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "twodash"),
    
    # Title
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 14),
    
    # X axis
    axis.title.x = element_text(size = 20),
    
    # Y axis
    axis.title.y = element_text(size = 20),
    
    # Legend
    legend.position = "none"
    
    ) -> EducationSpendingPlot

# Saving the plot
ggsave("education_spending.png", EducationSpendingPlot, height = 9, width = 16, units = "in")
