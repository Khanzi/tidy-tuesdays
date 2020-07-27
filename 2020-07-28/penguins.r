
# Libraries ---------------------------------------------------------------

library(tidyverse); theme_set(theme_minimal())
library(ggbeeswarm)
library(janitor)
library(caret)
library(cowplot)
library(ggpubr)
library(png)
library(tidytuesdayR)


# Getting the data --------------------------------------------------------

tuesdata <- tt_load('2020-07-28')

penguins <- tuesdata$penguins %>% clean_names()

penguins_raw <- tuesdata$penguins_raw %>% clean_names()

penguins_raw$island <- penguins_raw$island %>% factor()



# Initial Visualization ---------------------------------------------------

# there are 11 penguins with unknown sex

# Gender vs bill size for species
## Adelie
penguins %>% 
  filter(species == "Adelie") %>% 
  ggplot() +
  geom_point(aes(bill_depth_mm, bill_length_mm, color = sex, alpha = 0.1)) +
  
  # Labels
  xlab("Bill Depth (mm)") + ylab("Bill Length (mm)") +
  labs(title = "Bill Sizes vs Gender") +
  labs(subtitle = "Adelie (Pygoscelis adeliae)") + 
  
  
  # Annotation
  
  ## Drawing a line through gender separation
  annotate("segment",
           x = 16, xend = 21, y = 44, yend = 34,
           color = "gray",
           linetype = 2)  +
  
  ## Labeling Line
  annotate("text",
           x = 16.5, y = 44.5,
           label = "gender separation",
           color = "darkgray") +
  
  ## Labeling male portion
  annotate("text",
           x = 20 , y = 44,
           label = "males",
           color = "darkgray",
           size = 5) +
  

  ## Labeling female portion
  annotate("text",
           x = 16 , y = 34,
           label = "females",
           color = "darkgray",
           size = 5) -> p1

## Gentoo
penguins %>% 
  filter(species == "Gentoo") %>% 
  ggplot() +
  geom_point(aes(bill_depth_mm, bill_length_mm, color = sex, alpha = 0.1)) +
  
  # Labels
  xlab("Bill Depth (mm)") + ylab("Bill Length (mm)") +
  labs(title = "Bill Sizes vs Gender") +
  labs(subtitle = "Gentoo (Pygoscelis papua)") + 
  
  
  # Annotation
  
  ## Drawing a line through gender separation
  annotate("segment",
           x = 14, xend = 15.4, y = 53.4, yend = 43,
           color = "gray",
           linetype = 2)  +
  
  ## Labeling Line
  annotate("text",
           x = 14, y = 55,
           label = "gender separation",
           color = "darkgray") +
  
  ## Labeling male portion
  annotate("text",
           x = 16.5 , y = 55,
           label = "males",
           color = "darkgray",
           size = 5) +
  

  ## Labeling female portion
  annotate("text",
           x = 13.8 , y = 43,
           label = "females",
           color = "darkgray",
           size = 5) -> p2


## Chinstrap (Pygoscelis antarctica)
penguins %>% 
  filter(species == "Chinstrap") %>% 
  ggplot() +
  geom_point(aes(bill_depth_mm, bill_length_mm, color = sex, alpha = 0.1)) +
  
  # Labels
  xlab("Bill Depth (mm)") + ylab("Bill Length (mm)") +
  labs(title = "Bill Sizes vs Gender") +
  labs(subtitle = "Chinstrap (Pygoscelis antarctica)") + 
  
  
  # Annotation
  
  ## Drawing a line through gender separation
  annotate("segment",
           x = 17, xend = 19, y = 53.4, yend = 43,
           color = "gray",
           linetype = 2) +
  
  ## Labeling Line
  annotate("text",
           x = 17, y = 55,
           label = "gender separation",
           color = "darkgray") +
  
  ## Labeling male portion
  annotate("text",
           x = 20, y = 55,
           label = "males",
           color = "darkgray",
           size = 5) +
  

  ## Labeling female portion
  annotate("text",
           x = 17, y = 43,
           label = "females",
           color = "darkgray",
           size = 5) -> p3
###




# Modeling gender ---------------------------------------------------------

clean <- penguins %>% select(-c(year, island)) %>% filter(!is.na(flipper_length_mm))

clean$sex <- clean$sex %>% factor()
clean$species <- clean$species %>% factor()

clean %>% mutate(inferred = is.na(sex)) -> clean
x_names <- c("species","bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")

y_names <- c("sex")

## Preparing testing
set.seed(1234)
control <- trainControl(method='cv', number=10)
metric <- "Accuracy"

## Logistic
clean_no_na <- clean %>% drop_na() %>% select(x_names, sex)
fit.glm <- train(sex~., data = clean_no_na, method='lda', metric=metric, trControl = control)
fit.glm 

## Predicting
needs_prediction <- clean %>% filter(inferred == TRUE )
inferred_sex <- predict(fit.glm, needs_prediction[,x_names])
clean[clean$inferred == TRUE,]$sex <- inferred_sex
clean


# Plotting inferred vars --------------------------------------------------

clean %>% filter(species == "Adelie") %>% 
  ggplot() +
  geom_point(aes(bill_length_mm, bill_depth_mm , size = inferred, color=sex, alpha = 0.1, shape = inferred)) +
  
  # Labels and Titles
  xlab("Bill Length (mm)") + ylab("Bill Depth (mm)") +
  labs(title = "Prediction for Sex", subtitle = "Adelie Penguins") +
  
  
  # Annotations
  
  ## Arrow to inferred female
  annotate("curve",
           x = 32.3, xend = 33.5,
           y = 17, yend = 18,
           curvature = -0.3,
           arrow = arrow(
              length = unit(2, 'mm'),
              type = 'closed'),
           color = "gray"
           ) +
  
  ## Inferred female label
  annotate("text",
           x = 33.3, y=16.5,
           label = "reasonable prediction\nfor female",
           color = "gray") +
  
  ## Inferred male arrow
  annotate("curve",
           xend = 42.4, x = 44,
           yend = 20.1, y = 20.5,
           curvature = -0.3,
           arrow = arrow(length = unit(2,'mm'), type = 'closed'),
           color = "gray"
           ) +
  
  ## Inferred male label
  annotate("text",
           x = 44, y = 21,
           label = "reasonable prediction\nfor male",
           color = "gray") -> p4

clean %>% filter(species == "Gentoo") %>% 
  ggplot() +
  geom_point(aes(bill_length_mm, bill_depth_mm , size = inferred, color=sex, alpha = 0.1, shape = inferred)) +
  
  # Labels and Titles
  xlab("Bill Length (mm)") + ylab("Bill Depth (mm)") +
  labs(title = "Prediction for Sex", subtitle = "Gentoo Penguins") -> p5

ggdraw() + draw_text("No missing sex data for Chinstraps", size = 24) -> p6

# -------------------------------------------------------------------------

ggdraw() +
  draw_image("adelie.png") +
  draw_text("Adelie", y = 0.93, size = 40, color = "white") -> adelie_img

ggdraw() +
  draw_image("gentoo.png") +
  draw_text("Gentoo", y = 0.93, size = 40, color = "white") -> gentoo_img

ggdraw() +
  draw_image("chinstrap.png") +
  draw_text("Chinstrap", y = 0.93, size = 40, color = "white") -> chinstrap_img
# Cowplot -----------------------------------------------------------------

ggdraw() +
  draw_text("Title", y =0.9, x = 0.1, size = 40) +
  draw_text("subtitle", y = 0.8, x = 0.11, size = 30)


plot_grid(
          adelie_img, gentoo_img, chinstrap_img,
  
          p1,p2,p3,
          p4, p5, p6,
          
          
          ncol = 3) 

# TODO: Fix annotation; make them darker
# TODO: Add title and description
# TODO: Clean up legends 
# TODO: Tidy up theme

  