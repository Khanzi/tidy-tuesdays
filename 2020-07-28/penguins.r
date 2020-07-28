
# Libraries ---------------------------------------------------------------
setwd("~/Documents/tidy-tuesdays/2020-07-28")
library(tidyverse); theme_set(theme_minimal())
library(ggbeeswarm)
library(janitor)
library(caret)
library(cowplot)
library(ggpubr)
library(png)


# Getting the data --------------------------------------------------------


penguins <- read_csv('penguins.csv') 





# Initial Visualization ---------------------------------------------------

# there are 11 penguins with unknown sex

# Gender vs bill size for species
## Adelie
penguins %>% 
  filter(species == "Adelie") %>% 
  ggplot() +
  geom_point(aes(bill_depth_mm, bill_length_mm, color = sex, alpha = 0.1)) +
  
  # Labels
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  ylab("Bill Length (mm)") +
  
  
  # Annotation
  
  ## Drawing a line through gender separation
  annotate("segment",
           x = 16.4, xend = 21, y = 43.4, yend = 34,
           color = "black",
           linetype = 2)  +
  
  ## Labeling Line
  annotate("text",
           x = 16.5, y = 44.5,
           label = "gender separation",
           color = "black") +
  
  ## Labeling male portion
  annotate("text",
           x = 20 , y = 44,
           label = "males",
           color = "black",
           size = 5) +
  

  ## Labeling female portion
  annotate("text",
           x = 16 , y = 34,
           label = "females",
           color = "black",
           size = 5) -> p1

## Gentoo
penguins %>% 
  filter(species == "Gentoo") %>% 
  ggplot() +
  geom_point(aes(bill_depth_mm, bill_length_mm, color = sex, alpha = 0.1)) +
  
  # Labels
  theme(legend.position = "none") +
  theme(axis.title = element_blank()) + 
  
  # Annotation
  
  ## Drawing a line through gender separation
  annotate("segment",
           x = 14, xend = 15.4, y = 53.4, yend = 43,
           color = "black",
           linetype = 2)  +
  
  ## Labeling Line
  annotate("text",
           x = 14, y = 55,
           label = "gender separation",
           color = "black") +
  
  ## Labeling male portion
  annotate("text",
           x = 16.5 , y = 55,
           label = "males",
           color = "black",
           size = 5) +
  

  ## Labeling female portion
  annotate("text",
           x = 13.8 , y = 43,
           label = "females",
           color = "black",
           size = 5) -> p2


## Chinstrap (Pygoscelis antarctica)
penguins %>% 
  filter(species == "Chinstrap") %>% 
  ggplot() +
  geom_point(aes(bill_depth_mm, bill_length_mm, color = sex, alpha = 0.1)) +
  
  # Labels
  theme(legend.position = "none") +
  theme(axis.title = element_blank()) +
  
  # Annotation
  
  ## Drawing a line through gender separation
  annotate("segment",
           x = 17, xend = 19, y = 53.4, yend = 43,
           color = "black",
           linetype = 2) +
  
  ## Labeling Line
  annotate("text",
           x = 17, y = 55,
           label = "gender separation",
           color = "black") +
  
  ## Labeling male portion
  annotate("text",
           x = 20, y = 55,
           label = "males",
           color = "black",
           size = 5) +
  

  ## Labeling female portion
  annotate("text",
           x = 17, y = 43,
           label = "females",
           color = "black",
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
clean$inferred <- as.numeric(clean$inferred)
clean %>% mutate(sex = paste0(sex, '_', inferred)) ->  clean

clean %>% filter(species == "Adelie") %>% 
  ggplot() +
  geom_point(aes(y = bill_length_mm,  x = bill_depth_mm , size = inferred, color=sex, alpha = 0.1)) +
  
  # Labels and Titles
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank()) + 
  ylab("Bill Length (mm)") +
  
  # Annotations
  
  ## Arrow to inferred female
  annotate("curve",
           y = 36, yend = 37.6,
           x = 16.2, xend = 16.9,
           curvature = -0.3,
           arrow = arrow(
              length = unit(2, 'mm'),
              type = 'closed'),
           color = "black"
           ) +
  
  ## Inferred female label
  annotate("text",
           y = 34.3, x=16.5,
           label = "reasonable prediction\nfor female",
           color = "black") +
  
  ## Inferred male arrow
  annotate("curve",
           yend = 42, y = 42.2,
           xend = 20.4, x = 21,
           curvature = -0.3,
           arrow = arrow(length = unit(2,'mm'), type = 'closed'),
           color = "black"
           ) +
  
  ## Inferred male label
  annotate("text",
           y = 44, x = 21,
           label = "reasonable prediction\nfor male",
           color = "black") -> p4

clean %>% filter(species == "Gentoo") %>% 
  ggplot() +
 
  geom_point(aes(y = bill_length_mm,x = bill_depth_mm , size = inferred, color=sex, alpha = 0.1)) +
  
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 20)) + 
  scale_color_manual(values = c("#F8766D", "#7CAE00","#00BFC4" ,"#F8766D")) +
  xlab("Bill Depth (mm)") +
  
  annotate("text",
           label = "incorrect female prediction",
           x = 16.5, y = 40.5) +
  
  annotate("curve",
           x = 16.4, xend = 15.8,
           y = 41, yend = 44.4,
           arrow = arrow(length = unit(2, 'mm'), type = 'closed')) ->p5
  
  # Labels and Titles
ggdraw() + 
  draw_text("No missing data for Chinstraps", size = 30, x = 0.5, y = 0.5) +
  draw_text("@Khanzi_w on Twitter for #TidyTuesday | Data: Dr. Kristen Gorman | Original Art: @alaphair on Instagram",
            size = 10,
            x = 0.5, y = 0.05) -> p6

# -------------------------------------------------------------------------

ggdraw() +
  draw_image("adelie.png") +
  draw_text("Adelie", y = 0.92, size = 34, color = "white") -> adelie_img

ggdraw() +
  draw_image("gentoo.png") +
  draw_text("Gentoo", y = 0.92, size = 34, color = "white") -> gentoo_img

ggdraw() +
  draw_image("chinstrap.png") +
  draw_text("Chinstrap", y = 0.92, size = 34, color = "white") -> chinstrap_img
# Cowplot -----------------------------------------------------------------

ggdraw() +
  draw_text("Predicting Sex | Palmer Penguins", x= 0.5, y = 0.85, size = 40) +
  draw_text("LDA Model (Accuracy = 0.91, 10-fold cross validated)", x = 0.5, y = 0.60, size = 20) -> header_plot

header <- plot_grid(header_plot, ncol=1)
image_row <- plot_grid(adelie_img, gentoo_img, chinstrap_img, ncol=3)
row1 <- plot_grid(p1,p2,p3, ncol=3)
row2 <- plot_grid(p4,p5,p6, ncol=3)

plot_grid(
          header, 
          image_row,
          
          row1,
          
          row2,
          
          rel_heights = c(1.5,2,2.5,2.5),
          nrow = 4
)
          
          

# TODO: Fix annotation; make them darker./ DONE :)
# TODO: Add title and description
# TODO: Clean up legends 
# TODO: Tidy up theme

  