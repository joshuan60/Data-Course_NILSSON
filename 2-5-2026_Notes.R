# Plot how many penguins observed on each island
# and their species
# (bonus) how many of them are female, male, etc. 
library('palmerpenguins')
library('tidyverse')
library("ggplot2")
plotpen <- penguins %>% 
  group_by(sex, species, island) %>% 
  summarize(number_of_penguins = n()) %>% 
  # arrange(desc(island)) %>% 
  ggplot(aes(x = island, 
             y = number_of_penguins, 
             fill = species)) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_wrap(~ sex)

plotpen + facet_grid(sex~ island) # facet_wrap in multiple dimensions

# Testing values in vectors
vec = c(1,2,3, NA, 5, NA)
is.na(vec)
!is.na(vec) # ! is not NA?
!is.na(penguins$sex)

# Removing the NA values from the filter
penguins %>% 
  filter(!is.na(sex)) %>% 
  View()

# Are penguins with bigger flippers  heavier? 
# Is there a difference between species and sex? 
# make a plot to show that (remove the NA values for sex)
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = flipper_length_mm, 
             y = body_mass_g,
             color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Relationship between Flipper Length & Weight", 
       x = "Flipper (mm)", y = "Weight (g)",
       color = "Breed") +
  scale_colour_viridis_d(end = 0.5) +
  scale_x_continuous(limits = c(150, 250), expand = c(0,0)) +
  scale_y_continuous(limits = c(2500, 7000), expand = c(0,0)) +
  stat_ellipse()
  # ggsave('my_plot.png', width = 10, height = 15, dpi = 300) If I want to save the plot
# you can edit colors manually 
# scale_color_manual(values = c('Chinstrap' ="lightblue",'Gentoo' = orange", 'Adelie' = "green"))

# resolution = w*h*dpi
# (3000 x 4500) 
# (10x300 15x300)



