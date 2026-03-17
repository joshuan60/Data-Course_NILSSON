# Make a cool plot using penguin data (make sure there are no N/A)
# manually set the color as ugly as you can make it. 
# add title, fix axis labels
# save to local directory
library('palmerpenguins')
library('tidyverse')
library("ggplot2")
penplot <- penguins %>% 
  group_by(sex, species, island) %>% 
  summarize(number_of_penguins = n()) %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = island, 
             y = number_of_penguins, 
             fill = species)) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_wrap(~ sex) +
  labs(title = "Number of Penguins by Species by Island", 
       x = "Island", y = "Number of Penguins",
       fill = "Species") +
# element_text(angle, face, size)
# ggsave('my_plot.png', plot = penplot, width = 10, height = 15, dpi = 300) 
penplot + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic", size =10)) +
  theme(axis.text = element_text(angle = 45, size = 16)) 
  
# Putting it all together
penplot + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic", size =10),
  axis.text = element_text(angle = 45, size = 12))

# Coloring species locally rather than globally
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth()
# Coloring species globally
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g,
             color = species)) +
  geom_point(alpha = 0.5, size = 10)

#Example of utilizing global traits to customize more
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g,
             color = species)) +
  geom_point(color = "black") +
  geom_smooth()

# Reading a csv file in a subdirectory
setwd("Data")
read.csv("wide_income_rent.csv")

# Make a plot to show penguin weight change across 3 years
penguins %>% 
  ggplot(aes(x = as.factor(year),
             y = body_mass_g,
             color = species)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, aes(color = species))

str(penguins$year)

# Assignment 4 Notes

