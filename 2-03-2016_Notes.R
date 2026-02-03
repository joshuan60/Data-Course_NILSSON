# Using penguin data
# Add a new col (weight_status)
# for penguins with a mass more than 5000g -->
# for penguins with a mass less than or equal to 5000g 
# and more than 3000 g -->
# for penguin wight less than or equal to 3000 g -->
library("palmerpenguins")
library('tidyverse')
library("ggplot2")
library("viridis")


penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>% 
  summarize(avg_body_mass = mean(body_mass_g),gender_number = n())
updatedpen <- penguins %>% 
  mutate(Weight_status = case_when(body_mass_g > 5000 ~ "Overweight", 
                                   body_mass_g < 3000 ~ "Underweight", 
                                   body_mass_g < 5000 & body_mass_g > 3000 ~ "Healthy")) 
# Graphing in R
# Basic Graphing Functions
plot()
hist()
barplot()
boxplot()

names(updatedpen)
plot(updatedpen$bill_length_mm, updatedpen$body_mass_g)

# aesthetic graphs in ggplot2
ggplot(aes(x = updatedpen$bill_length_mm,
           y = updatedpen$body_mass_g),
       data = penguins) +
  geom_point()

updatedpen %>% 
  ggplot(aes(x = updatedpen$bill_length_mm,
             y = updatedpen$body_mass_g,
             color = sex,
             shape = species)) +
  geom_point() +
  theme_minimal()

# take a look at your new penguin data and make a cool graph
# Plot
updatedpen %>%
  ggplot(aes(x=island, y=flipper_length_mm, fill=sex)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Flipper Length by Island") +
  xlab("")