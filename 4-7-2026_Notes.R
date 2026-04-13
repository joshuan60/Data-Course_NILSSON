library("readxl")
library("tidyverse")
library("janitor")
library("skimr")
library("ggplot2")
library("measurements")
library("easystats")
# Build the best model to predict cty$mpg
cars <- mpg
?glm()
test <- glm(cty ~ displ + cyl + displ:year + model, data = cars)
report(test)
performance(test)
predict(test, cars)
summary(test)


modeltest2 <- glm(cty ~ displ + trans, data = cars)
modeltest3 <- glm(cty ~ displ:trans, data = cars)
modeltest4 <- glm(cty ~ displ:trans + model, data = cars)
compare_performance(test, modeltest2, modeltest3, modeltest4)

library(MASS)
CorrectTest <- stepAIC(test) # This is going to test all possible models and spit out the best
CorrectTest$formula

# As an alternative you could write this to test all combos
test2 <- glm(cty ~ ., data = cars)
CorrectTest2 <- stepAIC(test2)
CorrectTest2$formula

# Making combinations (Maybe don't run this; crashes computer :()
# test3 <- glm(cty ~ .^2, data = cars)
# CorrectTest3 <- stepAIC(test3)
# CorrectTest3$formula

# Looking at Binomial Distributions
# Does penguin mass vary between species 
library(palmerpenguins)
littledudes <- penguins
littledudes$species = relevel(littledudes$species, ref = "Gentoo")

mod_littledudes = glm(data = littledudes, formula = body_mass_g ~ species)
summary(mod_littledudes)
littledudes %>%
  ggplot(aes(x = species, y = body_mass_g, color = island))

# Build a model on whether a bird is Gentoo or not? This is a binomial distribution
# y = Yes/No (0/1 or T/F)
View(littledudes)
littledudes2 = littledudes %>% 
  mutate(gentoo = case_when(species == "Gentoo" ~ TRUE, 
                            TRUE ~ FALSE))
mod_gentoo = glm(data = littledudes2, formula = gentoo ~ bill_depth_mm + 
      bill_length_mm, family = "binomial")
summary(mod_gentoo)

predict(mod_gentoo, littledudes2) 
littledudes2$pred = predict(mod_gentoo, littledudes2, type = "response") # This is the probability of the end results based on our model

littledudes2 %>% 
  ggplot(aes(x = body_mass_g, y = pred, color = Species)) +
  geom_point()
