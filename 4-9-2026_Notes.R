# Using penguins data build a model to predict if a penguin is Gentoo
# If a penguin is Gentoo or not (Y/N)
library("readxl")
library("tidyverse")
library("janitor")
library("skimr")
library("ggplot2")
library("measurements")
library("easystats")
library("palmerpenguins")
littledudes <- penguins
littledudes2 <- littledudes %>% 
  mutate(gentoo = case_when(species == "Gentoo" ~ T, TRUE ~ FALSE))


predict_gentoo <- glm(gentoo ~ body_mass_g + flipper_length_mm, family = binomial, data = littledudes2)
summary(predict_gentoo)
littledudes2$pred <-predict(predict_gentoo, littledudes2, type = "response")

littledudes2 %>% 
  ggplot(aes(x = bill_length_mm, y = pred,
             color = species)) +
  geom_point()

# Checking how well the model accurately predicts your data
View(littledudes2)
# First set a threshold for the amount of prediction data that is your y-value
evaluation <- littledudes2 %>% 
  mutate(outcome = case_when(pred > 0.75 ~ "Gentoo", 
                             pred < 0.25 ~ "Not Gentoo", 
                             TRUE ~ "Uncertain")) %>% 
  select(species, outcome) %>% 
  mutate(matches = case_when(species == "Gentoo" & outcome == "Gentoo" ~ TRUE,
                             species != "Gentoo" & outcome == "Not Gentoo" ~ TRUE,
                             TRUE ~ FALSE)) 
table(evaluation$species)
# Route 2
evaluation2 <- littledudes2 %>% 
  mutate(outcome = case_when(pred > 0.75 ~ "Gentoo", 
                             pred < 0.25 ~ "Not Gentoo", 
                             TRUE ~ "Uncertain")) %>% 
  select(species, outcome) %>% 
  mutate(matches = case_when(species == "Gentoo" & outcome == "Gentoo" ~ TRUE,
                             species != "Gentoo" & outcome == "Not Gentoo" ~ TRUE,
                             TRUE ~ FALSE)) %>% 
  pluck("matches") 
table(evaluation2)
# Route 3
evaluation3 <- littledudes2 %>% 
  mutate(outcome = case_when(pred > 0.75 ~ "Gentoo", 
                             pred < 0.25 ~ "Not Gentoo", 
                             TRUE ~ "Uncertain")) %>% 
  select(species, outcome) %>% 
  mutate(matches = case_when(species == "Gentoo" & outcome == "Gentoo" ~ TRUE,
                             species != "Gentoo" & outcome == "Not Gentoo" ~ TRUE,
                             TRUE ~ FALSE)) %>% 
  pluck("matches") %>% 
  sum()/nrow(littledudes2)

# Use this data to predict grad school admission
dat_ad = read.csv("Data/GradSchool_Admissions.csv")
View(dat_ad)
admission <- glm(as.logical(admit) ~ gre + rank + gpa, data = dat_ad, family = "binomial")
summary(admission)
dat_ad$pred <-predict(admission, dat_ad, type = "response")
dat_ad %>% 
  ggplot(aes(x = gpa, y = pred,
             color = factor(rank))) +
  geom_point() +
  geom_smooth()


View(mpg)
library(caret) # Creating two data sets to create a model and error check
cars <- mpg
mod_joshua<- glm(cty ~ displ:trans + model, data = cars)
# createDataPartition()

id = createDataPartition(mpg$cty, p = 0.8, list = F)

dat_train = mpg[id, ] # Training data
dat_test = mpg[-id, ] # Testing data

mod_mpg_train = glm(data = dat_train,
                    formula = mod_joshua$formula)

dat_test$pred = predict(mod_mpg_train, dat_test)

dat_test %>% 
  mutate(error = abs(pred - cty)) %>% 
  pluck("error") %>% 
  summary()
