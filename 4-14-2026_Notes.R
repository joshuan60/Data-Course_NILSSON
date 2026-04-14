library("readxl")
library("tidyverse")
library("janitor")
library("skimr")
library("ggplot2")
library("measurements")
library("easystats")
library("palmerpenguins")
dat <- penguins
mod1 <- glm(data = penguins, formula = body_mass ~ species + bill_len)
summary(mod1)
# Setting up an anova model
mod2 <-aov(data = penguins, 
           formula = body_mass ~ species + bill_len)
summary(mod2)
