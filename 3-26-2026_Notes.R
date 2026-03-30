library("readxl")
library("tidyverse")
library("janitor")
library("skimr")
library("ggplot2")
library("measurements")
library("easystats")
cars <- mpg
View(mpg)

# Does the displ effect cty?
# (Hint: 1. plot out and examine; 2. statistical test)
?summarise
cars %>% 
  ggplot(aes(x = displ, y = cty, fill = manufacturer)) + #y-dependent variable
  geom_point()
# There appears to be a negative correlation. Now we need to check if it is significant 
# checking for normality
shapiro.test(cars$cty)

# Running a correlation test. 
cort <- cor.test(cars$displ, cars$cty)

# How much does displ affect cty? 
?glm()
test <- glm(cty ~ displ, data = cars) # y ~ x syntax
str(test)
summary(test) 
# y = mx + b
# cty <- a*displ + b
# cty = (-2.63)*displ +25.99

cars %>% 
  ggplot(aes(x = displ, y = cty)) + #y-dependent variable
  geom_point() +
  geom_smooth(method = "glm") +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
  annotate("text", x = 5, y = 30,
           label = "cty = (-2.63)*displ +25.99",
           size = 3)
test$coefficients # Gives intercept and slope
test$fitted.values # based on the model, these are the results for cty when plugging in the displ
# cty = (-2.63)*displ +25.99
# displ = 1.8

cty = 18
fitted = 21.256600
cty - fitted
# The "easystats" package
test$residuals # The differences between the prediction and reality
# Checking the fit of the models
performance(test)
# This gives you a report 
report(test)
report(cort)
# check model
check_model(test)

# Build a better model for "cty"
test2 <- glm(cty ~ displ + year + model, data = cars)
performance(test2)
report(test2)
test3 <- glm(cty ~ displ + year + model + manufacturer + cyl + trans, data = cars)

# Comparing multiple performance reports
compare_performance(test, test2, test3)
compare_performance(test, test2, test3) %>% plot()

# Compare your new model with the original model
# My initial model is only good for the BIC (weights), while the following models 
# are good for different things, but also have their drawbacks. 

