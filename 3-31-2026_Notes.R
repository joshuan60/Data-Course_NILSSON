library("readxl")
library("tidyverse")
library("janitor")
library("skimr")
library("ggplot2")
library("measurements")
library("easystats")
# Build a model to predict cty as a function of displ
# mpg dataset
cars <- mpg
test <- glm(cty ~ displ, data = cars)
summary(test)
report(test)
performance(test)

predict(test, cars) # this takes our model and our data and then predicts the values 
# in the model.

test$fitted.values[1] #This spits out our predicted values

plot(predict(test, cars), test$fitted.values)

predict(test, data.frame(displ = 1:100)) 

cars$displ %>% range()

cars$pred = predict(test, cars)
plot(cars$cty, cars$pred) # reality vs. prediction

test1 <- glm(cty ~ displ + cyl, data = cars)
summary(test1)
# y = 28.2885 + (-1.1979)*displ + (-1.2347)*cyl

test2 <- glm(cty ~ displ + cyl + displ:cyl, data = cars)
# displ:cyl is referring to the interaction between the two variables
summary(test2)

# exp: 
#10 pts on exam for every 60 min of study 
#5 pts for every 10 mins of review


cars %>% 
  ggplot(aes(x = displ, y = cty, color = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "glm")

compare_models(test1, test2) %>% plot()
compare_performance(test1, test2) %>% plot()

test3 <- glm(cty ~ displ*cyl*year, data = cars)
summary(test3) # check which parameters are significant and then drop the excess
# displ*cyl = displ + cyl + displ:cyl
test4 <- glm(cty ~ displ + cyl + year + displ:cyl + displ:year +
               cyl:year, data = cars)
summary(test4)
compare_performance(test, test1, test2, test3, test4) %>% plot()

# Predict cty using 3 models and compare the results
# (probably just plot plot out and see each of the predictions)
modeltest1 <- glm(cty ~ displ*hwy, data = cars) # using hwy is not very informative 
# because that is just the difference between mpg in cty vs on the hwy
summary(modeltest2)
modeltest2 <- glm(cty ~ displ + trans, data = cars)
modeltest3 <- glm(cty ~ displ:trans, data = cars)
modeltest4 <- glm(cty ~ displ:trans + model, data = cars)
compare_performance(modeltest1, modeltest2, modeltest3, modeltest4) %>% plot()

cars$pred1 <- predict(test, cars)
cars$pred2 <- predict(test1, cars)
cars$pred3 <- predict(test2, cars)

cars %>% 
  ggplot(aes(x = displ, y = pred3, color = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "glm")

cars %>% 
  pivot_longer(starts_with("pred")) %>% 
  ggplot(aes(x = displ, y = cty, color = factor(cyl)))+
  geom_point(aes(y = value), color = "black") +
  geom_smooth(method = "glm") +
  facet_wrap(~ name)
