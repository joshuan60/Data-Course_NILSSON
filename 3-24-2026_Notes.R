# Download "height.xlsx"
# make it tidy, make a plot, and explain it. 
library("readxl")
library("tidyverse")
library("janitor")
library("skimr")
library("ggplot2")
library("measurements")
height <- read_xlsx("Data/in_class_height.xlsx")
height2 <- height %>% 
  pivot_longer(cols = c("male", "female"), names_to = "sex")

clean_height <- height2 %>% 
  separate(value, into = c("feet", "inches"), convert = T) %>% 
  mutate(new_inch = feet*12 + inches) %>% 
  mutate(cm = conv_unit(new_inch, from = "in", to = "cm")) %>% 
  select(-c("feet", "inches", "new_inch"))

clean_height %>% 
  ggplot(aes(x = cm, fill = sex)) +
  geom_density(alpha = 0.5)

# Running a t-test to analyze the statistical significance of our data. 
# t-tests assume normal distribution
# When determining a test for your data test for normality and make sure you are choosing a good fit
t.test(cm ~ sex, data = clean_height)


?glm() # Generalized linear models
?lm() # Linear model

mod = glm(cm ~ sex, data = clean_height)
summary(mod)

relevel()