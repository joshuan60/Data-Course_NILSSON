# Assignment 7: Tidy Code 
library(tidyverse)
library(janitor)
library(ggplot2)

# Loading in our unclear dataset
utah = read.csv("Utah_Religions_by_County.csv")
# Cleaning our column names
clean_utah <- utah %>% 
  clean_names()
# Converting our religion columns to rows  
religions_utah <- clean_utah %>%
  pivot_longer(
    cols = -c(county, pop_2010, religious, non_religious),  
    names_to = "religion",
    values_to = "Unknown_Value"
  )

# I am not 100% certain what the Unknown_value is, but I am pretty sure it has 
# something to do with population 
religions_utah %>% 
  ggplot(aes(x = religion, y = Unknown_Value, color = )) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# Based on the above graph, it looks like the lds faith has the highest value 
# across the state. I am still uncertain as to what this number represents but 
# I still think it has to do with population because there is a higher proportion
# of lds people in Utah. 

# Is the Unknown_Value a proportional value of population? 
# If it is, the sum of the religion values should total the value of the 
# religious column
match_check <- clean_utah %>%
  mutate(religion_sum = assemblies_of_god + episcopal_church + pentecostal_church_of_god +
           greek_orthodox + lds + southern_baptist_convention + united_methodist_church +
           buddhism_mahayana + catholic + evangelical + muslim + non_denominational + orthodox) %>%
  select(county, religious, religion_sum)
# Visualizing the comparison
match_check %>%
  ggplot(aes(x = religion_sum, y = religious)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Sum of Religion Columns", y = "Religious Column",
       title = "Verifying that religion columns sum to 'religious'") +
  theme_bw()
# The value difference between the religious and new religion_sum column is very
# slightly off, however, in some cases it is the exact same. This is likely a rounding
# error. If the columns represent a proportion then the religious coloumn and 
# non-religious column should add up to 1. 
clean_utah %>%
  mutate(prop_check = religious + non_religious) %>%
  select(prop_check)
# In this case, it looks like a majority of the sums add up to approximately one
# so I am going to proceed with the assumption of these numbers representing 
# proportional values. 
religions_utah <- clean_utah %>%
  pivot_longer(
    cols = -c(county, pop_2010, religious, non_religious),  
    names_to = "religion",
    values_to = "Pop_proportion"
  )

# Question #1: Does population of a county correlate with the proportion of 
# any specific religious group in that county?
religions_utah %>%
  ggplot(aes(x=pop_2010, y=Pop_proportion)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  facet_wrap(~religion, scales = "free") +
  theme_bw()
cor_table1 <- religions_utah %>% 
  group_by(religion) %>% 
  summarise(cor(pop_2010, Pop_proportion))
# Some religions are more correlated with county population size than others.
# For example, muslims have a very strong positive correlation between pop_size and 
# Pop_proportion (r = 0.759270880). 

# Question #2: Does proportion of any specific religion in a given county 
# correlate with the proportion of non-religious people?”
religions_utah %>%
  ggplot(aes(x=Pop_proportion, y=non_religious)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  facet_wrap(~religion, scales = "free") +
  theme_bw()
cor_table2 <- religions_utah %>% 
  group_by(religion) %>% 
  summarise(cor(Pop_proportion, non_religious))
# The answer is yes, there is a relationship between the proportion of specific
# religions and the proportion of non-religious people in a county.For example, 
# there is a strong negative relationship between the proportion of lds people and
# non-religious people (r = -0.86977078)