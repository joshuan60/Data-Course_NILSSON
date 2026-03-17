# Create a plot showing how GDP and life expectancy have
# changed across different countries over the years
# label country (geom_text())
# (bonus) label country of interest
library('tidyverse')
library("GGally")
library("gapminder")
library("gganimate")
library("gifski")
library("av")
view(gapminder)
coolstuff<- gapminder
cont <- coolstuff %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~ continent) +
  geom_text(aes(label = country, hjust = -0.5, vjust = -0.5)) +
  theme(legend.position = "none") +
  transition_time(year) 

# Isolating Countries
# "New Zealand", "Kuwait", "Cambodia", "Rwanda", "United Kingdom", "China", "Norway", "Sri Lanka"
# Make a new coloumn 
# Only label the countries we are interested in
coolcountry <- c("New Zealand", "Kuwait", "Cambodia", "Rwanda", "United Kingdom", "China", "Norway", "Sri Lanka")
coolstuff %>% 
  mutate(cool_countries = case_when(country %in% coolcountry ~ country)) %>% 
  View()

cont2 <- coolstuff %>% 
  mutate(cool_countries = case_when(country %in% coolcountry ~ country)) %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~ continent) +
  geom_text(aes(label = cool_countries, hjust = -0.5, vjust = -0.5)) +
  theme(legend.position = "none") +
  transition_time(year) 

# Read in "wide_income_rent.csv" and make a plot to show
# rent in each state
dat = read.csv("Data/wide_income_rent.csv")

dat2 = t(dat) # transpose the data turning it into a matrix

dat3 = as.data.frame(dat2) # convert to dataframe

dat4 = dat3[-1,] # Remove the first row

colnames(dat4) = c("income", "rent") # add the column names

dat4$state = rownames(dat4) # creating a coloumn with the state names 

dat4 %>% 
  ggplot(aes(x = state,
             y = rent)) +
  geom_bar(stat = "identity")

# Options to customize
# pivot_longer()
# pivot_wider()
dat %>% 
  pivot_longer(-variable, names_to = "state", values_to = "USD") %>% 
  View()
  
dat_test = data.frame(
  ID = c(22, 33, 45, 60),
  H = c(145, 155, 160, 132),
  W = c(32, 22, 134, 50)
)

dat_test %>% 
  pivot_longer(c(H,W), names_to = "measure", values_to = "value")

dat_test %>% 
  pivot_longer(cols = everything(), names_to = "measure", values_to = "value")

dat_long = dat_test %>% 
  pivot_longer(-ID, names_to = "measure", values_to = "value") # this does the same as line 76

dat_long %>% 
  pivot_wider(names_from = "measure", values_from = "value")
  
# Using pivot to clean "wide_income_rent.csv" and plot rent for each state
datnew = read.csv("Data/wide_income_rent.csv")
View(datnew)
datnew %>% 
  pivot_longer(-variable, names_to = "State", values_to = "USD") %>% 
  pivot_wider(names_from = "variable", values_from = USD) 
  