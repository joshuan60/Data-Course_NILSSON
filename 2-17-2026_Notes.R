# read "DatasaurusDozen.tsv"
# Examine and make a good graph
# tsv = tab seperated
library('tidyverse')
read.csv("Data/DatasaurusDozen.tsv") # this will not serperate the tabs

Datasaur <- read.delim("Data/DatasaurusDozen.tsv")

mean(Datasaur$x)
head(Datasaur)
unique(Datasaur$dataset)
# Always look at your data first
Datasaur %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x),
            mean_y = mean(y),
            sd_x = sd(x),
            sd_y = sd(y),
            max_x = max(x),
            max_y = max(y))

Datasaur %>% 
  ggplot(aes(x = x,
         y = y,
         color = dataset)) +
  geom_point() +
  facet_wrap(~ dataset)

# GGally
library(GGally)
ggpairs(Datasaur) # this will play around with different graphs using two of your data points
library(palmerpenguins)
ggpairs(penguins)
dim(penguins)

# animation 
# https://gganimate.com
#install gapminder for the example data, gganimate is the package
library(gapminder)
library(gganimate)
library(gifski)
library(av)
# install.packages("gifski")
# install.packages("av")
# Take a look of gapminder and make some cool graphs 
view(gapminder)
coolstuff<- gapminder
?gganimate
cont <- coolstuff %>% 
  ggplot(aes(x = year,
             y = lifeExp,
             color = continent)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~ continent) +
  transition_components(year)

anim_save("my_anim.gif", animation = cont)
