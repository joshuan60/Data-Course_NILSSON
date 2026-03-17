library('qrcode') #creates a QR code
library('tidyverse')
# install.packages('tidyverse')
cars <- mtcars
# 1. get the car with cyl equal to 4
cyl_cars <- cars[cars$cyl == 4,]
# Save both mpg > 20 and cyl equal to 4 into a new object
Super_good_car = cyl_cars[cyl_cars$mpg > 20,]
cyl_mpg <- cars[cars$cyl == 4 & cars$mpg > 20,]
# What are the data type of each column? 
## convert all cols to character
str(Super_good_car)
dim(Super_good_car)
?as.character
for (col in names(Super_good_car)) {
  #print(Super_good_car[, col])
  Super_good_car[, col] = as.character(Super_good_car[, col])
}


# apply()
?apply
# Applying a function to everything in a column. 
apply(Super_good_car, 2, as.character)



#Utilizing tidyverse package
# 1. get the car with cyl equal to 4
# Save both mpg > 20 and cyl equal to 4 into a new object
#cars$mpg %>% & pipe | are equivalent (shift ctrl m)
new_car = cars %>% 
  filter(mpg > 20) %>% 
  filter(cyl == 4)

# Save cars with mpg > 22, cyl == 4, wt < 3
# and hp >90 in a new object
new_car = cars %>% 
  filter(mpg > 22) %>% 
  filter(cyl == 4) %>% 
  filter(wt < 3) %>% 
  filter(hp > 90)

write.csv(new_car, "Data/new_car.csv"

          