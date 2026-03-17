data() # Shows you the data available to you

# 1. Save mtcars dataset to new object. 
cars <- mtcars
str(cars)
dim(cars)

# 2. Save cars with mpg >20 to a new obj called "good_cars"
cars[1:2,]
cars[,1:2]
cars[1:2, 1:2]
cars[c(1,3), c(1,4)]
good_cars <- cars[cars$mpg > 20,]

# write.csv(good_cars, file = "Good_cars.csv")

# 1. get the car with cyl equal to 4
cyl_cars <- cars[cars$cyl == 4,]
# Save both mpg > 20 and cyl equal to 4 into a new object

