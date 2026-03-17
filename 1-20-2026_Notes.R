# Read "wingspan_vs_mass.csv" using both 
# relative path and absolute path. 
# 2. how many rows and columns are in the file
wings <- read.csv("/Users/joshu/Downloads/Data-Course_NILSSON/Data/wingspan_vs_mass.csv")
wing <- read.csv("Data/wingspan_vs_mass.csv")

# Rows and columns
nrow(wings)
ncol(wings)
dim(wings) 

# Level 1 ####
### My note ####
## Level 2 ####
"""
Types of R files
.Rproj 
.R
"""
#Types of data
"""
'apple''
apple
1,2,3
"1","2","3"
TRUE, FALSE, T, F
"""
as.character() # Converts to a string 
as.logical() # Converts to a Boolean value
as.numeric() # Converts to numbers
color = c('light blue', 'orange', 'grey73', 'green', 'mint green', 'red',
          'blue', 'pink', 'purple', 'red', 'green')
length(color)
as.factor(color)

str(wings)
# types of objects 
# 1. vector (one dim, same type)
c()
num2 <- c("1", "2", "3")
vec <- c("1", 2, T)

is.character(vec)

# 2. matrix (two dim, same type of data)
matrix(c(1:6), nrow = 3, ncol = 2)
matrix(c(1:6), nrow = 3, ncol = 2, byrow = T)
matrix()

# 3. data frame (two dim, different types of data, same length)
str(wings)
wings$mass
wings$mass * 100
wings$mass_plus_2000 = wings$mass * 2000 # Adding a new variable/column

df_1 = data.frame(names = c('Joshua', 'Kurt', 'Salma'), 
                  fruit = c('Strawberry', 'Kiwi', 'Peach'),
                  calories = c(1, 2, 4))
# 4. Array (multiple dim, same type)
array(c(1:12), dim = c(2,2,3))

# 5. List (multi dim, different type, different length)
list_a <- list(wings, number = c(1:6), fruit = "Strawberry")

list_a$number

# 6. function (store a function)
list() # this is a built in function 

tea <- function(x, y){
  out = x+y
  print(out)
}

tea(1,2)

# 7. loops
## For loops
fruits = c("Strawberry", "Mango", "Peach")
for (fruit in fruits) {
  print(paste("I like", fruit))
}
## While loops
i <-1
while (i < 6) {
  print(i)
  i <- i + 1
}

# 1. create a data frame containing your favorite fruits (at least 3)
df_1 <- data.frame(names = c('Joshua', 'Kurt', 'Salma'), 
                  fruit = c('Strawberry', 'Kiwi', 'Peach'))
# 2. add calories to the data frame
df_1$calories <- c(1, 2, 4)
# 3. after creating the df, add a new col called "calories_100"
# the value = original cal + 100
df_1$calories_100 <- df_1$calories + 100
# 4. write a loop to print out the name of fruit and the amount of calories
for (row in 1:nrow(df_1)) {
  print(paste("The calories of",df_1$fruit[row], "is", df_1$calories_100[row]))
}


# Paste() notes
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
paste(month.abb, "is the", nth, "month of the year.")
paste(month.abb, letters)