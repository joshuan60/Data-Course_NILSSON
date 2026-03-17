getwd()

read.csv()

#absolute path
read.csv("/Users/joshu/Downloads/Data-Course_NILSSON/Data/1620_scores.csv")
#relative path 
dat <- read.csv("Data/1620_scores.csv")

# This gives you a breakdown of files in particular folders
length(list.files('Data/'))

list.files("Data/", recursive = T) # recursive = T Counts every file in the specified directory, this includes subdirectories

list.files("Data/", pattern = '.csv', ignore.case = T, recursive = T) # pattern specifies the type of file you are looking for.

getwd()
dat[1:3, 1:4] # [row, column]
View(dat)
View(listlist)

head(dat)
tail(dat)

readLines('Data/1620_scores.csv')

# loop
'''
while (condition) {
  
}

for (variable in vector) {
  
}
'''
fruits = c("Strawberry", "Mango", "Peach")
for (fruit in fruits) {
  print(paste("I like", fruit))
}
