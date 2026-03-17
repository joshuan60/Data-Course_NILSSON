# Assignment 2 ####
## Question 4 ####
# pattern specifies the type of file you are looking for.
csv_files <- list.files("Data/", pattern = '.csv', 
                        recursive = T) 
## Question 5 ####
length(csv_files)
## Question 6 ####
df <- read.csv("Data/wingspan_vs_mass.csv")
## Question 7 ####
head(df, n = 5L)
## Question 8 ####
# you can customize the key characters pattern looks for.
finding_b<- list.files("Data/", pattern = '^b', 
                       recursive = T, full.names = T) 

## Question 9 ####
# In this loop I am having it parse through each file and print the first
# line. I used cat instead of read.csv and header so it made sure to 
# print the first line regardless of the file type. 
for (i in finding_b) {
  cat("File:", i, "\n")
  cat(readLines(i, n=1), "\n\n")
}
## Question 10 ####
# Despite these supposed to be csv files, we still used cat because 
# the first line of a csv file is not necessarily referring to the 
# first row of the file. 
csv_files2 <- list.files(path = "Data",
                         pattern = "\\.csv$",
                         recursive = T,
                         full.names = T
)

for (f in csv_files2) {
  cat("File:", f, "\n\n")
  cat(readLines(f, n = 1), "\n\n")
}
