## This is a script for bird data analysis

# Your name
# Date

# load required packages 
library(tidyverse)
library(janitor)
library(skimr) # new package

source('my_function.R')

# load
df = read.csv('Data/Bird_Measurements.csv')
df = read_csv('Data/Bird_Measurements.csv')
View(df)

skim(df)


## data cleaning
keepers = c('Family', 'Species_number', 'Species_name', 'English_name', 
            'Clutch_size', 'Egg_mass') %>% 
  str_to_lower()


# separate male data
male = df %>% 
  clean_names() %>% 
  select(keepers, starts_with('m_'), -ends_with('_n')) %>% 
  mutate(sex = 'male') 

# separate female data
female = df %>% 
  clean_names() %>% 
  select(keepers, starts_with('f_'), -ends_with('_n')) %>% 
  mutate(sex = 'female') 

# separate unsexed data
unsexed = df %>% 
  clean_names() %>% 
  select(keepers, starts_with('unsexed_'), -ends_with('_n')) %>% 
  mutate(sex = 'unsexed') 

names(unsexed) = c('gname', 'mass', 'ddd', ....)

# combine
full_join()

clean_data = full_join(male, female) %>% 
  full_join(unsexed)
  
clean_data = male %>% 
  full_join(female) %>% 
  full_join(unsexed)
  
View(clean_data)


names(male)
names(female)
names(unsexed)


names(male) <- str_remove(names(male), 'm_')
names(male) <- names(male) %>% str_remove('m_')

names(female) <- names(female) %>% str_remove('f_')
names(unsexed) <- names(unsexed) %>% str_remove('unsexed_')


identical(names(male), names(female))


real_clean_data = male %>% 
  full_join(female) %>% 
  full_join(unsexed)

View(real_clean_data)



## make your own function

my_function <- function(argument){
  # code to execute
}


say_hello <- function(){
  # code to execute
  print('Hello!!')
}

say_hello()


add_numbers <- function(x, y){
  results <- x + y
  return(results)
}


add_numbers(2,66)


check_even_odd <- function(x){
  if(x %% 2 == 0){
    return(paste(x, 'is even'))
  }
  else{
    return(paste(x, 'is odd'))
  }
}


check_even_odd(12345)


clean_bird_data <- function(dat){
  ## data cleaning
  keepers = c('Family', 'Species_number', 'Species_name', 'English_name', 
              'Clutch_size', 'Egg_mass') %>% 
    str_to_lower()
  
  # separate male data
  male = df %>% 
    clean_names() %>% 
    select(keepers, starts_with('m_'), -ends_with('_n')) %>% 
    mutate(sex = 'male') 
  
  # separate female data
  female = df %>% 
    clean_names() %>% 
    select(keepers, starts_with('f_'), -ends_with('_n')) %>% 
    mutate(sex = 'female') 
  
  # separate unsexed data
  unsexed = df %>% 
    clean_names() %>% 
    select(keepers, starts_with('unsexed_'), -ends_with('_n')) %>% 
    mutate(sex = 'unsexed') 
  
  # rename the col names
  names(male) <- names(male) %>% str_remove('m_')
  names(female) <- names(female) %>% str_remove('f_')
  names(unsexed) <- names(unsexed) %>% str_remove('unsexed_')
  
  real_clean_data = male %>% 
    full_join(female) %>% 
    full_join(unsexed)
  
  return(real_clean_data)
}

dffff = read_csv('Data/Bird_Measurements.csv')
clean_dff = clean_bird_data(dffff)
View(clean_dff)


source('my_function.R') # = reproducable, reuse!!


check_even_odd(12345)



# make R package
devtoo
