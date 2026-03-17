library("readxl")
library("tidyverse")
library("janitor")
path = read_xlsx("Data/messy_bp.xlsx", skip = 3)
names(path)

bp <- path %>% 
  select(-starts_with("HR")) %>%  
  pivot_longer(cols = starts_with("BP"),
               names_to = "visit",
               values_to = "BP") %>% 
  mutate(visit = case_when(visit == "BP...8" ~ 1,
                           visit == "BP...10"~ 2,
                           visit == "BP...12" ~ 3)) %>% 
  separate(BP, into = c("sys", "dia"))

# clean HR data and put them back
# check the data and see if there's anything that needs to be fixed. 
hr <- path %>% 
  select(-starts_with("BP")) %>% 
  pivot_longer(cols = starts_with("HR"),
               names_to = "visit",
               values_to = "HR") %>% 
  mutate(visit = case_when(visit == "HR...9" ~ 1,
                           visit == "HR...11"~ 2,
                           visit == "HR...13" ~ 3)) 

dat_clean <- full_join(bp, hr)
dat_clean %>%  names() # Original names before cleaning

dat_clean %>% 
  clean_names() %>% names() # names after cleaning

dat_clean %>% 
  clean_names() %>% 
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, sep = '-') %>%  as.Date()) %>% 
  ggplot(aes(x = DOB, y = hr)) +
  geom_line()
  # this takes the year, month, and day and creates a new coloumn with the date in a date format
# Correcting the issues with race
dat_clean_2 <- dat_clean %>% 
  clean_names() %>% 
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, sep = '-') %>%  as.Date()) %>% 
  mutate(race = case_when(race == 'Caucasian' | race == 'WHITE'~ "White",
                            TRUE ~ race)) 

dat_clean_2 %>% 
  ggplot(aes(x = visit, y = hr)) +
  geom_line() +
  facet_wrap(~ race)

dat_clean_2 %>% 
  ggplot(aes(x = visit, y = hr)) +
  geom_path() +
  facet_wrap(~ race)

# visualizing bp
dat_clean_2 %>% 
  ggplot(aes(x = visit, color = race)) +
  geom_path(aes(y = sys)) +
  geom_path(aes(y = dia)) +
  facet_wrap(~ race)
#The above looks weird because the sys and dia are chr
dat_clean_2 %>% str()
# Below we convert them to num
dat_clean_3 <- dat_clean_2 %>% 
  mutate(sys = sys %>% as.numeric(), 
         dia = dia %>% as.numeric())

# Visualizing it again
dat_clean_3 %>% 
  ggplot(aes(x = visit, color = race)) +
  geom_path(aes(y = sys)) +
  geom_path(aes(y = dia)) +
  facet_wrap(~ race)

# To visualize BP we need to combine sys and dia
dat_clean_4 <- dat_clean_3 %>% 
  pivot_longer(cols = c("sys", "dia"), names_to = "bp_type",
               values_to = "bp") 
dat_clean_4 %>% 
  ggplot(aes(x = visit, y = bp, color = bp_type)) +
  geom_path() +
  facet_wrap(~ race)
# Exploring our data
# Looking at hispanic people compared to others
dat_clean_4 %>% 
  ggplot(aes(x = visit, y = bp, color = bp_type)) +
  geom_path() +
  facet_wrap(~ hispanic)

