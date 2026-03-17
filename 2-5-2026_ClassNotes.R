library("readxl")
library("tidyverse")
path = 'Data/messy_bp.xlsx'

dat = read_xlsx(path, skip = 3)
View(dat)


names(dat)
dat %>% 
  select(-c("HR...9", "HR...11", "HR...13")) %>% View()
  pivot_longer(cols = "BP...8", "BP...8")  

  
dat %>% 
  select(ends_with('8'))

bp = dat %>% 
  select(-starts_with('HR')) %>% 
  pivot_longer(cols = starts_with('BP'),
               names_to = 'visit',
               values_to = 'BP') %>% 
  mutate(visit = case_when(visit == 'BP...8' ~ 1,
                           visit == 'BP...10' ~ 2,
                           visit == 'BP...12' ~ 3)) %>% 
  separate(BP, into = c('sys', 'dia')) %>% View()
  
?ends_with


## clean HR data and put them back
## check the data and see if there's anything needs to be fixed

hr = dat %>% 
  select(-starts_with('BP')) %>% 
  pivot_longer(cols = starts_with('HR'),
               names_to = 'visit',
               values_to = 'HR') %>% 
  mutate(visit = case_when(visit == 'HR...9' ~ 1,
                           visit == 'HR...11' ~ 2,
                           visit == 'HR...13' ~ 3)) 

dat_clean = full_join(bp, hr)
path_clean =full_join(bp, hr)
View(dat_clean)

?merge

names(dat_clean)
dat_clean$Month of birth
dat_clean$`Month of birth`

MM/DD/YYYY
YYYY/MM/DD
DD/MM/YYYY

YYYY-MM-DD



dat_clean %>% 
  mutate(DOB = paste(`Month of birth`, `Month of birth`, `Month of birth`, sep = '-')) %>% View()


library(janitor)
clean_names()
make_clean_names()


dat_clean %>% 
  clean_names() %>% names()


dat_clean %>% 
  clean_names() %>%
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, sep = '-')) %>% View()


datttt = read_xlsx('/Users/yu-yaliang/Documents/Course/2024_Spring/BIOL3100/Worst Data Storage Ever.xlsx',
                sheet = 2, range = 'C1:S10')
View(datttt)

datttt %>% 
  clean_names() %>% names()

make_clean_names('# of bacteria')
make_clean_names('% of fungi')



dat_clean %>% 
  clean_names() %>%
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, sep = '-') %>% as.Date()) %>% 
  ggplot(aes(x = DOB, y = hr)) +
  geom_line()

dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(dates, "%m/%d/%y")


dat_clean_2 = dat_clean %>% 
  clean_names() %>%
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, sep = '-') %>% as.Date()) 

View(dat_clean_2)


dat_clean_2$race %>% unique()


dat_clean_2 %>% 
  ggplot(aes(x = visit, y = hr)) +
  geom_line() +
  facet_wrap(~ race)


dat_clean_2 %>% 
  mutate(race_2 = case_when(race == 'Caucasian' ~ "White",
                          race == 'WHITE' ~ "White",
                          TRUE ~ race)) %>% View()


dat_clean_2 %>% 
  mutate(race_2 = case_when(race == 'Caucasian' | race == 'WHITE'~ "White",
                            TRUE ~ race)) %>% 
  mutate(visit = as.factor(visit)) %>% 
  ggplot(aes(x = visit, y = hr)) +
  geom_line() +
  facet_wrap(~ race)


