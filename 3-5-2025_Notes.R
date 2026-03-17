library("readxl")
library("tidyverse")
path = read_xlsx("Data/messy_bp.xlsx", skip = 3)
names(path)
path %>% 
  select(-c("HR...9","HR...11","HR...13")) %>% 
  pivot_longer(cols = "BP...8", "BP...10", "BP...12")
mutate(Visit - case_when(colnames()))

bp <- path %>% 
  select(-starts_with("HR")) %>%  # This does the same as the line above
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
  pivot_longer(cols = starts_with("HR"),
               names_to = "visit",
               values_to = "HR") %>% 
  mutate(visit = case_when(visit == "HR...9" ~ 1,
                           visit == "HR...11"~ 2,
                           visit == "HR...13" ~ 3)) 
path_clean =full_join(bp, hr)
names(path_clean)
path_clean$Month of Birth
path_clean$`Month of `

path_clean %>% 
  mutate(DOB = paste(`Month of birth`, `Month of birth`, `Month of birth`, sep = "-"))

library(janitor)
clean_names()
make_clean_names()

make_clean_names("# of bacteria")
make_clean_names("% of fungi")

path_clean %>% 
  clean_names() %>% 
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, sep = "-")) %>% 
  ggplot(aes(x = DOB, y = hr)) +
  geom_line()

dates <- c("02/27/92", "02/27/92")
as.Date(dates, "%m/%d/%y")


clean_two <- path_clean %>% 
  clean_names() %>% 
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, sep = "-")) %>% as.Date()
View(clean_two)
  
clean_two$race %>% unique()

clean_two %>% 
  ggplot(aes(x = visit, y = hr)) +
  geom_line() +
  facet_wrap(~ wrap)

clean_two %>% 
  mutate(race_2 = case_when(race == "Caucasian" ~ "White",
                            race == "WHITE" ~ "White",
                            TRUE ~ race))

clean_two %>% 
  mutate(race_2 = case_when(race == "Caucasian" ~ "White",
                            race == "WHITE" ~ "White",
                            TRUE ~ race))

