# install palmerpenguins package
# open 'penguins' dataset
# save penguins with bill length > 40 in to a new object
# calculate the average body mass

# install.packages("palmerpenguins")
library('palmerpenguins')
library('tidyverse')
# big_bill = penguins[penguins$bill_length_mm > 40,]
# mean(big_bill)
# big_bill = penguins %>% 
#   filter(bill_length_mm > 40) %>% 
# 
# big_bill$body_mass_g %>% 
# Broken ^^
mean(big_bill$bill_length_mm, na.rm = TRUE)
# Alternatively
penguins %>% 
  filter(bill_length_mm > 40) %>% 
  pluck("body_mass_g") %>%
  mean()
view(penguins)
# calculate body mass by sex
penguins %>% 
  filter(sex == "male") %>% 
  pluck("body_mass_g") %>% 
  mean()

penguins %>% 
  filter(sex == "female") %>% 
  pluck("body_mass_g") %>% 
  mean()
#calculate body mass of sex for penguins whose bill length > 40
penguins %>% 
  filter(sex == "male") %>% 
  filter(bill_length_mm > 40) %>% 
  pluck("body_mass_g") %>% 
  mean()

penguins %>% 
  filter(sex == "female") %>% 
  filter(bill_length_mm > 40) %>% 
  pluck("body_mass_g") %>% 
  mean()

# Alternative way to sort by categorical
penguins %>% 
  filter(bill_length_mm > 40) %>% 
  group_by(sex, island) %>% 
  summarize(avg_body_mass = mean(body_mass_g), number_of_penguins = n(), max_mass = max(body_mass_g), min_mass = min(body_mass_g)) %>% 
  arrange(desc(max_mass))

# find the large penguins (body mass > 5000)
# count how many male & female
# return the max body mass by sex
# add new column to data to tell if they are overweight or not

penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>% 
  summarize(avg_body_mass = mean(body_mass_g),gender_number = n())

# mutate() changing a column or adding one that does not exist 
penguins %>% 
  mutate(len_times_dpt = bill_length_mm * bill_depth_mm) %>% 


penguins %>% 
  mutate(Weight_status = case_when(body_mass_g > 5000 ~ "Overweight", 
                                   body_mass_g < 3000 ~ "Underweight", 
                                   body_mass_g < 5000 & body_mass_g > 3000 ~ "Healthy")) %>% 
  view()

penguins %>% 
  mutate(Weight_status = case_when(body_mass_g > 5000 ~ "Overweight", 
                                   body_mass_g < 3000 ~ "Underweight",
                                   TRUE ~ "Healthy"))%>% 
  view()

# add new column to data to highlight penguins with big bills

penguins %>% 
  mutate(Bill_size = case_when(bill_length_mm > 41 ~ "Big", 
                                   bill_length_mm < 38 ~ "Small", 
                                   bill_length_mm < 41 & bill_length_mm > 38 ~ "Average")) %>% 
  view()