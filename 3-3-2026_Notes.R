library("tidyverse")
table1
table2
table3
# 1 observation per row
# col = variable
# Make table2 clean/tidy/good to make a plot
table2 %>% 
  pivot_wider(names_from = type,
              values_from = count) %>% 
  View()
# In table3 convert the rate to a better form
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE) %>% 
  mutate(rate = cases/population) %>% 
  # select(-rate)
  View()

# table4a & table4b Merge the two and convert them to the proper format
table4a
table4b
full_join(table4a, table4b) # this is not good, because it will make the population and 
# cases indistinguishable
taba = table4a %>% 
  pivot_longer(-country, names_to = "year", values_to = "cases")  
tabb = table4a %>% 
  pivot_longer(-country, names_to = "year", values_to = "population")
full_join(taba, tabb)
full_join(taba, tabb, by = "country") # selecting the variable to join by

# Fix table5
table5
table5 %>% 
  mutate(year = paste0(century, year) %>% as.numeric()) %>% 
  select(-century) %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE) %>% 
  mutate(rate = cases/population) 
  
?merge
?paste

# Cleaning messy_bp.xlsx
# Since this is not a csv we need a package to read it
library("readxl")
path = read_xlsx("Data/messy_bp.xlsx", skip = 3)
names(path)
path %>% 
  select(-c("HR...9","HR...11","HR...13")) %>% 
  pivot_longer(cols = "BP...8", "BP...10", "BP...12")
  mutate(Visit - case_when(colnames()))



# Worst Data Storage
wacky = read_xlsx("Data/Worst Data Storage Ever.xlsx", sheet = 2)

wacky2 = read_xlsx("Data/Worst Data Storage Ever.xlsx", sheet = 2, range = "C1:S10")
