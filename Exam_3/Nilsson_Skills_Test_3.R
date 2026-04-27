library(tidyverse)
library(ggplot2)
library(janitor)
library(easystats)
library(broom)

# Question 1: Clean the table, and reconstruct the example graph.  
salaries <- read.csv("FacultySalaries_1995.csv")
# Cleaning the csv to add the necessary rows for our graph. 
clean_salaries <- salaries %>% 
  pivot_longer(cols = matches("Salary"),
               names_to = "Rank",
               values_to = "Salary") %>% 
  mutate(
    Rank = case_when(
      Rank == "AvgFullProfSalary"   ~ "Full",
      Rank == "AvgAssocProfSalary"  ~ "Associate",
      Rank == "AvgAssistProfSalary" ~ "Assistant",
      Rank == "AvgProfSalaryAll"    ~ "All",
      TRUE ~ Rank
    )
  ) %>% 
  filter(Rank != "All", Tier != "VIIB")  #Excluding the "All" group and "VIIB" tier

# Plotting our graph. 
plot <- clean_salaries %>% 
  ggplot(aes(x = Rank, y = Salary, fill = Rank)) +
  geom_boxplot() +
  facet_wrap(~ Tier) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotating the x-axis text. 

# Question 2: Build an ANOVA model that takes into consideration "State”, “Tier”, “Rank” on “Salary” 
aovmod <- clean_salaries %>% 
  aov(formula = Salary ~ State + Tier + Rank)
summary(aovmod1) #Prints the summary output

# Question 3: Tidy the “Juniper_Oils.csv” data
oils <- read.csv("Juniper_Oils.csv", check.names = FALSE)
names(oils)
clean_oils <- oils %>% 
  pivot_longer(cols = matches(c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")),
               names_to = "ChemicalID",
               values_to = "Concentration") 
  
# Question 4: Make a graph of Chemical Concentrations through time
plot2 <- clean_oils %>% 
  ggplot(aes(x = YearsSinceBurn, y = Concentration)) +
  facet_wrap(~ChemicalID, axes = "all_x", scales = "free_y") +
  geom_smooth()

# Question 5: Create a GLM to find which chemicals are significantly
# affected by "Years Since Burn"
glmmod <- glm(data = clean_oils, formula = Concentration ~ ChemicalID + ChemicalID:YearsSinceBurn)
glmmod %>% 
  tidy() %>% 
  filter(p.value < 0.05) %>% 
  mutate(term = gsub("ChemicalID", "", term)) %>% 
  print()

