library("tidyverse")
#### Question 1: Reading in the data set and saving it into a dataframe object.####
cov_dat <- read.csv("cleaned_covid_data.csv")
#### Question 2: Subsetting the dataset for instances that have a state starting with the letter A. ####
a_states <- cov_dat %>% 
  filter(grepl("^a", Province_State, ignore.case = TRUE))
#### Question 3: Make a Scatter plot that shows deaths through time with states as the facet wrap. ####
a_states$Last_Update <- as.Date(a_states$Last_Update) # Changing to a date instead
# of a character. 
a_states %>%
  ggplot(aes(x = Last_Update, y = Deaths)) + # Gives the x/y for the graph
  geom_point(alpha = 0.6) + # prints the data into a scatter plot.
  geom_smooth(method = "loess", se = FALSE, color = "navy") + #Plotting the smooth line to show the trend through time. 
  facet_wrap(~ Province_State, scales = "free") + # This graphs each state independently
  labs(
    title = "Deaths Due to COVID-19 Through Time",
    x = "Date",
    y = "Deaths"
  ) +
  theme_bw()


#### Question 4: Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. ####
state_max_fatality_rate <- cov_dat %>%
  group_by(Province_State) %>% # This groups the states together
  summarize(Max_Ratio = max(Case_Fatality_Ratio, na.rm = T)) %>%  # This part takes the max of each state and saves it in a new column. 
  arrange(desc(Max_Ratio))
#### Question 5: Make a plot using the state_max_fatality_rate in descending order ####
state_max_fatality_rate %>% 
  ggplot(aes(x = reorder(Province_State, -Max_Ratio), y = Max_Ratio)) + 
  geom_col() +
  labs(
    title = "Max Covid Fatality Ratio by State",
    x = "State",
    y = "Max Fatality Ratio"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Question 6: Plotting Cumulative Covid Deaths in the US through time ####
cov_dat$Last_Update <- as.Date(cov_dat$Last_Update) 
cov_dat %>% 
  group_by(Last_Update) %>% 
  summarise(total_deaths = sum(Deaths)) %>% 
  ggplot(aes(x = Last_Update, y = total_deaths)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "navy") + #Plotting the smooth line to show the trend through time. 
  labs(
    title = "Deaths Due to COVID-19 Through Time in The US",
    x = "Date",
    y = "Deaths"
  ) +
  theme_bw()
