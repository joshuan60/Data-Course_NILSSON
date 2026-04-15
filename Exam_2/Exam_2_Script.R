library("tidyverse")
library("ggplot2")
library("janitor")
library("skimr")
library("easystats")
#### Question 1: Reading in our data ####
unicef <-read.csv("unicef-u5mr.csv")
#### Question 2: Cleaning our data ####
clean_unicef <- unicef %>% 
  pivot_longer(cols = starts_with("U5MR."),
               names_to = "Years",
               values_to = "U5MR") %>% 
  separate(Years, into = c("Trash", "Years")) %>% 
  mutate(Years = as.numeric(Years)) %>%
  select(-Trash)
#### Question 3. Plotting our U5MR Data for every country split between each continent ####
plot1 <- clean_unicef %>% 
  ggplot(aes(x = Years, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~ Continent)+
  labs(
    x = "Year",
    y = "U5MR"
  ) +
  theme_bw()
#### Question 4. Saving the above plot as a png ####
ggsave('Nilsson_Plot_1.png', plot = plot1)
#### Question 5. Ploting the mean U5MR for all the countries within a given continent at each year ####
plot2 <- clean_unicef %>% 
  group_by(Continent, Years) %>% 
  summarize(Average_Mortality = mean(U5MR, na.rm = T)) %>% 
  ggplot(aes(x = Years, y = Average_Mortality, color = Continent))+
  geom_line(linewidth = 1.2)+
  labs(
    x = "Year",
    y = "Mean_U5MR"
  ) +
  theme_bw()
#### Question 6. Saving the above plot as a png ####
ggsave('Nilsson_Plot_2.png', plot = plot2)
#### Question 7. Creating three models to predict U5MR ####
mod1 <- glm(U5MR ~ Years, data = clean_unicef)
mod2 <- glm(U5MR ~ Years + Continent, data = clean_unicef)
mod3 <- glm(U5MR ~ Years*Continent, data = clean_unicef)
#### Question 8. Compare the three models and determine the model that best fits ####
## Model 1
summary(mod1)
performance(mod1)
report(mod1)
## Model 2 
summary(mod2)
performance(mod2)
report(mod2)
## Model 3
summary(mod3)
performance(mod3)
report(mod3)
## Looking at all three
compare_performance(mod1, mod2, mod3) %>% plot()
## Based on the reports from all three models, model 2 and 3 are better models than 
## model one. When comparing all three models using the compare_performance() function
## I can see that both model 2 and 3 perform better than model 1, however, model 3
## outperforms model 2 in AIC, AICc, and BIC values, indicating it has a better fit for the
## data. Therefore, Model 3 is our best model based on the reports. 
#### Question 9. Plot each model in respect to their predictions ####
pred_df <- clean_unicef %>%
  mutate(
    mod1 = predict(mod1, newdata = clean_unicef),
    mod2 = predict(mod2, newdata = clean_unicef),
    mod3 = predict(mod3, newdata = clean_unicef)
  )
pred_long <- pred_df %>%
  pivot_longer(
    cols = starts_with("mod"),
    names_to = "Model",
    values_to = "Pred"
  )
pred_long %>%
  ggplot(aes(x = Years, y = Pred, color = Continent)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Model) +
  labs(
    x = "Year",
    y = "Predicted U5MR",
    title = "Model Predictions"
  ) 
