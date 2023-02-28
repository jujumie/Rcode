suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
fastfood <- openintro::fastfood

#Looking only at Burger King and Chick-Fil-A, which item has the highest calories?
Q1 <- fastfood %>%
  filter(restaurant == 'Burger King' | restaurant == 'Chick Fil-A') %>%
  arrange(desc(calories)) %>%
  slice(1) %>%
  select(item)
  
print(Q1)


#What is the mean sugar amount for all items from Subway?
Q2 <- fastfood %>%
  filter(restaurant == 'Subway') %>%
  summarise(mean_sugar = mean(sugar)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()
  
print(Q2)


#What is the mean value of calories for all items from Taco Bell?
Q3 <- fastfood %>%
  filter(restaurant == 'Taco Bell') %>%
  summarise(mean_calories = mean(calories)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

print(Q3)


#Create a variable equal to total_fat  sugar called fatXsugar. Produce a tibble that has the 
#restaurant, item, and fatXsugar for the top 3 items, from highest to lowest.

Q4 <- fastfood %>%
  mutate(fatXsugar = total_fat * sugar) %>%
  select(restaurant, item, fatXsugar) %>%
  arrange(desc(fatXsugar)) %>%
  head(3)
  

print(Q4)


#How many restaurants have an average saturated fat over 10?
Q5 <- fastfood %>%
  group_by(restaurant) %>%
  mutate(av_sat_fat = mean(sat_fat)) %>%
  filter(av_sat_fat > 10) %>%
  select(restaurant) %>%
  distinct() %>%
  summarise(n = as.integer(n())) %>%
  count()

print(Q5)