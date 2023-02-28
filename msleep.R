suppressPackageStartupMessages(library(tidyverse))
data(msleep)

#What is the variance in total sleep for carnivores and those whose conservation status is lc?
Q1 <- msleep %>%
  filter(vore == 'carni' & conservation == 'lc') %>%
  summarise(var = var(sleep_total)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

print(Q1)


#What rodent (order Rodentia) has the highest total sleep/rem sleep ratio?
Q2 <- msleep %>%
  filter(order == 'Rodentia') %>%
  mutate(sleep_ratio = sleep_total / sleep_rem) %>%
  arrange(desc(sleep_ratio)) %>%
  slice(1) %>%
  select(name)
  

print(Q2)


#How many primates have a bodyweight/brainwt ratio greater than 100?
Q3 <- msleep %>%
  mutate(bb_ratio = bodywt / brainwt) %>%
  filter(order == 'Primates' & bb_ratio > 100) %>%
  summarise(name = n())

print(Q3)


#Create two new variables, the mean of sleep total and variance of sleep total, 
#grouped by conservation, and removing missing values.  The names of the variables 
#should correspond to those in the example below.
Q4 <- msleep %>%
  group_by(conservation) %>%
  filter(!is.na(conservation)) %>%
  mutate(mean_sleep = mean(sleep_total, na.rm = TRUE)) %>%
  mutate(var_sleep = var(sleep_total, na.rm = TRUE)) %>%
  select(conservation, mean_sleep, var_sleep) %>%
  distinct(.keep_all = FALSE) %>%
  arrange(conservation) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()
  
print(Q4)


#Provide the name(s) of all the domesticated herbivores that sleep more than 12 hours.
Q5 <- msleep %>%
  filter(conservation == 'domesticated' & vore == 'herbi' & sleep_total > 12) %>%
  select(name)
  
print(Q5)