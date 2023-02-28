suppressPackageStartupMessages(library(tidyverse))
pizza <- read_csv('pizza.csv')

#Create a dataframe containing driver names of instances where free_wine = 1, 
#discount_customer = 1, and the order contained more than 4 pizzas
Q1 <- pizza %>%
  filter(free_wine == 1 & discount_customer == 1 & pizzas > 4) %>%
  select(driver)
  
print(Q1)


#Create a variable that is the ratio of bill to pizza, called ratio.  What is 
#the mean of that value (call the value mean_ratio)?
Q2 <- pizza %>%
  summarise(ratio = bill / pizzas) %>%
  summarise(mean_ratio = mean(ratio)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

print(Q2)


#For each day of the week, what is the variance in pizzas? The created values 
#should be called var_pizzas.
Q3 <- pizza %>%
  group_by(day) %>%
  summarise(var_pizzas = var(pizzas)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

print(Q3)


#Which operator had the higher average bill?
Q4 <- pizza %>%
  group_by(operator) %>%
  summarise(av_bill = mean(bill)) %>%
  filter(operator == 'Melissa') %>%
  select(operator)

print(Q4)


#What was the highest amount of free wine given by day/driver combination?
#(For instance, Friday Bruno was 13, while Wednesday Salvator was 12)

Q5 <- pizza %>%
  select(day, driver, free_wine) %>%
  group_by(day, driver) %>%
  summarise(n = as.integer(sum(free_wine)), .groups = 'keep') %>%
  arrange(desc(n)) %>%
  head(1)
  
print(Q5)