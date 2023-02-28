suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
pizza <- read_csv('pizza.csv')

#Create a correlation matrix for temperature, bill, pizzas, and got_wine.
temp_cor <- select(pizza, temperature, bill, pizzas, got_wine)
print(round(Q1 <- cor(temp_cor),2))


#Create a correlation matrix of the relationships between time, temperature, bill, 
#and pizzas for Laura in the East branch.
laura_corr <- pizza %>%
  filter(branch == 'East' & operator =='Laura') %>%
  select(time, temperature, bill, pizzas)

print(round(Q2 <- cor(laura_corr),2))

  
#Run a regression predicting whether or not wine was ordered from temperature, bill, and pizza.
#Assign the coefficients to Q3
wine_reg <- glm(got_wine ~ temperature + bill + pizzas, binomial(), data = pizza)
print(Q3 <- round(summary(wine_reg)$coefficients, 2))

  
#Run a regression predicting bill from temperature, pizzas, and got_wine.
#Assign the standardized regression coefficients to Q4.  You should not round these values.
bill_reg <- lm(bill ~ temperature + pizzas + got_wine, data = pizza)
print(Q4 <- lm.beta(bill_reg))

  
#Add operator to the regression from Q4.  Which is the better model?  
#Assign the better model’s AIC to Q5.
bill_reg_2 <- lm(bill ~ temperature + pizzas + got_wine + operator, data = pizza)
AIC(bill_reg, k = 3)
print(Q5 <- AIC(bill_reg, k = 2))
  
  