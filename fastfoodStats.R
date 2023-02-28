suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
suppressPackageStartupMessages(library(lm.beta))
fastfood <- openintro::fastfood

#Create a correlation matrix for the relations between calories, total_fat, sugar, and 
#calcium for all items at Sonic, Subway, and Taco Bell, omitting missing values with na.omit().
fastfood_cor <- fastfood %>%
  filter(restaurant == 'Sonic' | restaurant == 'Subway' | restaurant == 'Taco Bell') %>%
  na.omit() %>%
  select(calories, total_fat, sugar, calcium)

print(round(Q1 <- cor(fastfood_cor),2))
  

#Create a regression predicting whether or not a restaurant is McDonalds or Subway based on 
#calories, sodium, and protein.  (McDonalds should be 1, Subway 0). Save the coefficients as Q2
restaurant_reg <- fastfood %>%
  filter(restaurant == 'Mcdonalds' | restaurant == 'Subway') %>%
  mutate(restbin = ifelse(restaurant == 'Subway', 0, 1))

levels(restaurant_reg$restaurant) <- c('Yes', 'No')

table(restaurant_reg$restaurant)

rest_reg_final <- glm(restbin ~ calories + sodium + protein, binomial(), data = restaurant_reg)

print(round(Q2 <- rest_reg_final$coefficients, 2))


#Run the same regression as in #2 but remove sodium as a predictor. Which model fits better?  
#Save the AIC of the better model to Q8.  
print(restaurant_reg_2 <- glm(restbin ~ calories + protein, binomial(), data = restaurant_reg))
print(rest_reg_final <- glm(restbin ~ calories + sodium + protein, binomial(), data = restaurant_reg))
print(round(Q8 <- rest_reg_final$aic, 2))


#Run a regression predicting calories from saturated fat, fiber, and sugar.  Based on standardized 
#regression coefficients, identify the strongest predictor. Assign the unstandardized regression 
#coefficient of the strongest predictor to Q4. (You can access the coefficients by indexing the 
#model object)
restaurant_reg_3 <- lm(calories ~ sat_fat + fiber + sugar, data = fastfood)
lm.beta(restaurant_reg_3)

print(round(Q4 <- restaurant_reg_3$coefficients[2],2))


#For this question, use data from only restaurants with between 50 and 60 items in the data set.  
#Predict total fat from cholesterol, total carbs, vitamin a, and restaurant.  Remove any 
#nonsignificant predictors and run again. Assign the strongest standardized regression 
#coefficient to Q5. 
filtered_rest <- fastfood %>%
  group_by(restaurant) %>%
  summarise(n=n()) %>%
  filter(n > 50 & n < 60)

total_fat_reg <- fastfood %>%
  filter(restaurant == 'Arbys' | restaurant == 'Mcdonalds' | restaurant == 'Sonic')

print(summary(lm(total_fat ~ cholesterol + total_carb + vit_a + restaurant, data = total_fat_reg)))

reg_wo_nonsig <- lm(total_fat ~ cholesterol + total_carb + restaurant, data = total_fat_reg)

print(stand_reg <- lm.beta(reg_wo_nonsig))

print(round(Q5 <- stand_reg$standardized.coefficients[2], 2))