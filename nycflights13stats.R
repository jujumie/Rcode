suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library("nycflights13"))


#Address the outliers for departure delay as described in the outliers lectures, 
#using 0.997 and 0.003 as the cutoffs.  What percentage of data remains following 
#the removal of these outliers?  
dep_upper <- quantile(flights$dep_delay, 0.997, na.rm = TRUE)
dep_lower <- quantile(flights$dep_delay, 0.003, na.rm = TRUE)
dep_out <- which(flights$dep_delay > dep_upper | flights$dep_delay < dep_lower)
depnoout <- flights[-dep_out,]
round((Q1 <- (nrow(flights) - length(dep_out))/nrow(flights)*100), 2)

print(Q1)


#Run cor.test for the relationship between departure delay and distance. You should
#not round
print(Q2 <- cor.test(depnoout$dep_delay, depnoout$distance, data = flights))


#Create a regression predicting departure delay from distance. You should
#not round
model <- lm(depnoout$dep_delay ~ depnoout$distance, data = flights)
print(Q3 <- summary(model))


#Calculate standardized regression coefficients with lm.beta for the regression from Q3.
#You should not round
model_1 <- lm(dep_delay ~ distance, data = depnoout)
print(Q4 <- lm.beta(model_1))


#Create another regression, this time adding carrier to the regression from Q3.
#You should not round
model_two <- lm(depnoout$dep_delay ~ depnoout$distance + depnoout$carrier, data = flights)
print(Q5 <- summary(model_two))