suppressPackageStartupMessages(library(tidyverse))
data(msleep)

#Run cor.test for the relationship between total sleep and body weight. You should not 
#round these values.The answer should be assigned to Q1
print(Q1 <- cor.test(msleep$sleep_total, msleep$bodywt, data = msleep))


#Create a correlation matrix for the relations among total sleep, rem sleep, brain weight,
#and body weight.  Make sure to remove missing values. The matrix should be assigned to Q2
msleep_cor <- select(msleep, sleep_total, sleep_rem, brainwt, bodywt)
print(Q2 <- round(cor(msleep_cor, use = 'complete.obs'),2))


#Run a regression predicting body weight by vore. Assign the coefficients to Q3.
bodywt_reg <- lm(bodywt ~ vore, data = msleep)
print(Q3 <- round(coef(bodywt_reg),2))


#Create a regression predicting bodywt by vore and REM sleep.  Compared to the model 
#in Q3, which one has the better AIC? Assign the better AIC value to Q4
bodywt_reg <- lm(bodywt ~ vore, data = msleep, na.action = 'na.exclude')
bodywt_reg_2 <- lm(bodywt ~ vore + sleep_rem, data = msleep, na.action = 'na.exclude')
AIC(bodywt_reg, k = 1)
AIC(bodywt_reg_2, k = 2)
print(Q4 <- AIC(bodywt_reg_2, k = 2))


#Create a logistic regression predicting whether or not an animal is a carnivore or 
#herbivore based on sleep total. You should not round these values. 
log_reg <- msleep %>%
  filter(vore != 'omni' & vore != 'insecti') %>%
  mutate(vorebin = ifelse(vore == 'carni', 0, 1))

print(summary(log_reg_final <- glm(vorebin ~ sleep_total, binomial(), data = log_reg)))