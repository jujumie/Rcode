suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
suppressPackageStartupMessages(library(lm.beta))
brf <- read_csv('BRFSS2015.csv')


#Question 1
Q1 <- brf %>%
  filter(HLTHPLN1 == '1') %>%
  select(HLTHPLN1) %>%
  summarise(n = n())


#Question 2
brf$MENTHLTH[brf$MENTHLTH == 88] <- 0

Q2 <- brf %>%
  filter(`_STATE` == '42') %>%
  filter(MENTHLTH != '77' & MENTHLTH != '99') %>%
  select(`_STATE`, MENTHLTH) %>%
  summarise(mean_not_good = mean(MENTHLTH)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()


#Question 3
Q3 <- brf %>%
  filter(HAVARTH3 == '1' | HAVARTH3 == '2') %>%
  select(HAVARTH3, WTKG3) %>%
  mutate(WTLBS = WTKG3 *0.0220462) %>%
  group_by(HAVARTH3) %>%
  summarise(mean_weight = mean(WTLBS, na.rm = TRUE), sd_weight = sd(WTLBS, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()


#Question 4
phys_upper <- quantile(brf$PA1MIN_, 0.997, na.rm = TRUE)
phys_lower <- quantile(brf$PA1MIN_, 0.003, na.rm = TRUE)
phys_out <- which(brf$PA1MIN_ > phys_upper | brf$PA1MIN_ < phys_lower)
phys_no_out <- brf[-phys_out,]
round(Q4 <- (nrow(brf) - length(phys_out))/nrow(brf)*100, 2)


#Question 5
phys_no_out2 <- phys_no_out %>%
  filter(MARITAL != '9') %>%
  mutate(marital_factor = factor(MARITAL))

Q5 <- phys_no_out2 %>%
  select(marital_factor, PA1MIN_) %>%
  group_by(marital_factor) %>%
  summarise(mean_phys = mean(PA1MIN_, na.rm = TRUE), sd_phys = sd(PA1MIN_, na.rm = TRUE), 
            min_phys = min(PA1MIN_, na.rm = TRUE), max_phys = max(PA1MIN_, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()
  

#Question 6
print(Q6 <- ggplot(data = phys_no_out2, aes(x = 'marital_factor', y = 'PA1MIN_')) +
  geom_boxplot())


#Question 7
marital_reg <- lm(PA1MIN_ ~ marital_factor, data = phys_no_out2)
Q7 <- summary(marital_reg)


#Question 8
marital_anova <- aov(PA1MIN_ ~ marital_factor, data = phys_no_out2)
Q8 <- TukeyHSD(marital_anova)


#Question 9
marital_reg_2 <- lm(PA1MIN_ ~ marital_factor + `_FRUTSUM`, data = phys_no_out2)
print(summary(marital_reg_2))
print(summary(marital_reg))
AIC(marital_reg_2)
AIC(marital_reg)

round(Q9 <- AIC(marital_reg_2), 2)


#Question 10
#I chose the non-random outlier removal method for the AVEDRNK2 variable I chose because using
#the random outlier removal method, the only values removed were 1 and 99 and the data set without
#those values printed blank, so I simply filtered for the outliers which were 1 and 99. I 
#filtered for under 77 because the value 77 corresponds to the answer "not sure" and therefore
# I did not want to code that as 0 because that is not what it means, but it also does not mean
#77 drinks. The other 3 variables I chose, GENHLTH, MEDCOST, and ADDEPEV2, are based
#on coded values and therefore I did not remove any responses. However, in Question 11, I will
#address those coded values for accuracy in statistical analysis. 
drinks_upper <- quantile(brf$AVEDRNK2, 0.997, na.rm = TRUE)
drinks_lower <- quantile(brf$AVEDRNK2, 0.003, na.rm = TRUE)
drinks_out <- which(brf$AVEDRNK2 > drinks_upper | brf$AVEDRNK2 < drinks_lower)
drinks_no_out <- brf[-drinks_out,]

Q10 <- brf %>%
  filter(AVEDRNK2 > 1 & AVEDRNK2 < 77)


#Question 11
#As mentioned in Question 10, I filtered out the value 77 for the variable AVEDRNK2 because the 
#value 77 corresponds to the answer "not sure" and not 77 drinks. I converted the variables 
#GENHLTH, MEDCOST, ADDEPEV2 into factors for analysis. 
Q11 <- Q10 %>%
  mutate(genhlth_factor = factor(GENHLTH)) %>%
  mutate(medcost_factor = factor(MEDCOST)) %>%
  mutate(addepev2_factor = factor(ADDEPEV2))


#Question 12
ggplot(data = Q11) +
  geom_bar(mapping = aes(x = genhlth_factor))

ggplot(data = Q11) +
  geom_bar(mapping = aes(x = medcost_factor))

ggplot(data = Q11) +
  geom_bar(mapping = aes(x = addepev2_factor))

ggplot(data = Q11) +
  geom_histogram(mapping = aes(x = AVEDRNK2), binwidth = 0.6)

ggplot(data = Q11, mapping = aes(x = AVEDRNK2, colour = medcost_factor)) +
  geom_freqpoly(binwidth = 15)

ggplot(data = Q11, mapping = aes(x = AVEDRNK2, colour = addepev2_factor)) +
  geom_freqpoly(binwidth = 15)

Q12 <- ggplot(data = Q11, mapping = aes(x = AVEDRNK2, colour = genhlth_factor)) +
  geom_freqpoly(binwidth = 15)


#Question 13
Q13 <- Q11 %>%
  select(genhlth_factor, medcost_factor, addepev2_factor, AVEDRNK2) %>%
  group_by(AVEDRNK2) %>%
  summarise(mean_drinks = mean(AVEDRNK2, na.rm = TRUE), sd_drinks = sd(AVEDRNK2, na.rm = TRUE), 
            min_drinks = min(AVEDRNK2, na.rm = TRUE), max_drinks = max(AVEDRNK2, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()


#Question 14
model_1 <- lm(AVEDRNK2 ~ genhlth_factor, data = Q11)
model_2 <- lm(AVEDRNK2 ~ genhlth_factor + medcost_factor, data = Q11)
model_3 <- lm(AVEDRNK2 ~ genhlth_factor + medcost_factor + addepev2_factor, data = Q11)

summary(model_1)
summary(model_2)
summary(model_3)
AIC(model_1)
AIC(model_2)
AIC(model_3)

Q14 <- summary(model_3)