suppressPackageStartupMessages(library(tidyverse))
library("nycflights13")

# What is the mean distance of flights for each of the carriers AA, EV, and FL?
Q1 <- flights %>%
  filter(carrier == 'AA' | carrier == 'EV' | carrier == 'FL') %>%
  group_by(carrier) %>%
  summarise(av_dist = mean(distance)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

print(Q1)


# For the month with the highest number of flights, what is that value? Hint: use head(1).
Q2 <- flights %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  slice_max(n)

print(Q2)


#Find the five shortest minimum distances, called min_dist, by origin/destination combination.  
Q3 <- flights %>%
  select(origin, dest, distance) %>%
  group_by(origin, dest, distance) %>%
  arrange(distance) %>%
  rename(min_dist = distance) %>%
  distinct() %>%
  head(5)

print(Q3)


#What five days of the year had the highest mean distance when leaving from JFK?
#Sort in descending order.
Q4 <- flights %>%
  filter(origin == 'JFK') %>%
  group_by(month, day) %>%
  summarise(mean_distance = mean(distance)) %>%
  arrange(desc(mean_distance)) %>%
  mutate_if(is.numeric, round, 2) %>%
  head(5) %>%
  as.data.frame()
  
print(Q4)


# Calculate the maximum arrival delay for flights to Boston and Atlanta, separately.
Q5 <- flights %>%
  filter(dest == 'BOS' | dest == 'ATL') %>%
  select(dest, arr_delay) %>%
  group_by(dest) %>%
  summarise(max_arr_delay = max(arr_delay, na.rm = TRUE))
  
print(Q5)
