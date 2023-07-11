library(tidyverse)
library(nycflights13)
library(Lahman)
library(babynames)
library(nasaweather)
library(fueleconomy)

### Exercise 3

flights %>% 
  mutate(flight_id = row_number())

### Exercise 5
# Relationship between age of plane and average arrival delay
age_delay <- flights %>% 
  left_join(planes,
            by = c('tailnum')) %>% 
  group_by(tailnum) %>% 
  mutate(age = year.x - year.y) %>% 
  filter(!is.na(age)) %>% 
  summarize(age = mean(age, na.rm = TRUE),
            mean_arr_delay = mean(arr_delay, na.rm = TRUE))

age_delay %>% 
  ungroup() %>% 
  select(age, mean_arr_delay) %>% 
  cor(use = "complete.obs")

age_delay %>% 
  ggplot(aes(x = age, y = mean_arr_delay)) +
  geom_point()

### Exercise 6
weather_impact <- flights %>% 
  left_join(weather,
            by = c('origin', 'time_hour', 'year', 'month', 'day', 'hour'))

# average daily impact. Under the assumption there is not a huge weather change on one day.
weather_impact %>% 
  group_by(origin, year, month, day) %>% 
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            dewp = mean(dewp, na.rm = TRUE), 
            humid = mean(humid, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            precip = mean(precip, na.rm = TRUE),
            visib = mean(visib, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(mean_delay, temp, dewp, humid, wind_speed, precip, visib) %>% 
  cor(use = "complete.obs")

### Exercise 7
flights %>%
  anti_join(planes, by = 'tailnum') %>%
  group_by(carrier) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


### Exercise 8
flights %>% 
  group_by(tailnum) %>% 
  summarize(n=n()) %>% 
  filter(n > 100)

### Exercise 11

# help from TA, using `distinct()` to find distinct airline and plane combos
single_airlines <- flights %>%
  filter(!is.na(tailnum)) %>% 
  distinct(tailnum, carrier)

# number of planes that have flown more with more than one airline
single_airlines %>% 
  count(tailnum) %>% 
  filter(n > 1)

# joining data set with airlines to get specific names of tailnums that have flown for multiple airlines
airline_names <- single_airlines %>% 
  filter(n()>1) %>% 
  left_join(airlines, by = "carrier") %>% 
  arrange(tailnum, carrier)

airline_names

  
  
  
  