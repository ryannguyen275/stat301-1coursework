library(nycflights13)
library(dplyr)

# Exercise 3G -> departed between midnight and 6am (inclusive)

flights %>% 
  filter(dep_time == 2400 | dep_time <= 600)

# Exercise 7

flights %>%
  arrange(dep_delay) %>% 
  select(dep_delay, origin, flight, dest, time_hour)

# Exercise 11

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

flights %>% 
  select(any_of(vars))

# Exercise 18
# Look at the number of cancelled flights per day. line graph
# Is there a pattern? 
library(lubridate)

flights_cancelled <- flights %>% 
  mutate(date = as.Date(time_hour)) %>% 
  group_by(date) %>% 
  summarize(num_cancelled = sum(is.na(arr_delay)))

ggplot(flights_cancelled, aes( x = date, y = num_cancelled )) +
  geom_line()

  
# Is the proportion of cancelled
# flights related to the average delay? scatterplot

flights_prop <- flights %>% 
  mutate(date = as.Date(time_hour)) %>% 
  group_by(date) %>% 
  summarize(prop_cancelled = sum(is.na(arr_delay)))

# Exercise 20

# For each destination, compute the total minutes of delay. 
# For each flight, compute the proportion of the total delay for its destination.

#arr_delay
flights %>%
  filter(arr_delay > 0, !is.na(arr_delay())) %>%
  group_by(dest) %>% 
  mutate(total_delay = sum(arr_delay),
            prop_delay = arr_delay/total_delay) %>% 
  select(tailnum, arr_delay, dest, total_delay, prop_delay, time_hour)


# Exercise 21 

# For each plane, count the number of flights
# before the first delay of greater than 1 hour.
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  select(tailnum, year, month, day, dep_time, arr_delay) %>% 
  # arrange them in order
  arrange(tailnum, year, month, day, dep_time) %>% 
  #group planes together
  group_by(tailnum) %>% 
  mutate(
    flights_before_delay = cumsum(arr_delay > 60)) %>% 
  #is 0 when the flight is before the first delay over an hour
  summarise(n_flights = sum(flights_before_delay == 0))

