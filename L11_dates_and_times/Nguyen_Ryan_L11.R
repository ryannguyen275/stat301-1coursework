library(tidyverse)
library(lubridate)
library(nycflights13)

### Exercise 4 

# Compare `air_time` with the difference between departure time and arrival time. Explain your findings. 

# *Hint:* Consider the location of the airport --- this may require joining data frames.

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_air <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    air_time_calc = arr_time - dep_time,
    check_time = air_time_calc == air_time
  ) %>% 
  select(origin, dest, contains("time"))

# join airports 
  
flights_join <- flights_air %>% 
  inner_join(airports, by = c("origin" = "faa")) %>% 
  inner_join(airports, by = c("dest" = "faa")) %>% 
  mutate(time_change = tz.y - tz.x,
         air_time_calc_adj = air_time_calc-60*time_change) %>% 
  select(origin, dest, contains("time"))

# we found negative numbers that were because they arrived the next day. how can we adjust for this?

### Exercise 5

# group by hour

# find mean for each hour 

flights_hour <- flights_dt %>% 
  mutate(
    sched_dep_hour = hour(sched_dep_time)
  ) %>% 
  group_by(sched_dep_hour) %>% 
  summarise(avg_delay = mean(dep_delay))

ggplot(flights_hour, aes(x = sched_dep_hour, y = avg_delay)) + 
  geom_line() +
  labs(x = "Hour of the Day", y = "Average Delay (in minutes)")

### Exercise 6

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_wday <- flights_dt %>% 
  mutate(day = wday(sched_dep_time, label = TRUE, abbr = FALSE),
         delayed = ifelse(dep_delay >0, 1, 0)) %>% 
  group_by(day) %>% 
  mutate(count = n()) %>% 
  group_by(day, delayed) %>% 
  mutate(delay_prop = n()/count) %>% 
  filter(delayed == 1)

ggplot(flights_wday, aes(x = day, y = delay_prop)) +
  geom_bar(stat = "identity") +
  labs(x = "Day of the Week", y = "Proportion of Delays")
  
  
### Exercise 7

tibble(Date = ymd("2022-01-01") + months(0:11), 
       Month = months(ymd(Date)), 
       "Day of the Week" = wday(Date, label = TRUE, abbr = FALSE))

### Exercise 8

age_year <- function(bday = "2003-01-01") {
  diff <- (ymd(bday) %--% today()) / years(1)
  
  year_round <- diff %/% 1
  
  return(cat("You are", year_round, "years old."))
}

age_year(bday = "2002-09-13")
