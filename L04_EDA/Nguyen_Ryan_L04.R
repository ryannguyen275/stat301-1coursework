# L04 Exploratory Data Analysis (EDA)


## load pkgs ----
library(tidyverse)
library(nycflights13)
library(patchwork)
library(naniar)

### inspect data
?diamonds

diamonds %>% 
  skimr::skim_without_charts()

diamonds %>% 
  miss_var_summary()

mpg %>% 
  skim_without_charts()

dplyr::filter()

## Exercises ----

### Exercise 2 
hist_x <- ggplot(diamonds, aes(x = x)) +
  geom_histogram(bins = 50, color = "white")

hist_y <- ggplot(diamonds, aes(x = y)) +
  geom_histogram(bins = 50, color = "white")

hist_z <- ggplot(diamonds, aes(x = z)) +
  geom_histogram(bins = 50, color = "white")

hist_x + hist_y + hist_z

ggplot(diamonds) + 
  geom_density(aes(x), color = "red") +
  geom_density(aes(y), color = "blue") +
  geom_density(aes(z), color = "green")

### Exercise 3
diamonds %>% 
  ggplot(aes(x = price)) + 
  geom_histogram(binwidth = 200)

### Exercise 4
diamonds %>% 
  group_by(carat) %>% 
  filter(carat == 0.99 | carat == 1) %>% 
  summarise(count = n())

### Exercise 6
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 4 | y > 20, NA, y))

diamonds2 %>% 
  ggplot(aes(x = y)) + 
  geom_histogram(bins = 5, color = "white")

diamonds3 <- diamonds %>% 
  filter(y > 10) %>% 
  mutate(y = ifelse(y > 20, NA, y)) %>% 
  mutate(y = as.character(y))

diamonds3 %>%
  ggplot(aes(x = y)) +
  geom_bar()

### Exercise 7
flights %>% 
  summarise(mean = mean(dep_time),
            sum = sum(dep_time))

flights %>% 
  summarise(mean = mean(dep_time, na.rm = TRUE),
            sum = sum(dep_time, na.rm = TRUE))

### Exercise 8
# goal is to improve visualization 
flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(color = cancelled),
                stat = "density", 
                binwidth = 1/4)

flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = cancelled)) + 
  geom_boxplot()

### Exercise 9 Variable most important in predicting price

ggplot(diamonds, aes( x = carat, y = price )) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE)

ggplot(diamonds, aes( x = price )) +
  geom_freqpoly(aes(color = cut))

ggplot(diamonds, aes( x = price)) +
  geom_freqpoly(aes(color = color))

ggplot(diamonds, aes( x = price )) +
  geom_freqpoly(aes(color = clarity))

ggplot(diamonds, aes( x = depth, y = price )) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE)

ggplot(diamonds, aes( x = table, y = price )) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE)


### Exercise 13

# joint distribution
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# color within cut
diamonds %>% 
  count(color, cut) %>%  
  group_by(cut) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))

# cut within color
diamonds %>% 
  count(color, cut) %>%  
  group_by(color) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))


### Exercise 14

#switching x and y
diamonds %>% 
  count(color, cut) %>%  
  group_by(cut) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(mapping = aes(x = cut, y = color)) +
  geom_tile(mapping = aes(fill = prop))

### Exercise 15

flights %>%
  group_by(month, dest) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = dest, fill = arr_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Arrival Delay")

#airports I've been to
my_dest <- c('SAN', 'IAD', 'ORD', 'OMA', 'FLL', 'JAX', 'TPA', 'BWI', 'MCO')

flights %>% 
  filter(dest %in% my_dest) %>% 
  group_by(month, dest) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = arr_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Arrival Delay")

### Exercise 18

diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_smooth(se = FALSE)

diamonds %>% 
  mutate(carat_bin = cut_number(carat, 3)) %>% 
  ggplot() +
  geom_boxplot(aes(x = carat_bin, y = price, fill = cut)) +
  theme_minimal()
  