#This is a giant code chunk

library(tidyverse)
library(skimr)
catdog <- read_delim("data/catvdogs.txt", delim = "|")

# Exercise 3

# Tidyverse notation -- cleanest efficient method
catdog %>% 
  filter(location == "Illinois") %>% 
  select(percent_dog_owners)

# alternative method
catdog %>% 
  filter(location == "Illinois") %>% 
  pull(percent_dog_owners)

catdog %>% 
  filter(location == "Illinois") %>% 
  pull(percent_dog_owners)

# Base R notation
catdog$percent_dog_owners[catdog$location == "Illinois"]

# Exercise 4

skim(catdog)

glimpse(catdog)

# Exercise 5: Finding the mean

# tidyverse
catdog %>% 
  summarize(mean = mean(percent_dog_owners))

# Base R notation
mean(catdog$percent_dog_owners)

# weighted mean
catdog %>% 
  summarize(wt.mean = weighted.mean (x = percent_dog_owners,
                                    w = num_households / sum(num_households)))


