### L07_tidy_data

library(tidyverse)

### Exercise 1

table2_wide <- pivot_wider(table2, 
            id_cols = c(country, year),
            names_from = type, 
            values_from = count)

table2_rate <- table2_wide %>% 
  mutate(rate = 10000*cases/population)

table4a_long <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year", 
               values_to = "cases")

table4b_long <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year", 
               values_to = "population")

merge(table4a_long, table4b_long, by = c("country", "year"))


### Exercise 4

# dataset/table
people <- tribble(
  ~respondent_name,  ~key,    ~value, 
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people$time <- c("t1", "t1", "t2", "t1", "t1")

pivot_wider(people, 
            id_cols = c(respondent_name),
            names_from = key,
            values_from = value)

### Exercise 5

mm_data <- tribble(
  ~mm_type, ~blue, ~orange,	~green,	~yellow, ~brown, ~red, ~cyan_blue,
  "plain",  6,     18,      12,	    6,       7,	     7,    NA,
  "peanut", NA,	   11,	    9,	    1,	     12,	   8,    15
)


mm_data_tidy <- mm_data %>% pivot_longer(c(blue, orange, yellow, green, brown, red, cyan_blue),
                         names_to = "color",
                         values_to = "count") %>% 
  pivot_wider(id_cols = c(color),
              names_from = mm_type,
              values_from = count)


### Exercise 7
who

who_tidy <- who %>%
  pivot_longer(
    cols = c(new_sp_m014:newrel_f65), 
    names_to = "code", 
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who_tidy

### Exercise 8

who_na <- who %>%
  pivot_longer(
    cols = c(new_sp_m014:newrel_f65), 
    names_to = "code", 
    values_to = "value",
    values_drop_na = FALSE
  ) %>% 
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who_na

#how many na values
nrow(filter(who_na, is.na(value)))

#are they recording 0's in value or is that a reason for NA
nrow(filter(who_na, value == 0))

#helps confirms 0's are being recorded and missing values are most likely NA

who_years<- who_na %>% 
  group_by(country) %>% 
  summarize(min = min(year),
            max = max(year))

### Exercise 9

select(who, country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  # removing distinct observations
  filter(n() > 1)
