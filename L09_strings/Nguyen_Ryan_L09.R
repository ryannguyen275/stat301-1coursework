### Loading Packages

library(tidyverse)
library(stringr)

### Exercise 3

st1 <- "data"
st2 <- "science"

# find middle value
# round()
# ceiling() always rounds up
# floor() always rounds down

str_length(st1)/2

str_sub(st1, start = ceiling(str_length(st1)/2), 
        end = floor(str_length(st1)/2 + 1))

str_sub(st2, start = ceiling(str_length(st2)/2), 
        end = floor(str_length(st2)/2 + 1))

get_middle <- function(x) {
  str_sub(x, start = ceiling(str_length(x)/2), 
         end = floor(str_length(x)/2 + 1))
}

get_middle(x = "ryan")


### Exercise 5

## match sequence with str_view()
string <- "\"\'\\"
writeLines(string)
str_view(string, pattern = "\"\'\\\\")

### Exercise 6
string <- "$^$"
writeLines(string)
str_view(string, pattern = "\\$\\^\\$")

### Exercise 7
str_view_all(words, pattern = "^[y]")

words_tibble <- words %>% 
  as_tibble()

words_tibble %>% 
  filter(str_detect(value, pattern = "^y")) 

words_tibble %>% 
  filter(str_detect(value, pattern = "x$"))

### Exercise 8

# contains no vowels

words_tibble %>% 
  filter(str_detect(value, pattern = "[aeiou]", negate = TRUE))

### Exercise 10

phone <- "(123)-456-7890"
phone2 <- "123-456-7890"

pattern <- "\\(]*\\d{3}[)]-*\\d{3}-*\\d{4}"
pattern2 <- "\\d{3}-*\\d{3}-*\\d{4}"
str_view(phone, pattern)

str_view(phone2, pattern2)

### Exercise 11

pattern_a <- "^.*$"
pattern_b <- "\\{.+\\}"
pattern_c <- "\\d{4}-\\d{2}-\\d{2}"
pattern_d <- "\\\\{4}"
x <- c("\\\\\\\\", "{ryan}", "{2}", "43", "1234-56-78")

str_view(x, pattern_a)
str_view(x, pattern_b)
str_view(x, pattern_c)
str_view(x, pattern_d)

### Exercise 13

pattern_e <- "(.)\\1\\1"
pattern_f <- "(.)(.)\\2\\1"
pattern_g <- "(..)\\1"
pattern_h <- "(.).\\1.\\1"
y <- c("aaaa", "baaa", "aaba", "abba", "abab", "aabb")

str_view(y, pattern_e)
str_view(y, pattern_f)
str_view(y, pattern_g)
str_view(y, pattern_h)


### Exercise 15

# transfer the vector sentences into a tibble
sentences %>% 
  as.tibble() %>% 
  # rename value to sentence
  rename(sentence = value) %>% 
  # create a new variable that extracts only the first word by using "[A-Za-z0-9]+[\\s\\,]?"
  mutate(first_word = str_trim(str_extract(sentence, "[A-Za-z0-9]+[\\s\\,]?")))

# ending in 'ing'
sentences_ing <- sentences %>% 
  # make sentences into a tibble
  as.tibble() %>% 
  # renames value to sentence 
  rename(sentence = value) %>% 
  # extracting words ending in ing
  mutate(words_ending_in_ing = str_extract_all(sentence, "[A-Za-z]+(ing)[\\s\\.\\,]"),
         # replacing punctuation with nothing and then removing extra white space from that word 
         words_ending_in_ing = map(words_ending_in_ing, function(x) {      str_trim(str_replace(x, "[:punct:]", ""))
         })
  ) %>%  
  select(words_ending_in_ing) %>%
  unnest()
sentences_ing