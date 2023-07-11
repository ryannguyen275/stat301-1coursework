#L05 Scratch Work
library(tidyverse, quietly = T)
library(tibble)
library(nycflights13)

### Exercise 2

tibble(
  price = c(3.99, 3.75, 3.00),
  store = c("target", "walmart", "amazon"),
  ounces = c(128, 128, 128)
)


store_data <- tribble(
  ~price, ~store, ~ounces,
  #-----|-------|--------
  3.99, "target", 128,
  3.75, "walmart", 128,
  3.00, "amazon", 128
)

store_data$price <- format(store_data$price, digits =3)
store_data

### Exercise 4

as_tibble(mtcars) %>% 
  slice_head(n = 4)

### Exercise 5
df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

df2 <- tibble(abc = 1, xyz = "a")
df2$x
df2[, "xyz"]
df2[, c("abc", "xyz")]

### Exercise 6

as_tibble(mpg)
var <- "displ"
mpg %>% 
  select(var)

### Exercise 8

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

print(annoying)

annoying$"1"

annoying %>% 
  ggplot(aes(x = .data[["2"]], y = .data[["1"]])) +
  geom_point()

annoying2 <- annoying %>% 
  mutate("3" = .data[["2"]] / .data [["1"]])

annoying2

rename(annoying2, 
       "one" = "1",
       "two" = "2",
       "three" = "3")