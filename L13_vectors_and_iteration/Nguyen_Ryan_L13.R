library(tidyverse)
library(nycflights13)

# Exercise 5
# for loop
# compute the mean of every column in `mtcars`

seq_along(mtcars)
ncol(mtcars) # each column
nrow(mtcars) # each observation

# for (what we're looping, what we're looping through)

# [,i] : rows comma columns

car_mean <- list()

for(i in 1:ncol(mtcars)) {
  col <- mtcars[,i]
  
  if(is.numeric(col)) {
  car_mean[[ names(mtcars)[i] ]] <- mean(col)
  }
}

car_mean

enframe(unlist(car_mean))

car_mean2 <- car_names <- vector()

for(i in seq_along(mtcars)){
  col <- mtcars[[i]]
  
  car_names[[i]] <- names(mtcars)[i]
  car_mean2[i] <- mean(col)
}

tibble(names = car_names, mean = car_mean2)


# Generate 10 random draws from a normal distribution with $\mu =$ −10, 0, 10, and 100

mu <- c(-10, 0, 10, 100)

map(mu, function(x) {
  rnorm(10, mean = x)
} 
)

random_draws <- c(-10, 0, 10, 100)

for(i in 1:ncol(random_draws)) {
  mean <- random_draws[[i]]
  
  random_draw <- rnorm(10, mean, sd = 1)
}

enframe(random_draw)

rnorm(10)
rnorm(10, mean = -10, sd = 1)



### Exercise 7


paste0(names(iris[1]), ":", round(mean(iris[[1]]), 2 ))

show_mean <- function(df) {
  
  for(i in 1:ncol(df)){
    if(is.numeric(df[[i]])) {
    cat(paste0(names(df[i]), ": ", round(mean(df[[i]]), 2 ), "\n"))
    }
  }
}

show_mean(df = iris)       

show_mean(df = flights)

### Exercise 9

x <- split(mtcars, mtcars$cyl)
map(x, function(df) lm(mpg ~ wt, data = df))
map(x, ~ lm(mpg ~ wt, data = .))

