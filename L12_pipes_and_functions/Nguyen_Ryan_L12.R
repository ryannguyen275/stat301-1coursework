

### Exercise 1
x <- c(-Inf, 0, 5, 10, Inf)

rescale01 <- function(x) {
  x <- ifelse(x == -Inf, 0, x)
  x <- ifelse(x == Inf, 1, x)
  
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

x <- c(1:10, Inf)
rescale01(x)

min(x[x = !-Inf])
max(x[x = !Inf])
rescale02 <- function(x) {
  x <- ifelse(x == -Inf, min(x[x = !-Inf]), x)
  x <- ifelse(x == Inf, max(x[x = !Inf]), x)
  
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale02(x)

### Exercise 3

x <- c(20, NA, 5, 4, NA)
y <- c(NA, NA, 4, 2, NA)
z <- c(4, 3, 1 , 2, 1)

both_na <- function(x, y) {
  stopifnot(length(x) == length(y))
  length(which(is.na(x) & is.na(y)))
}

both_na(x, z)

### Exercise 6

lubridate::now()

class(time)

greet_me <- function(time = lubridate::now()){
  stopifnot(class(time) %in% c("POSIXct", "POSIXt"))
  
  hour <- hour(time)
  
  return(
    case_when(
    hour < 12 ~ 'Good Morning', 
    12 <= hour & hour < 18 ~ 'Good Afternoon',
    18 <= hour ~ 'Good Evening'
    )
  )
}

greet_me()

### Exercise 7

# takes single number
# if divisible by 3, returns "fizz"

fizzbuzz <- function(x){
  return(
    dplyr::case_when(
      x%%5!=0 & x%%3 == 0 ~ 'fizz',
      x%%5==0 & x%%3 != 0 ~ 'buzz',
      x%%5==0 & x%%3 == 0 ~ 'fizzbuzz',
      TRUE ~ as.character(x)
      )
  )
}

x <- 13
fizzbuzz(7)

3%%4

### Exercise 9

x <- "a"

base::switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

# if we provide a value, it will return the right hand side. if you haev a present value that is blank, it will return the next available answer.
# if it is not an option, it will not return anything