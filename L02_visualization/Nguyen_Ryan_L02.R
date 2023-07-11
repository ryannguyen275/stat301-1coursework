library(ggplot2)

# Exercise 2
ggplot(data = mpg, aes(x = cyl, y = hwy)) +
  geom_point()

# Exercise 3
#|: class vs drv
ggplot(data = mpg, aes(x = drv, y = class)) +
  geom_point()

# Exercise 4
#| label: ex-4
ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy, color = "blue"))


# Exercise 15
ggplot(data = diamonds) + 
  geom_pointrange(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median,
    stat = "summary"
  )


# Exercise 20

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  coord_fixed()
