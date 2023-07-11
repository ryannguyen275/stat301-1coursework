### L06 Import Data

### Loading Packages
library(tidyverse)

### Exercise 3
column_names <- c(
  "Entry", "Per.", "Post Date", "GL Account", "Description", "Srce.", 
  "Cflow", "Ref.", "Post", "Debit", "Credit", "Alloc."
)

column_widths <- c(8, 4, 12, 13, 26, 6, 4, 10, 13, 19, 13, 4)
                   
fwf_example <- read_fwf(file = "data/fwf_example.txt",
         col_positions = fwf_widths(widths = column_widths,
                                    col_names = column_names),
         skip = 2)

### Exercise 4
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1, "%B %d, %Y")
parse_date(d2, "%Y-%b-%d")
parse_date(d3, "%d-%b-%Y")
parse_date(d4, "%B %d (%Y)")
parse_date(d5, "%m/%d/%y")
parse_time(t1, "%H%M")
parse_time(t2, "%I:%m:%OS %p")


### Exercise 7

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`)))

annoying

annoying2 <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))) %>% 
  janitor::clean_names()

annoying2