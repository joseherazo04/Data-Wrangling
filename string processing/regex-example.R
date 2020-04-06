# load stringr through tidyverse
library(tidyverse)
data(murders)

# detect whether a comma is present
pattern <- ","
str_detect(murders$total, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches") # this performs the same as above

# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)

# this example highlight all the digits 
str_view_all(s, "\\d")