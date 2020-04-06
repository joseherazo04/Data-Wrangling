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

# only match ones and sevens we define the next character class: [17]
# for all the digits it will be: [1-9] (same as \\d)
# as in a regex everything is a string, this class represents 1,2 and 0: [1-20]
str_view(s,"[17]")

# ^ means start of string, $ means end of string
# they are called anchors
pattern <- "^\\d$" #start of a strinf followed by a digit followed by the end
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$" #start of string followed by one or two digit followed by the end
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
# read:
# start of string
# a digit between 4 and 7
# ' character
# one or two digits
# " character
# end of the string
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)