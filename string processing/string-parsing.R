library(tidyverse)
library(rvest)

# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

# we can notice both variables are characters when they actually represent numbers

###########################
# Escaping characters 
###########################

# Ir R to add strings with quotes or single quotes we have to scape those characters using
# \... For example: to initialize a string with this values: 2'3", we should have something like this:
#  "2'3\"" or '2\'3"' 

###########################
# Transforming to numeric
###########################

# 1. str_detect()  to determine whether a string contains a certain pattern
# 2. str_replace_all() to replace all instances of one pattern with another pattern
# 3. parse_number() removes punctuation from strings and converts them to numeric
# 4. mutate_at() performs the same transformation on the specified column numbers

# Defining function to detec commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas)) #fing where column has commas

# Replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "") #replace
test_1 <- as.numeric(test_1) #coerse to numeric

# work done so far but there is another way
# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population) #more efficient
identical(test_1, test_2)

# Mutate
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

