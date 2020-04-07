# load raw heights data and inspect
library(dslabs)
library(tidyverse)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x)) # to check how many NAs there are

# keep only entries that result in NAs
# it's good separate correct values from problematic ones
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

# to find all the problematic cases we need to avoid unreal values so we
# calculate cutoffs that cover 99.999% of human population
# we want to avoid heights that are not real
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9) #tallest
qnorm(alpha/2, 63.7, 2.7) #smallest

# keep only entries that either result in NAs or are outside the plausible range of heights
# this function detect all the problematic cases
# number with characters that are returned NA when using as.numeric
# and outliers
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

#############
# Patterns found after carefull revision
#############

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#####################################################
# Solutions
#####################################################

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest)) #convert cm to inches
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems

#####################################################
# Analysis of the rest of the problems
#####################################################

# After evaluating the problems, we found:
# 1. Many students measuring exactly 5 or 6 feet did not enter any inches. 
#    For example, 6' - our pattern requires that inches be included
# 2. Some students measuring exactly 5 or 6 feet entered just that number.
# 3. Some of the inches were entered with decimal points. For example 5'7.5''. 
#    Our pattern only looks for two digits.
# 4. Some entires have spaces at the end, for example 5 ' 9.
# 5. Some entries are in meters and some of these use European decimals: 1.6, 1,7.
# 6. Two students added cm.
# 7. One student spelled out the numbers: Five foot eight inches.

# Solution: 1
str_replace(s, "^([4-7])$", "\\1'0")

# Solution: 2 and 4
str_replace(s, "^([56])'?$", "\\1'0")

# Solution: 3
# we must permit zero or one period . followed by zero or more digits. 
# So we will use both ? and * ( . need to be escaped because it means any character )
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# Solution: 5
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") #only begin with 1 or 2 because it is in meters

# Solution: 7
# First to lower case
s <- c("Five feet eight inches")
str_to_lower(s)

#####################################################
# Solution for the problematic entries
#####################################################

# We create a function to deal with the majority of problematic entries
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

# And a procedure to convert words in lower case to numbers
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# Last check
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# There are 42 problematic entries yet, but some of them are not valid

#####################################################
# Final Solution
#####################################################

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

# Check entries converted
new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()