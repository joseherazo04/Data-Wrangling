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