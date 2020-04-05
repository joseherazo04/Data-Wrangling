library(tidyverse)
library(dslabs)

data(admissions)
dat <- admissions %>% select(-applicants)