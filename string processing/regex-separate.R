library(tidyverse)

# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","52223'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# with extract we have more flexibility because we can define a regex expression

##############################################
# Rare cases in the case of study
##############################################

# 1. Many students measuring exactly 5 or 6 feet did not enter any inches. 
#    For example, 6' - our pattern requires that inches be included
# Solution
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
# For the case "5''" we can add '{0,1} to avoid the second ' or ?

# 2. Some students measuring exactly 5 or 6 feet entered just that number.
# 3. Some entires have spaces at the end, for example 5 ' 9.
# Solution
str_replace(s, "^([56])'?$", "\\1'0")
# numbers 4 and 7 are avoided because they are too rare

# 4. Some of the inches were entered with decimal points. For example 5'7.5''. 
#    Our pattern only looks for two digits.

# 5. Removing spaces at the start or the end of a string str_trim
str_trim("5 ' 9 ") #remove last space

