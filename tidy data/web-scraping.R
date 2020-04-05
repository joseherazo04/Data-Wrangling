# import a webpage into R

#rvest for web scraping or web harvesting from tidyverse package
library(rvest)


########################################################
#READING FROM URL
########################################################

url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url) #convert html webpage to document
class(h)
h

########################################################
#EXTRACT TABLE
########################################################

#If we explore the html web page code we are going to notice some patterns that
#describe the table

#There are 3 tables in this page and they are inside this tags:
#<table></table>

#This line of code returns a list of table nodes as "xml_nodeset"
tab <- h %>% html_nodes("table")
#we decide to take the second table in the html page 
#it is returned as "xml_node"
tab <- tab[[2]] 
#html_tab convert xml_node to data.frame
tab <- tab %>% html_table
class(tab)

#Some arragements in the titles to end
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

########################################################
#Another more complicated example (CSS needed)
########################################################

#UNFORTUNATELY LINK DOESN'T WORK

#An example in which we elements or patterns patterns in a guacamole recipe
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

#Knowing how to access elements in the web page, we can create a function to access elements in 
#another page with the same patter. Useful if we want to extract data from diffents url with same behaviour

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

#The you can call this function with another url and everytime you want
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


