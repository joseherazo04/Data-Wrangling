# this steps ca be skip because the data set is in dslab package but it worths taking a look
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]
# skip above

data("raw_data_research_funding_rates")
raw_data_research_funding_rates %>% head
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]] #it returned a list of 1 element, we select [[1]]
tab %>% head
# we select the titles of the columns. They are spread in two rows
the_names_1 <- tab[3]
the_names_2 <- tab[4]
#lists 3 and 4 have the column names

# To extract the names of the columns in the first row we use regex
the_names_1 <- the_names_1 %>%
  str_trim() %>% #remove spaces at the start and end
  str_replace_all(",\\s.", "") %>% #remove spaces between names
  str_split("\\s{2,}", simplify = TRUE) #remove rest of spaces
the_names_1

# For the second row we use trim to remove spaces at the start and end of the string
# then split it by spaces
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# Join to generate one name for each column
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
#rep repeate each colum 3 times
#the_names_2[-1] select all the column except the first one
the_names <- c(the_names_2[1], tmp_names) %>% #add the_names_2[1] "Discipline"
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>% #remove first spaces
  str_split("\\s{2,}", simplify = TRUE) %>%  #remove rest of spaces
  data.frame(stringsAsFactors = FALSE) %>% #return data.frame
  setNames(the_names) %>% #add column names
  mutate_at(-1, parse_number) #convert to number
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)


