library(dplyr)
library(rvest)
jan6url <- "https://www.justice.gov/usao-dc/capitol-breach-cases"
# parse with rvest
df <- jan6url %>% 
  html() %>%
  # html_nodes()
  html_node('.tablesaw-stack') %>%
  html_table() 
  # setNames(gsub('\\S+\\s+(\\S+)', '\\1', names(.))) %>%    # clean column names
  # setNames(gsub('\\s', '_', names(.)))
