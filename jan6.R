library(dplyr)
library(rvest)
library(htmltools)
jan6url <- "https://www.justice.gov/usao-dc/capitol-breach-cases"
# parse with rvest

df <- jan6url %>% 
  read_html() %>%
  # html_nodes()
  html_node('.tablesaw-stack') %>%
  html_table() 

df$YEAR <- df$`Case Number` %>% str_remove_all("-.*$") %>% str_remove_all("1:") %>% str_remove_all("DC Superior Court 20")
df$DOCKET <- df$`Case Number` %>% str_remove_all("^.*-") %>% str_pad(5, "left", "0")

df$TYPEREG  <- df$`Case Number` %>% str_extract_all("-[A-Za-z]{2}-") %>% str_remove_all("-") %>% toupper() 
df$docketNumber <- paste0("1:",df$YEAR,"-",tolower(df$TYPEREG),"-", df$DOCKET)
df$DOCKET <- paste0(df$YEAR, df$DOCKET)
