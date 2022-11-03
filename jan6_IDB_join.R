library(haven)
library(dplyr)

setwd('/home/sam/Projects/RichardDefense/enhanced_sentencing')
cr17to22_in <- "Inputs/cr17to22.sas7bdat"

source("jan6.R")

df <- df %>%
  within({
    DOCKET <- paste0(`Case Number` %>% str_remove_all("^.*\\:") %>% str_remove_all("-.*$"),
                     `Case Number` %>% str_remove_all("^.*-") %>% str_pad(5, "left", "0"))
    
    DOCKET[nchar(DOCKET) < 7] <- paste0("2",DOCKET[nchar(DOCKET) < 7])
    DOCKET[nchar(DOCKET) > 7] <- NA
    
  })


DCidb <- cr17to22_in %>%
  read_sas() %>% 
  filter(FILEDATE > as.Date("2021-01-01")) %>%
  arrange(paste(DISTRICT, DOCKET, OFFICE),
          desc(LOADDATE)) %>%
  filter(!duplicated(paste(DISTRICT, DOCKET, OFFICE))) %>% 
  filter(DISTRICT == "90" & OFFICE == "1")


jan6_idb <- df %>% 
  filter(grepl("cr",tolower(`Case Number`))) %>%
  left_join(DCidb)

jan6_idb$PRISTOT[jan6_idb$PRISTOT== -8] <- NA
jan6_idb$SENTDATE[jan6_idb$SENTDATE<as.Date("2000-01-01")] <- NA
jan6_idb$SENTDATE %>% is.na %>% table
jan6_idb$PRISTOT %>% is.na %>% table