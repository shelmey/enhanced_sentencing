library(haven)
cr17to22 <- read_sas("C:/Users/Sam/Downloads/cr17to22/cr17to22.sas7bdat")
cr20to22 <- cr17to22 %>% filter(FILEDATE > as.Date("2020-01-01")) 
rm(cr17to22)
cr20to22$DOCKET %>% str

cr20to22$NAME[cr20to22$NAME!=""] %>% str
cr20to22$
myFeds$DOCKET <- NA
myFeds$DOCKET[grepl("-cr-", myFeds$docketNumber)] <- gsub("[^0-9]","",myFeds$docketNumber[grepl("-cr-", myFeds$docketNumber)])
myFeds$DOCKET <- myFeds$DOCKET %>% substr(2,8)
myFeds$DISTRICT <- myFeds$District
cr20to22unique <- cr20to22 %>%
  arrange(paste(DISTRICT, DOCKET),
          desc(LOADDATE)) %>%
  filter(!duplicated(paste(DISTRICT, DOCKET)))

fed_idb <- myFeds %>% left_join(cr20to22unique)
fed_idb$NAME %>% is.na %>% table
fed_idb %>% sheet_write(tPP_sheet, "Federal Cases w IDB cols")
