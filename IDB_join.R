library(haven)
library(dplyr)
library(reshape2)
library(stringr)

inputs <- dir("/home/sam/Projects/RichardDefense/enhanced_sentencing/Inputs",pattern = "cr2[0-2]_0\\.sas7bdat", full.names = T)

cr20to22 <- inputs %>%
  lapply(read_sas) %>%
  bind_rows() %>%
  arrange(paste(DISTRICT, OFFICE, DOCKET),
          desc(LOADDATE)) %>%
  filter(!duplicated(paste(DISTRICT, OFFICE, DOCKET)))

cr20to22$DOCKET %>% str

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

### ID Felony Convictions ###
disps <- cbind(`Case ID` = fed_idb$`Case ID`, fed_idb[, grepl("DISP[0-9]", names(fed_idb))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
TOFFLVL <- cbind(`Case ID` = fed_idb$`Case ID`, fed_idb[,grepl("TOFFLVL",names(fed_idb))])
names(TOFFLVL) <- names(TOFFLVL) %>% str_remove_all("TOFFLVL")
disps <- disps %>% melt
names(disps)[2] <- "Offense"
names(disps)[3] <- "DISP"
TOFFLVL <- TOFFLVL %>% melt
names(TOFFLVL)[2] <- "Offense"
names(TOFFLVL)[3] <- "TOFFLVL"
disps <- disps %>% left_join(TOFFLVL)
disps$Tfelony <- (disps$DISP %in% c(4,9)) & disps$TOFFLVL == 4

disps_max <- disps %>%
  group_by(`Case ID`) %>%
  summarise(Tfelony = max(Tfelony,na.rm = T))
fed_idb <- fed_idb %>% left_join(disps_max)

terror_charges <- read.csv("Inputs/terror_titles.csv",stringsAsFactors = F, strip.white = T)

myTitles <- paste(terror_charges$Title, terror_charges$Section, sep = ":") %>% 
  str_remove_all(" .*$") %>%
  toupper() %>%
  str_replace_all("[)(]+","\\.") %>%
  str_remove_all("\\.$")

ttitles <- fed_idb[, grepl("TTITLE[0-9]", names(fed_idb))]
ftitles <- fed_idb[, grepl("FTITLE[0-9]", names(fed_idb))]

terrorMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"\\.") %in% str_remove_all(myTitles,"\\."))))

fed_idb$TTerror <- rowSums(terrorMat)>0
terrorMat <- apply(ftitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"\\.") %in% str_remove_all(myTitles,"\\."))))

fed_idb$FTerror <- rowSums(terrorMat)>0
dickCharges <- c("18:111","18:1361")
dickMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[^0-9\\:]") %in% dickCharges)))
fed_idb$TRH <- rowSums(dickMat)>0

# terrorMat <- apply(ftitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[A-Z\\.].*$") %in% str_remove_all(myTitles,"[A-Z\\.].*$"))))
# fed_idb$MaybeFTerror <- rowSums(terrorMat)>0
# terrorMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[A-Z\\.].*$") %in% str_remove_all(myTitles,"[A-Z\\.].*$"))))
# fed_idb$MaybeTTerror <- rowSums(terrorMat)>0

fed_idb %>% sheet_write(mySheet, "Federal Cases w IDB cols")
