
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(kableExtra)
library(stringr)
library(reshape2)
library(pdftools)
library(haven)
mySheet <- "12TXm_e9Hba-jv3g4GasbP2RLsfC-FtZpGWewcG8gJjg"

inputs <- dir("/home/sam/Projects/RichardDefense/enhanced_sentencing/Inputs",pattern = "cr2[0-2]_0\\.sas7bdat", full.names = T)

cr20to22DC <- inputs %>%
  lapply(read_sas) %>%
  bind_rows() %>%
  filter(DISTRICT == "90") %>%
  arrange(DEFLGKY,
          desc(LOADDATE)) %>% 
  filter(!duplicated(DEFLGKY))
   
cr20to22DCunique <- cr20to22DC %>% filter(!duplicated(paste(DISTRICT, OFFICE, DOCKET)) & !duplicated(paste(DISTRICT, OFFICE, DOCKET), fromLast = T))
cr20to22DCcodefendants <- cr20to22DC %>% filter(duplicated(paste(DISTRICT, OFFICE, DOCKET)) | duplicated(paste(DISTRICT, OFFICE, DOCKET), fromLast = T))


jan6_idb <- df %>% 
  inner_join(cr20to22DCunique) %>%
  mutate(terminated = TERMDATE > as.Date("1900-01-01"),# dummy variable for terminated
         sentenced = SENTDATE > as.Date("1900-01-01")) # dummy variable for sentenced

codefendants <- df %>% filter(duplicated(paste(DOCKET, TYPEREG)) | duplicated(paste(DOCKET, TYPEREG), fromLast = T))
not_codefendants <- df %>% filter(!(duplicated(paste(DOCKET, TYPEREG)) | duplicated(paste(DOCKET, TYPEREG), fromLast = T)))

###################################


FOFFLVL <- cr20to22DC[,grepl("FOFFLVL",names(cr20to22DC))]
disps <- cbind(CASLGKY = cr20to22DC$CASLGKY, cr20to22DC[, grepl("DISP[0-9]", names(cr20to22DC))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
TOFFLVL <- cbind(CASLGKY = cr20to22DC$CASLGKY, cr20to22DC[,grepl("TOFFLVL",names(cr20to22DC))])
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
  group_by(CASLGKY) %>%
  summarise(Tfelony = max(Tfelony,na.rm = T))

maybeFelonies <- disps_max %>% 
  filter(Tfelony > 0) %>% 
  left_join(unique(select(cr20to22DC, CASLGKY, DOCKET, TYPEREG)))

myFelonies <- codefendants %>% filter(paste(DOCKET, TYPEREG) %in% paste(maybeFelonies$DOCKET, maybeFelonies$TYPEREG))

defFelonies <- not_codefendants %>%
  filter(paste(DOCKET, TYPEREG) %in% paste(maybeFelonies$DOCKET, maybeFelonies$TYPEREG)) %>% 
  inner_join(filter(cr20to22DC,SENTDATE > as.Date("1900-01-01")))

manual_data <- read_sheet(mySheet, "Plea Agreements") %>%
  select(Name,
         Count3A1.4,
         Type,
         Offense_Type,
         `PACER last checked`,
         Codefendant,
         Sentenced,
         `Transcript Available`,
         `Date Transcript Requested`,
         `Transcript Requested/Saved By`,
         `Transcript Saved`,
         `Transcript Location`,
         `Assigned Reader`,
         `Date Read`,
         Violent,
         Destructive,
         Misc,
         `Assigned Second Reader`,
         `Date Read Again`,
         Tfelony)
# cr20to22DC$CASLGKY
defFelonies <- defFelonies[,c("Name","Case Number","SENTDATE","PRISTOT","DOCKET","TYPEREG","CASLGKY",names(defFelonies)[grepl("TOFFLVL|TTITLE",names(defFelonies))])]
defFelonies <- defFelonies %>% left_join(manual_data)
defFelonies$FELONY <- TRUE
myFelonies$FELONY <- NA

myFelonies <- myFelonies %>% left_join(manual_data)
myFelonies <- myFelonies %>% filter(Tfelony | is.na(Tfelony))
allFelonies <- defFelonies %>% bind_rows(myFelonies)
allFelonies %>%
  sheet_write(ss = mySheet, sheet = "Felony Convictions")
# jan6_idb$Felony <- apply(FOFFLVL,1,function(x){ifelse(sum(is.na(x))==5,NA,4 %in% x)})
disps <- cbind(Name = jan6_idb$Name, jan6_idb[, grepl("DISP[0-9]", names(jan6_idb))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
jan6_idb$relevantDisp <- apply(disps,1,function(x){
  x <- as.vector(x)
  4 %in% x|8 %in% x|9 %in% x})

relevantDispCases <- jan6_idb %>% filter(relevantDisp)

termTitles <- jan6_idb[, grepl("TTITLE[0-9]", names(jan6_idb))]

filingTitles <- jan6_idb[, grepl("FTITLE[0-9]", names(jan6_idb))]

jan6_idb$relevantTitle <- apply(termTitles,1,function(x){
  x <- as.vector(x)
  sum(grepl("18:111|18:1361", x), na.rm = T)>0})

jan6_idb$relevantFiling <- apply(filingTitles,1,function(x){
  x <- as.vector(x)
  sum(grepl("18:111|18:1361", x), na.rm = T)>0})

relevantCases <- jan6_idb %>% filter(relevantDisp & relevantTitle)

relevantTitleCases <- jan6_idb %>% filter(relevantTitle)

relevantFilingCases <- jan6_idb %>%
  filter(relevantFiling) %>%
  filter(terminated)
# googledrive::drive_ls()
# gs4_create(paste0("January 6 cases as of ", Sys.Date()), sheets = c("Source data", "Relevant Charges & Disps", "Relevant Disps", "Relevant Filings", "Relevant Charges"))

jan6_idb %>% sheet_write(ss = mySheet, sheet = "Source data")

relevantCases %>%
  select(Name, `Case Number`, `Charge(s)`, Felony, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = mySheet, sheet = "Relevant Charges & Disps")

relevantDispCases %>%
  select(Name, `Case Number`, `Charge(s)`, Felony, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTOT, PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = mySheet, sheet = "Convicted or Pleaded Guilty")

relevantFilingCases %>%
  select(Name, `Case Number`, `Charge(s)`, Felony, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = mySheet, sheet = "Relevant Filings")



PRISTIM <- jan6_idb[,grepl("PRISTIM",names(jan6_idb))]

# Longest Sentences
jan6_idb[rev(order(apply(PRISTIM, 1, max, na.rm = T))),] %>% 
  head(10) %>%
  select(Name, `Case Number`, `Charge(s)`, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = mySheet, sheet = "Longest Single Sentences")
###################################

