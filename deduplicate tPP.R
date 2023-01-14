library(haven)
library(stringr)
library(reshape2)
library(dplyr)
library(googlesheets4)

mySheet <- "1g_kBPD5BiAf0hhXjjLgJojgJy_1_HKOO--YkIRZJ_nA"
tPP <- mySheet %>% read_sheet("Federal Cases w Docket Numbers")

inputs <- dir("/home/sam/Projects/RichardDefense/enhanced_sentencing/Inputs",pattern = "cr2[0-2]_0\\.sas7bdat", full.names = T)

cr20to22 <- inputs %>%
  lapply(read_sas) %>%
  bind_rows() %>%
  arrange(DEFLGKY,
          desc(LOADDATE)) %>% 
  filter(!duplicated(DEFLGKY))

cr20to22unique <- cr20to22 %>% filter(!duplicated(paste(DISTRICT, OFFICE, DOCKET)) & !duplicated(paste(DISTRICT, OFFICE, DOCKET), fromLast = T))
cr20to22codefendants <- cr20to22 %>% filter(duplicated(paste(DISTRICT, OFFICE, DOCKET)) | duplicated(paste(DISTRICT, OFFICE, DOCKET), fromLast = T))

tPP$OFFICE <- tPP$docketNumber %>% str_remove_all(":.*$")
tPP$DOCKET[grepl("-cr-", tPP$docketNumber)] <- gsub("[^0-9]","", tPP$docketNumber[grepl("-cr-", tPP$docketNumber)])
tPP$TYPEREG <- tPP$docketNumber %>% str_extract("-[a-z]{2}-") %>% str_remove_all("-") %>% toupper
tPP$DOCKET <- tPP$DOCKET %>% substr(2,8)
tPP$DISTRICT <- tPP$District


tPP_idb <- tPP  %>% 
  inner_join(cr20to22unique) %>%
  mutate(terminated = TERMDATE > as.Date("1900-01-01"),# dummy variable for terminated
         sentenced = SENTDATE > as.Date("1900-01-01")) # dummy variable for sentenced

codefendants <- tPP %>% filter(duplicated(paste(DISTRICT, OFFICE, DOCKET, TYPEREG)) | duplicated(paste(DISTRICT, OFFICE, DOCKET, TYPEREG), fromLast = T))
not_codefendants <- tPP %>% filter(!(duplicated(paste(DISTRICT, OFFICE, DOCKET, TYPEREG)) | duplicated(paste(DISTRICT, OFFICE, DOCKET, TYPEREG), fromLast = T)))

###################################
FOFFLVL <- cr20to22[,grepl("FOFFLVL",names(cr20to22))]
disps <- cbind(CASLGKY = cr20to22$CASLGKY, cr20to22[, grepl("DISP[0-9]", names(cr20to22))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
TOFFLVL <- cbind(CASLGKY = cr20to22$CASLGKY, cr20to22[,grepl("TOFFLVL",names(cr20to22))])
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
  left_join(unique(select(cr20to22, CASLGKY, DISTRICT, OFFICE, DOCKET, TYPEREG)))

myFelonies <- codefendants %>% filter(paste(DISTRICT, OFFICE, DOCKET, TYPEREG) %in% paste(maybeFelonies$DISTRICT, maybeFelonies$OFFICE, maybeFelonies$DOCKET, maybeFelonies$TYPEREG))

defFelonies <- not_codefendants %>%
  filter(paste(DISTRICT, OFFICE, DOCKET, TYPEREG) %in% paste(maybeFelonies$DISTRICT, maybeFelonies$OFFICE, maybeFelonies$DOCKET, maybeFelonies$TYPEREG)) %>% 
  inner_join(filter(cr20to22, SENTDATE > as.Date("1900-01-01")))

felony_convictions <- read_sheet(mySheet, "Felony Convictions 2")

felony_convictions <- felony_convictions %>% filter(Tfelony==1)
################################################################################
felony_convictions <- felony_convictions %>% 
  select(`Full legal name`, DISTRICT, OFFICE, DOCKET, TYPEREG, DEFNO) %>% 
  inner_join(cr20to22)

notfc_co <- tPP %>% 
  anti_join(select(felony_convictions, DISTRICT, OFFICE, DOCKET, TYPEREG, DEFNO)) %>% 
  semi_join(cr20to22codefendants)

notfc_notco <- tPP %>% 
  anti_join(select(felony_convictions, DISTRICT, OFFICE, DOCKET, TYPEREG, DEFNO)) %>% 
  inner_join(cr20to22unique)

bigDF <- felony_convictions %>% bind_rows(notfc_co) %>% bind_rows(notfc_notco)
help <- anti_join(tPP, select(bigDF,`Full legal name`))
bigDF <- bigDF %>% bind_rows(help)
bigDF <- bigDF %>% filter(!duplicated(`Full legal name`))

bigDF[,names(bigDF) %in% c(names(cr20to22), names(tPP))] %>% 
  sheet_write(ss = mySheet, sheet = "Source data")
