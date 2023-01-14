library(haven)
library(stringr)
library(reshape2)

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

felony_convictions <- read_sheet(mySheet, "Felony Convictions")

felony_convictions <- felony_convictions %>% filter(FELONY)

felony_conviction_co <- felony_convictions[, !names(felony_convictions) %in% c("SENTDATE", "PRISTOT", "CASLGKY",  "TTITLE1", "TOFFLVL1", "TTITLE2",
                                             "TOFFLVL2", "TTITLE3", "TOFFLVL3", "TTITLE4", "TOFFLVL4", "TTITLE5", "TOFFLVL5")] %>% inner_join(cr20to22DCcodefendants)

fc <- df %>% 
  inner_join(felony_convictions[, !names(felony_convictions) %in% c("Case Number", "SENTDATE", "PRISTOT", "CASLGKY",  "TTITLE1", "TOFFLVL1", "TTITLE2", "TOFFLVL2", "TTITLE3", "TOFFLVL3", "TTITLE4", "TOFFLVL4", "TTITLE5", "TOFFLVL5")])# %>% 
help <- fc %>% anti_join(cr20to22DC)

fc <- fc %>% inner_join(cr20to22DC)

notfc_co <- df %>% 
  anti_join(felony_convictions[, !names(felony_convictions) %in% c("Case Number", "Name", "SENTDATE", "PRISTOT", "CASLGKY",  "TTITLE1", "TOFFLVL1", "TTITLE2", "TOFFLVL2", "TTITLE3", "TOFFLVL3", "TTITLE4", "TOFFLVL4", "TTITLE5", "TOFFLVL5")]) %>% 
  semi_join(cr20to22DCcodefendants)

notfc_notco <- df %>% 
  anti_join(felony_convictions[, !names(felony_convictions) %in% c("Case Number", "Name", "SENTDATE", "PRISTOT", "CASLGKY",  "TTITLE1", "TOFFLVL1", "TTITLE2", "TOFFLVL2", "TTITLE3", "TOFFLVL3", "TTITLE4", "TOFFLVL4", "TTITLE5", "TOFFLVL5")]) %>% 
  inner_join(cr20to22DCunique)

bigDF <- fc %>% bind_rows(notfc_co) %>% bind_rows(notfc_notco)

bigDF <- bigDF %>% bind_rows(anti_join(df, bigDF))
bigDF <- bigDF %>% filter(!duplicated(Name))

bigDF[,names(bigDF) %in% c(names(cr20to22DC), names(df))] %>% 
  sheet_write(ss = mySheet, sheet = "Source data")
