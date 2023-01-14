library(rvest)
library(dplyr)
library(stringr)
library(pdftools)
library(googlesheets4)
library(googledrive)

mySheet <- "12TXm_e9Hba-jv3g4GasbP2RLsfC-FtZpGWewcG8gJjg"

jan6 <- read_sheet(mySheet, "Source data")

terror_charges <- read.csv("Inputs/terror_titles.csv",stringsAsFactors = F, strip.white = T)

myTitles <- paste(terror_charges$Title, terror_charges$Section, sep = ":") %>% 
  str_remove_all(" .*$") %>%
  toupper() %>%
  str_replace_all("[)(]+","\\.") %>%
  str_remove_all("\\.$")

ttitles <- jan6[, grepl("TTITLE[0-9]", names(jan6))]
ftitles <- jan6[, grepl("FTITLE[0-9]", names(jan6))]

terrorMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"\\.") %in% str_remove_all(myTitles,"\\."))))

jan6$TTerror <- rowSums(terrorMat)>0
terrorMat <- apply(ftitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"\\.") %in% str_remove_all(myTitles,"\\."))))

jan6$FTerror <- rowSums(terrorMat)>0
dickCharges <- c("18:111","18:1361")
dickMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[^0-9\\:]") %in% dickCharges)))
jan6$TRH <- rowSums(dickMat)>0
jan6$TTerror  %>% table

jan6 <- read_sheet(mySheet, "Federal Cases w IDB cols")

PSEV <- jan6[,grepl("TSEV", names(jan6))] %>%
  apply(1, substr, 1, 1) %>% t %>%
  apply(1, function(x)gsub("[^ABC0-9]","",x)) %>% t %>%
  apply(1, function(x)ifelse(x=="",NA,x)) %>% t %>%
  as.data.frame %>%
  mutate_all(factor, levels = c("A","B", "C", as.character(0:9))) 

names(PSEV) <- paste0("P",names(PSEV))

jan6$PFSEVTOT <- PSEV[,grepl("PTSEV",names(PSEV))] %>%
  mutate_all(as.character) %>%
  mutate_all(function(x)case_when(is.na(x) ~ NA_real_, x == "A" ~ 0, x == "B" ~ 1, x == "C" ~ 2, TRUE ~ as.numeric(x)+3)) %>% 
  apply(1,max,na.rm = T)

disps <- cbind(Name = jan6$Name, jan6[, grepl("DISP[0-9]", names(jan6))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
TOFFLVL <- cbind(Name = jan6$Name, jan6[,grepl("TOFFLVL",names(jan6))])
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
  group_by(Name) %>%
  summarise(Tfelony = max(Tfelony,na.rm = T))

jan6 <- jan6 %>% left_join(disps_max)
jan6$Tfelony <- as.logical(jan6$Tfelony)

severe <- jan6 %>% filter(PFSEVTOT > 4 & !is.infinite(PFSEVTOT) & Tfelony)

plea_agreements <- read_sheet(mySheet, "Plea Agreements")

severe <- severe %>% left_join(select(plea_agreements, Name, Count3A1.4))

severe %>% filter(Count3A1.4 == 0) %>% 
  sheet_write(mySheet,"Severe Termination Offenses")
