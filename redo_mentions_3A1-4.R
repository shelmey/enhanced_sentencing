library(rvest)
library(dplyr)
library(stringr)
library(pdftools)
library(googlesheets4)

base_url <- "https://www.justice.gov"
# Look for 'U.S.S.G. § 3A1.4'
jan6url <- "https://www.justice.gov/usao-dc/capitol-breach-cases"

gsUrl <- "12TXm_e9Hba-jv3g4GasbP2RLsfC-FtZpGWewcG8gJjg"
oldSheet <- "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0"
jan6_idb <- read_sheet(gsUrl)

# parse with rvest
myLinks <- jan6url %>% 
  read_html() %>%
  html_nodes("a") 

myUrls <- myLinks %>% 
  html_attr('href')

myLinks <- myLinks %>% 
  html_text()

myUrls <- paste0(base_url, myUrls)

myPleaUrls <- myUrls[grepl("Plea Agreement", myLinks)]

pleaTexts <- lapply(myPleaUrls, pdf_text) 


nPleaAgreements <- str_count(jan6_idb$`Case Documents`, "Plea Agreement") 
nPleaAgreements[is.na(nPleaAgreements)] <- 0
pleaNames <- unlist(lapply(1:nrow(jan6_idb),function(n)rep(jan6_idb$Name[n],nPleaAgreements[n])))

Plea_Terror <- cbind.data.frame(Name = pleaNames, url = myPleaUrls)

DateRegex <- paste(paste0(month.name," [0-9]{1,2}, +202[0-2]{1}"), collapse = "|")

Plea_Terror$Date <- unlist(lapply(pleaTexts,function(x)str_extract(x[1], DateRegex)))

noDate <- is.na(Plea_Terror$Date)

noDateText <- lapply(myPleaUrls[noDate], pdf_ocr_text)

Plea_Terror$Date[noDate] <- unlist(lapply(noDateText,function(x)str_extract(x[1], DateRegex)))
for(n in 1:length(pleaTexts[noDate])){
  pleaTexts[noDate][[n]]<-noDateText[[n]]}

pleaTexts <- lapply(pleaTexts, paste, collapse = " ")
pleaTexts <- unlist(pleaTexts)
pleaTexts<-trimws(pleaTexts)
pleaTexts <- pleaTexts %>% str_replace_all("\\n"," ")
pleaTexts <- pleaTexts %>% str_replace_all("\\r"," ")

pleaTexts <- pleaTexts %>% str_replace_all(" +"," ")
pleaTextsSum <- lapply(pleaTexts,function(x)sum(grepl("3A1\\.4",x), na.rm = T))

Plea_Terror$Count3A1.4 <- unlist(pleaTextsSum)

sentences3A1.4 <- lapply(pleaTexts, function(x)str_extract_all(x,"[^\\.]+U\\.S\\.S\\.G\\. § 3A1\\.4, n\\. 4\\. [^\\.]+\\.")) 

sentences3A1.4 <- sentences3A1.4 %>% lapply(str_remove_all, "Page [0-9]{1,2} of [0-9]{1,2} Case [0-9]{1}\\:[0-9]{1,2}-[crmj]{2}-[0-9]{5}-{0,1}[A-Z]{0,3} Document [0-9]{1,3} Filed [0-9]{2}/[0-9]{2}/[0-9]{2,4} Page [0-9]{1,2} of [0-9]{1,2} ")
sentences3A1.4 <- sentences3A1.4 %>% lapply(str_remove_all, " \\|")
TOFFLVL <- jan6_idb[,grepl("TOFFLVL",names(jan6_idb))]
TOFFLVL$Felony <- apply(TOFFLVL,1,function(x){ifelse(sum(is.na(x))==5,NA,4 %in% x)})
TOFFLVL$Name <- jan6_idb$Name
Plea_Terror <- Plea_Terror %>% left_join(TOFFLVL)
noTerror_Felonies <- Plea_Terror %>% filter(Felony & Count3A1.4==0)

library(reshape2)

jan6All <- read_sheet(gsUrl, "Source data")

disps <- cbind(Name = jan6All$Name, jan6All[, grepl("DISP[0-9]", names(jan6All))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
TOFFLVL <- cbind(Name = jan6All$Name, jan6All[,grepl("TOFFLVL",names(jan6All))])
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

Plea_Terror <- Plea_Terror %>% left_join(disps_max)
Plea_Terror$Tfelony <- as.logical(Plea_Terror$Tfelony)



TSEV <- cbind(Name = jan6All$Name, jan6All[, grepl("TSEV[0-9]", names(jan6All))])
names(TSEV) <- names(TSEV) %>% str_remove_all("TSEV")
TSEV <- TSEV %>% melt(id = "Name")
names(TSEV)[2] <- "Offense"
names(TSEV)[3] <- "TSEV"
TSEV$Type <- as.numeric(substr(TSEV$TSEV,2,2))
disps <- disps %>% left_join(TSEV)

type_max <- disps %>%
  filter(DISP %in% c(4,9)) %>%
  group_by(Name) %>%
  summarise(Type = max(Type,na.rm = T))

types <- cbind.data.frame(Type = c(0:3),Offense_Type = c("None","Moral Terpitude","Property","Personal"))

type_max <- type_max %>% left_join(types)

Plea_Terror <- Plea_Terror %>% left_join(type_max)

oldPleas <- read_sheet(oldSheet, sheet = "Plea Agreements")
which(names(oldPleas) == "PACER last checked")

oldPleas <- oldPleas[,c(1,15:length(oldPleas))]
oldPleas <- oldPleas %>% select(-`Transcript has 3A1.4`)
Plea_Terror <- Plea_Terror %>% left_join(oldPleas)
Plea_Terror <- Plea_Terror %>% left_join(select(jan6All, Name, SENTDATE))
Plea_Terror %>%
  sheet_write(ss = gsUrl, sheet = "Plea Agreements")

