library(rvest)
library(dplyr)
library(stringr)
library(pdftools)
library(googlesheets4)

base_url <- "https://www.justice.gov"
# Look for 'U.S.S.G. § 3A1.4'
jan6url <- "https://www.justice.gov/usao-dc/capitol-breach-cases"

gsUrl <- "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0"

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


nPleaAgreements <- str_count(df$`Case Documents`, "Plea Agreement") 
nPleaAgreements[is.na(nPleaAgreements)] <- 0
pleaNames <- unlist(lapply(1:nrow(df),function(n)rep(df$Name[n],nPleaAgreements[n])))

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
Plea_Terror %>%
  sheet_write(ss = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", sheet = "Plea Agreements")

