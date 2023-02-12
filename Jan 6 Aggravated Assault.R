library(googlesheets4)
library(dplyr)
library(stringr)

jan6_sheet <- "12TXm_e9Hba-jv3g4GasbP2RLsfC-FtZpGWewcG8gJjg"
alljan6 <- read_sheet(jan6_sheet, "Source data working")
jan6 <- jan6_sheet %>% read_sheet("Source data")

# Read courts
{courts <- read.csv("courts.csv", stringsAsFactors = F, strip.white = T) %>% 
    select(court_id = id, court_full_name = full_name, Court = short_name, DISTRICT = fjc_court_id) %>% 
    filter(!duplicated(DISTRICT))
}

jan6 <- jan6 %>% left_join(courts)

fc2 <- jan6_sheet %>% 
  read_sheet("Felony Convictions") %>%
  within({
    violent <- Violent
    destructive <- Destructive
    Destructive <- case_when(is.na(Destructive) ~ "",
                             !is.na(Destructive) & Destructive ~ "Yes",
                             !is.na(Destructive) & !Destructive ~ "No")
    Violent <- case_when(is.na(Violent) ~ "",
                         !is.na(Violent) & Violent ~ "Yes",
                         !is.na(Violent) & !Violent ~ "No")})

# Aggravated Assault Titles
{aatitles <- c("18:111", 
               "18:112", 
               "18:113(a)(2)", 
               "18:113(a)(3)", 
               "18:113(a)(6)",
               "18:113(a)(8)", 
               "18:113(a)(3)", 
               "18:114", 
               "18:115(a)", 
               "18:115(b)(1)", 
               "18:351(e)", 
               "18:1751(e)", 
               "18:1841(a)(2)(C)", 
               "18:1992(a)(7)", 
               "18:2199", 
               "18:2291", 
               "18:2332b(a)(1)", 
               "18:2340A")
  }


myTitles <- aatitles %>% 
  str_remove_all(" .*$") %>%
  toupper() %>%
  str_replace_all("[)(]+","\\.") %>%
  str_remove_all("\\.$")

ttitles <- jan6[, grepl("TTITLE[0-9]", names(jan6))]
ftitles <- jan6[, grepl("FTITLE[0-9]", names(jan6))]

DISP <- jan6[, grepl("DISP[0-9]", names(jan6))] %>% 
  mutate_all(function(x)case_when(x == -8 ~ NA_character_, is.na(x) ~ NA_character_, x == 4 ~ "Convicted/final plea of guilty", x == 1 ~ "Dismissed", x == 15 ~ "Dismissed without prejudice", x == 9 ~ "Convicted by jury after trial", x == 2 ~ "Acquitted by court", x == 9 ~ "Convicted by jury after trial", x == 11 ~ "Nolle prosequi", TRUE ~ as.character(x)) %>% factor(ordered = TRUE, levels = c("Convicted by jury after trial", "Convicted/final plea of guilty", "Acquitted by court", "Nolle prosequi", "Dismissed without prejudice", "Dismissed")))


aaMat <- apply(ttitles, 1, function(x)unlist(lapply(x,function(y)str_extract_all(y,"18\\:[0-9]+") %in% str_extract_all(myTitles,"18\\:[0-9]+")))) #& apply(DISP,2,function(x)grepl("Convicted",x))

TOFFLVL <- jan6[,grepl("TOFFLVL", names(jan6))]
TFelony <- TOFFLVL == 4 & apply(DISP,2,function(x)grepl("Convicted",x))

TwoFelonies <- rowSums(TFelony)>1

# aaMat[1][unlist(aaMat[1] %>% lapply(is.null))] <- NA
aaMat <- aaMat %>% bind_cols()
aa <- rowSums(aaMat)>0
table(aa)
jan6$Name[aa & TwoFelonies]

assaults <- fc2 %>% filter(Name %in% jan6$Name[aa])


aaMat <- apply(ttitles, 1, function(x)unlist(lapply(x,function(y)str_extract_all(y,"18\\:[0-9]+") %in% str_extract_all(myTitles,"18\\:[0-9]+")))) 
# aaMat[1][unlist(aaMat[1] %>% lapply(is.null))] <- NA
aaMat <- aaMat %>% bind_rows()
aaMat <- aaMat & apply(DISP,2,function(x)grepl("Convicted",x))
aa <- rowSums(aaMat)>0
table(aa)
jan6$Name[aa & !is.na(jan6$SENTDATE) & as.Date(jan6$SENTDATE) > as.Date("2021-01-06")]

willdenTable <- read_sheet("1k7KxDCjrlJxPl4Wl2zoZtC3sDNfVCiM7dwuw_0uU3xY", skip = 3)
willdenTable$NAME <- toupper(willdenTable$`Defendant Name`) 
jan6$NAME <- toupper(jan6$Name)
table(jan6$NAME %in% willdenTable$NAME)

# Get the max sentence for each case
jan6$myPRISTOT <- apply(jan6[,grepl("PRISTIM",names(jan6))],1,max, na.rm = T)
jan6$myPRISTOT[jan6$myPRISTOT==-1] <- 0  
jan6$myPRISTOT[jan6$myPRISTOT<0] <- NA  

jan6$myPROBTOT <- apply(jan6[,grepl("PROBMON",names(jan6))],1,max, na.rm = T)
jan6$myPROBTOT[jan6$myPROBTOT==-1] <- 0  
jan6$myPROBTOT[jan6$myPROBTOT<0] <- NA  
ttitles$ID <- 1:nrow(ttitles)
ttitles$TTITLE1 <- unlist(lapply(ttitles$TTITLE1, function(x)ifelse(is.null(x),NA,x)))
TTITLES <- melt(ttitles, id = "ID") 
DISP$ID <- 1:nrow(DISP)
DISPS <- melt(DISP, id = "ID") 
names(DISPS) <- paste0("d",names(DISPS))
TTITLES <- TTITLES %>% cbind(DISPS)

TTITLES <- TTITLES %>% 
  filter(dvalue %in% c("Convicted/final plea of guilty", "Convicted by jury after trial"))
TTITLES <- TTITLES %>% group_by(ID) %>%
  summarise(Offenses = paste(value, collapse = ", "))
jan6$Offenses <- NA

jan6$Offenses[TTITLES$ID] <- TTITLES$Offenses

ctitles <- titles == 4 & apply(DISP,2,function(x)grepl("Convicted",x))


myAssaults <- jan6 %>%
  left_join(select(fc2, Name, `Government Recommendation (months)`)) %>%
  filter(aa & !is.na(SENTDATE) & as.Date(SENTDATE) > as.Date("2021-01-06")) %>%
  transmute(Name,
         `Case Number`,
         `Convicted Offenses` = Offenses,
         `Government Recommendation (months)`,
         `Prison Time (months)` = myPRISTOT
         # `Probation (months)` = myPROBTOT,
         # PROBTOT,
         # Fine = scales::dollar(FINETOT)
         )

myAssaults %>% write_sheet(ss = jan6_sheet, sheet = "Assaults for Affidavit")


# Felony Filings
crJan6 <- cr20to22DC %>%
  filter(paste(DOCKET, TYPEREG) %in% paste(jan6$DOCKET, jan6$TYPEREG))
FOFFLVL <- crJan6[,grepl("FOFFLVL", names(crJan6))]
FFelony <- FOFFLVL == 4
FFelonies <- rowSums(FFelony)
table(FFelonies>0)

DISP <- crJan6[, grepl("DISP[0-9]", names(crJan6))] %>% 
  mutate_all(function(x)case_when(x == -8 ~ NA_character_, is.na(x) ~ NA_character_, x == 4 ~ "Convicted/final plea of guilty", x == 1 ~ "Dismissed", x == 15 ~ "Dismissed without prejudice", x == 9 ~ "Convicted by jury after trial", x == 2 ~ "Acquitted by court", x == 9 ~ "Convicted by jury after trial", x == 11 ~ "Nolle prosequi", TRUE ~ as.character(x)) %>% factor(ordered = TRUE, levels = c("Convicted by jury after trial", "Convicted/final plea of guilty", "Acquitted by court", "Nolle prosequi", "Dismissed without prejudice", "Dismissed")))

TOFFLVL <- crJan6[,grepl("TOFFLVL", names(crJan6))]
TFelony <- TOFFLVL == 4 & apply(DISP,2,function(x)grepl("Convicted",x))
table(rowSums(TFelony)>0)


# Felony Disps
jan6$DISPmax <-  apply(DISP, 1, function(x){x[order(x)][1]})

fc2 <- fc2 %>% left_join(select(jan6, Name, DISPmax))
table(fc2$DISPmax[fc2$FELONY])

