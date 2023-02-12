library(rvest)
library(dplyr)
library(stringr)
library(pdftools)
library(googlesheets4)
library(haven)
library(ggplot2)
base_url <- "https://www.justice.gov"
# Look for 'U.S.S.G. § 3A1.4'
jan6url <- "https://www.justice.gov/usao-dc/capitol-breach-cases"

gsUrl <- "12TXm_e9Hba-jv3g4GasbP2RLsfC-FtZpGWewcG8gJjg"

Plea_Agreements <- read_sheet(gsUrl, "Plea Agreements")
Source_Data <- read_sheet(gsUrl, "Source data")

# Plea_Agreements$Count3A1.4 %>% table

Felony_Convictions <- read_sheet(gsUrl, "Felony Convictions")

inputs <- dir("/home/sam/Projects/RichardDefense/enhanced_sentencing/Inputs",pattern = "cr2[0-2]_0\\.sas7bdat", full.names = T)

Plea_Agreements$SENTDATE %>% summary

plea_agreements <- Plea_Agreements %>% 
  select(Name, Count3A1.4) %>%
  left_join(Source_Data)


FOFFLVL <- plea_agreements[,grepl("FOFFLVL", names(plea_agreements))] %>%
  mutate_all(function(x)ifelse(x<0,NA,x))

plea_agreements$FOFFLVLmax <- apply(FOFFLVL, 1, max, na.rm = T)
table(plea_agreements$FOFFLVLmax)
tlvls <- plea_agreements[, grepl("TOFFLVL[0-9]", names(plea_agreements))]

DISP <- plea_agreements[, grepl("DISP[0-9]", names(plea_agreements))] %>% 
  mutate_all(function(x)case_when(x == -8 ~ NA_character_, is.na(x) ~ NA_character_, x == 4 ~ "Convicted/final plea of guilty", x == 1 ~ "Dismissed", x == 15 ~ "Dismissed without prejudice", x == 9 ~ "Convicted by jury after trial", x == 2 ~ "Acquitted by court", x == 11 ~ "Nolle prosequi", TRUE ~ as.character(x)) %>% factor(ordered = TRUE, levels = c("Convicted by jury after trial", "Convicted/final plea of guilty", "Acquitted by court", "Nolle prosequi", "Dismissed without prejudice", "Dismissed")))


felonyMat <- apply(tlvls, 2, function(x)x == 4) & apply(DISP,2,function(x)grepl("Convicted",x))
plea_agreements$tfelony <- rowSums(felonyMat)>0
plea_agreements$felony <- rowSums(apply(tlvls, 2, function(x)x == 4))>0
table(plea_agreements$felony)
help <- plea_agreements %>% filter(felony & !tfelony)

plea_agreements$DISPmax <-  apply(DISP, 1, function(x){x[order(x)][1]})

plea_agreements %>% 
  mutate(`Offense Level` = case_when(FOFFLVLmax == 3 ~ "Class A Misdemeanor",
                                     FOFFLVLmax == 1 ~ "Petty Offense",
                                     FOFFLVLmax == 4 ~ "Felony",
                                     TRUE ~ NA_character_ 
                                     
  )) %>%
  ggplot(aes(x = Count3A1.4, fill = `Offense Level`, group = `Offense Level`)) +
  geom_bar()

myPleaAgreements <- plea_agreements %>% 
  transmute(Name, 
            `Case Number`, 
            DISPmax, 
            `Reserved 3A1.4` = as.logical(Count3A1.4), 
            `Offense Level` = case_when(FOFFLVLmax == 3 ~ "Class A Misdemeanor",
                                        FOFFLVLmax == 1 ~ "Petty Offense",
                                        FOFFLVLmax == 4 | Name %in% Felony_Convictions$Name[Felony_Convictions$FELONY] ~ "Felony",
                                        TRUE ~ NA_character_ ),
            `Felony Conviction` = tfelony|Name %in% Felony_Convictions$Name[Felony_Convictions$FELONY])

# myPleaAgreements %>% sheet_write(gsUrl,"Plea Outcomes")
myPleaAgreements %>% group_by(`Reserved 3A1.4`,`Offense Level`,`Felony Conviction`) %>%
  summarise(Count = n())

plea_agreements_dedup <- myPleaAgreements %>%
  arrange(Name,`Case Number`, desc(`Reserved 3A1.4`)) %>% filter(!duplicated(paste(Name, `Case Number`)))

plea_agreements_dedup$`Reserved 3A1.4`[plea_agreements_dedup$`Offense Level` == "Felony"] %>% table
plea_agreements_dedup$`Reserved 3A1.4`[plea_agreements_dedup$`Felony Conviction`] %>% table
