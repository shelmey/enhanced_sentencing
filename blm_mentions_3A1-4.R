library(rvest)
library(dplyr)
library(stringr)
library(pdftools)
library(googlesheets4)
library(googledrive)
gsUrl <- "1g_kBPD5BiAf0hhXjjLgJojgJy_1_HKOO--YkIRZJ_nA"

blm_idb <- read_sheet(gsUrl, "Federal Cases w IDB cols")
myPleaUrls <- blm_idb$`Plea agreement`
myPleaUrls <- myPleaUrls[!is.na(myPleaUrls)]

pleaTexts <- lapply(myPleaUrls, function(x){
  myPDF <- tempfile(fileext = ".pdf")
  googledrive::drive_download(x, myPDF)
  pdf_ocr_text(myPDF)
  }) 
# 3A1.4
# 2332
# terror

pleaTexts <- lapply(pleaTexts, paste, collapse = " ")
pleaTexts <- unlist(pleaTexts)
pleaTexts <- trimws(pleaTexts)
pleaTexts <- pleaTexts %>% str_replace_all("\\n"," ")
pleaTexts <- pleaTexts %>% str_replace_all("\\r"," ")

pleaTexts <- pleaTexts %>% str_replace_all(" +"," ")
pleaTextsSum <- lapply(pleaTexts,function(x)sum(grepl("3A1\\.4",x), na.rm = T))

blm_idb$Count3553[!is.na(blm_idb$`Plea agreement`)] <- unlist(pleaTextsSum)

blm_idb %>% select(`Case ID`, `Name of case`, docketNumber, District, Count3553) %>%
  sheet_write(ss = gsUrl, sheet = "Count 3553")

