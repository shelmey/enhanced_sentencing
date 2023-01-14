library(dplyr)
library(stringr)
library(googlesheets4)

getwd()
mySheet <- "1g_kBPD5BiAf0hhXjjLgJojgJy_1_HKOO--YkIRZJ_nA"
##################
### End Params ###
##################

tPP <- read_sheet(mySheet, "Source data")

# tPP <- tPP %>%
#   within({
#     federal <- Federal == "Federal"
#     })

# gs4_create(paste0("tPP cases as of ", tPP_updated))
# myDrive <- googledrive::drive_ls()

# tPP %>% sheet_write(mySheet, "Source data")
