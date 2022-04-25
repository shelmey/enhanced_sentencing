library(googlesheets4)
library(dplyr)

# Google sheet with the tPP population
tPP_sheet <- "https://docs.google.com/spreadsheets/d/1jSN5ueyn5UpgUwnjEIOJumQst7VIjuou6qrgHCNuhIM/edit"

# Read the tab with the IDB variables appended
fed_idb <- read_sheet(tPP_sheet, "Federal Cases w IDB cols") %>%
  mutate(terminated = TERMDATE > as.Date("1900-01-01"),# dummy variable for terminated
         sentenced = SENTDATE > as.Date("1900-01-01")) # dummy variable for sentenced


# Cases that have been sentenced
Sentenced <- fed_idb[fed_idb$sentenced,]

# Names of filing offense and termination offense level fields (1-5)
FOFFnames <- names(Sentenced)[grepl("FOFFLVL",names(Sentenced))]
TOFFnames <- names(Sentenced)[grepl("TOFFLVL",names(Sentenced))]
FSEVnames <- names(Sentenced)[grepl("FSEV",names(Sentenced))]
TSEVnames <- names(Sentenced)[grepl("TSEV",names(Sentenced))]

# Make a dummy variable for whether the charge levels increased between filing and sentencing 
Sentenced <- Sentenced %>% 
  within({
    for(n in 1:5){
      assign(paste0("LVLCHNG",n), get(FOFFnames[n])<get(TOFFnames[n]))
      assign(paste0("SEVCHNG",n), get(FSEVnames[n])<get(TSEVnames[n]))
      }})

# summarise to one dummy variable for whether any charge's level increased
enhanced_lvl <- unlist(apply(Sentenced[, grepl("LVLCHNG",names(Sentenced))],1,sum, na.rm = T))>0
enhanced_sev <- unlist(apply(Sentenced[, grepl("SEVCHNG",names(Sentenced))],1,sum, na.rm = T))>0

# There are just three cases where an offense level changed between filing and sentencing. 
# There are 26 where the severity changed. 
# What's their deal?
Sentenced$`Case ID`[enhanced]
Sentenced$`Short narrative`[enhanced] %>% cat(sep = "\n\n")
Sentenced$`Additional details`[enhanced] %>% cat(sep = "\n\n")
Sentenced$docketNumber[enhanced] %>% cat(sep = "\n\n")
