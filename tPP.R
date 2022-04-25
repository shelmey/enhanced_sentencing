library(dplyr)
library(stringr)
getwd()
tPP_path <- "Inputs/tPPFall2020cases_2022-02-14.csv"

tPP <- read.csv(tPP_path, stringsAsFactors = F, strip.white = T)

tPP <- tPP %>%
  within({
    federal <- Federal == "Federal"
    })

# tPP %>% filter(grepl("vida", tolower(Full.legal.name)))

feds <- tPP %>% filter(federal)
feds$Case.ID %>% head(30)
feds$Docket <- feds$Case.ID %>% str_remove("_.*$")

table(feds$Docket %in% cr$DOCKET)
"2007180" %in% cr$DOCKET
head(cr$DOCKET)
"1200265" %in% cr$DOCKET
cr %>% filter(DOCKET == "1200265" & FISCALYR == 2020) %>% t


tPP$terror <- grepl("terror",tolower(paste(tPP$Additional.details,tPP$Short.narrative)))
