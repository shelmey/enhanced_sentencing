library(googlesheets4)
library(reshape2)
mySheet <- "1g_kBPD5BiAf0hhXjjLgJojgJy_1_HKOO--YkIRZJ_nA"


tpp <- mySheet %>% read_sheet(mySheet, "Felony Convictions 2")
fed_idb <- tpp
### ID Felony Convictions ###
disps <- cbind(`Case ID` = fed_idb$`Case ID`, fed_idb[, grepl("DISP[0-9]", names(fed_idb))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
TOFFLVL <- cbind(`Case ID` = fed_idb$`Case ID`, fed_idb[,grepl("TOFFLVL",names(fed_idb))])
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
  group_by(`Case ID`) %>%
  summarise(Tfelony = max(Tfelony,na.rm = T))

fed_idb <- fed_idb %>% left_join(disps_max)

terror_charges <- read.csv("Inputs/terror_titles.csv",stringsAsFactors = F, strip.white = T)

myTitles <- paste(terror_charges$Title, terror_charges$Section, sep = ":") %>% 
  str_remove_all(" .*$") %>%
  toupper() %>%
  str_replace_all("[)(]+","\\.") %>%
  str_remove_all("\\.$")

ttitles <- fed_idb[, grepl("TTITLE[0-9]", names(fed_idb))]
DISPS <- fed_idb[, grepl("DISP[0-9]", names(fed_idb))]
ftitles <- fed_idb[, grepl("FTITLE[0-9]", names(fed_idb))]
for(n in 1:5)ttitles[,n][!(DISPS[,n]==4|DISPS[,n]==9)] <- NA

terrorMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[\\.A-Z].*$") %in% str_remove_all(myTitles,"[\\.A-Z].*$"))))

terrorMat <- apply(ttitles, 2, function(x){
  unlist(lapply(x,function(y){
    sum(unlist(lapply(myTitles, 
                      function(x){
                        grepl(gsub("\\.","",x),gsub("\\.","",y)) & (gsub("[\\.A-Z].*$","",y) == gsub("[\\.A-Z].*$","",x))
                        })),na.rm = T)>0 
    }
    )
    )
  }
  )

fed_idb$TTerror <- rowSums(terrorMat)>0
terrorMat <- apply(ftitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[\\.A-Z].*$") %in% str_remove_all(myTitles,"[\\.A-Z].*$"))))

terrorMat <- apply(ftitles, 2, function(x){
  unlist(lapply(x,function(y){
    sum(unlist(lapply(myTitles, 
                      function(x){
                        grepl(gsub("\\.","",x),gsub("\\.","",y)) & (gsub("[\\.A-Z].*$","",y) == gsub("[\\.A-Z].*$","",x))
                      })),na.rm = T)>0 
  }
  )
  )
}
)
fed_idb$FTerror <- rowSums(terrorMat)>0
dickCharges <- c("18:111","18:1361")
dickMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[^0-9\\:]") %in% dickCharges)))
fed_idb$TRH <- rowSums(dickMat)>0

# terrorMat <- apply(ftitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[A-Z\\.].*$") %in% str_remove_all(myTitles,"[A-Z\\.].*$"))))
# fed_idb$MaybeFTerror <- rowSums(terrorMat)>0
# terrorMat <- apply(ttitles, 2, function(x)unlist(lapply(x,function(y)str_remove_all(y,"[A-Z\\.].*$") %in% str_remove_all(myTitles,"[A-Z\\.].*$"))))
# fed_idb$MaybeTTerror <- rowSums(terrorMat)>0

fed_idb %>% sheet_write(mySheet, "Felony Convictions 2")
