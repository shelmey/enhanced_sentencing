library(httr)
library(dplyr)
library(stringr)
library(googlesheets4)

mySheet <- "1g_kBPD5BiAf0hhXjjLgJojgJy_1_HKOO--YkIRZJ_nA"
old_tPP_sheet <- "https://docs.google.com/spreadsheets/d/1jSN5ueyn5UpgUwnjEIOJumQst7VIjuou6qrgHCNuhIM/edit"

Courts <- read.csv("courts.csv", stringsAsFactors = F, strip.white = T)
dc_codes <- read.csv("districts.csv", stringsAsFactors = F, strip.white = T)
dc_codes$court <-paste("District Court,", dc_codes$short_name)
FD <- Courts %>% filter(jurisdiction == "FD")

df <- read_sheet(old_tPP_sheet, "Federal Cases w Docket Numbers")
df$Federal[df$`Case ID` == "08282020_BMS"] <- "Non-Federal" 
feds <- df %>% filter(Federal == "Federal")
feds <- feds %>% filter(!`Case ID` %in% c("07242020_RBH"))
feds$`Full legal name`[feds$`Full legal name`=="Rebecca Gonzales-Mota"] <- "Rebecca Mota Gonzalez"

feds <- feds %>% select(`Case ID`, docketNumber, District)

tPP <- read_sheet(mySheet, "Source data")

federal <- tPP %>% filter(federal)

# table(df$`Case ID` %in% tPP$Case.ID)
federal <- federal %>% left_join(feds)
# tPP$terror <- grepl("terror",tolower(paste(tPP$Additional.details,tPP$Short.narrative)))
cached <- federal %>% filter(!is.na(docketNumber))
federal <- federal %>% filter(is.na(docketNumber))

allResults = NULL

# federal$`Location: state`[!federal$`Location: state` %in% state.name]
for(n in 3:nrow(federal)){
myState <- federal$`Location: state`[n]
myDefendant <- federal$`Full legal name`[n]
myDefendantFL <- paste(str_remove(federal$`First name`[n], " .*$"),federal$`Family name`[n])
myDefendantFL2 <- paste(str_remove(federal$`Full legal name`[n], " .*$"),str_remove(federal$`Full legal name`[n],"^.* "))
myDefendantFL3 <- gsub("-"," ",myDefendantFL2)
myDefendantFL4 <- gsub("'","",myDefendantFL)

myCourts <- FD$id[str_detect(FD$full_name, myState)]

queryURL <- paste0("https://www.courtlistener.com/api/rest/v3/search/?q=&type=d&order_by=score%20desc&filed_after=01%2F01%2F2020",
                   "&party_name=", URLencode(myDefendant),
                   "&court=", paste(myCourts, collapse = "%20"))

resp <- GET(queryURL ,
            add_headers(Authorization = paste("Token", myToken)))

respContent <- content(resp)
if(length(respContent$results) == 0 & myDefendantFL4!=myDefendant){
  queryURL <- paste0("https://www.courtlistener.com/api/rest/v3/search/?q=&type=d&order_by=score%20desc&filed_after=01%2F01%2F2020",
                     "&party_name=", URLencode(myDefendantFL4),
                     "&court=", paste(myCourts, collapse = "%20"))
  
  resp <- GET(queryURL ,
              add_headers(Authorization = paste("Token", myToken)))
  
  respContent <- content(resp)
  
}

if(length(respContent$results) == 0 & myDefendant!=myDefendantFL){
  queryURL <- paste0("https://www.courtlistener.com/api/rest/v3/search/?q=&type=d&order_by=score%20desc&filed_after=01%2F01%2F2020",
                     "&party_name=", URLencode(myDefendantFL),
                     "&court=", paste(myCourts, collapse = "%20"))
  
  resp <- GET(queryURL ,
              add_headers(Authorization = paste("Token", myToken)))
  
  respContent <- content(resp)
  
}

if(length(respContent$results) == 0 & myDefendantFL2!=myDefendantFL){
  queryURL <- paste0("https://www.courtlistener.com/api/rest/v3/search/?q=&type=d&order_by=score%20desc&filed_after=01%2F01%2F2020",
                     "&party_name=", URLencode(myDefendantFL2),
                     "&court=", paste(myCourts, collapse = "%20"))
  
  resp <- GET(queryURL ,
              add_headers(Authorization = paste("Token", myToken)))
  
  respContent <- content(resp)
  
}

resultsDF <- bind_rows(lapply(respContent$results, function(R1)cbind.data.frame(R1[unlist(lapply(R1, length))==1])))

defendants <- lapply(respContent$results, function(R1)unlist(R1$party[unlist(R1$party)!="USA"]))

resultsDF$defendant = defendants

resultsDF$`Case ID` <- federal$`Case ID`[n]
if(nrow(resultsDF)>0){
resultsDF <- resultsDF %>%
  within({
    CR = str_detect(docketNumber, "-cr-")
    if(exists("terminated")){terminated = !is.na(dateTerminated)}else{terminated = FALSE}
    exact_match = myDefendant %in% defendant
    close_match = myDefendantFL %in% defendant}) %>%
  arrange(desc(exact_match),
          desc(close_match),
          desc(CR),
          desc(terminated))

allResults <- bind_rows(allResults,resultsDF)}
 }

allResults %>% select(-defendant) %>% write.csv("all results.csv", row.names = F)

allResults <- allResults %>% left_join(select(dc_codes, court, Code))

topResults <- allResults %>% filter(!duplicated(`Case ID`))

myFeds <- federal %>% 
  select(-District) %>%
  left_join(select(topResults, `Case ID`, docketNumber2 = docketNumber, District = Code)) %>%
  mutate(docketNumber = coalesce(docketNumber, docketNumber2)) %>% 
  select(-docketNumber2) %>% 
  rbind(cached)


sheet_write(myFeds, mySheet, "Federal Cases w Docket Numbers")

# federal$`Case ID`[duplicated(federal$`Case ID`)]
# table(federal$DOCKET[2:13]==topResults$docketNumber)
# bad <- federal[2:13,][federal$DOCKET[2:13]!=topResults$docketNumber,]
