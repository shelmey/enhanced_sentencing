library(httr)
library(dplyr)
library(stringr)
library(googlesheets4)

pw = "cm3.5bAtM^}+"
username = "defendrichard@gmail.com"

jan6_sheet <- "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0"
jan6All <- read_sheet(jan6_sheet)
jan6 <- jan6All %>% filter(Felony)

myToken <- "b22efa2b23592979fa0fef3735a356df4c3723f9"

pleas <- read_sheet(jan6_sheet, "Plea Agreements")

pleas3a1.4 <- pleas %>% 
  filter(Count3A1.4 > 0) 

pleaNames <- pleas3a1.4$Name %>% unique


# queryURLdcd <- paste0("https://www.courtlistener.com/api/rest/v3/search/?q=&type=d&order_by=score%20desc&filed_after=01%2F05%2F2021&court=dcd")
# 
# respdcd <- GET(queryURLdcd ,
#             add_headers(Authorization = paste("Token", myToken)))
# 
# dcd <- content(respdcd)
# 
# bind_cols(dcd[["results"]][[17]]) %>% View

allResults = NULL

# federal$`Location: state`[!federal$`Location: state` %in% state.name]
for(n in pleaNames){
  myName <- toupper(paste(str_remove(n, "^.*, "), str_remove(n, ",.*$")))
  
  queryURL <- paste0("https://www.courtlistener.com/api/rest/v3/search/?q=&type=d&order_by=score%20desc&filed_after=01%2F05%2F2021",
                     "&party_name=", URLencode(myName),
                     # "&docketNumber=", jan6$docketNumber[jan6$Name == n],
                     "&court=dcd")
  
  resp <- GET(queryURL , add_headers(Authorization = paste("Token", myToken)))
  
  respContent <- content(resp)
  
  queryURL2 <- paste0("https://www.courtlistener.com/api/rest/v3/recap-fetch/?&ucourt=dcd&docket_number=", jan6$docketNumber[jan6$Name == n])
  
  resp2 <- POST(queryURL2 , add_headers(Authorization = paste("Token", myToken)))
  
  respContent2 <- content(resp2)
  
  
  
  docketMatch <- unlist(lapply(respContent[["results"]],function(x)x[["docketNumber"]] == jan6$docketNumber[jan6$Name == n]))
  
  myDockets <- respContent[["results"]][docketMatch]
  myIDs <- unlist(lapply(myDockets, function(x)x[["id"]]))
  myID <- min(myIDs)
  
  
  respEntries <- GET(paste0("https://www.courtlistener.com/api/rest/v3/docket-entries/?docket__id=", myID), add_headers(Authorization = paste("Token", myToken)))
  
  entriesContent <- content(respEntries)
  

  
resultsDF <- bind_rows(lapply(respContent$results, function(R1)cbind.data.frame(R1[unlist(lapply(R1, length))==1])))

defendants <- lapply(respContent$results, function(R1)unlist(R1$party[unlist(R1$party)!="USA"]))

resultsDF$defendant = defendants

resultsDF$`Case ID` <- federal$`Case ID`[n]

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

allResults <- bind_rows(allResults,resultsDF)
 }

allResults %>% select(-defendant) %>% write.csv("all results.csv", row.names = F)

allResults <- allResults %>% left_join(select(dc_codes, court, Code))

topResults <- allResults %>% filter(!duplicated(`Case ID`))

myFeds <- federal %>% left_join(select(topResults, `Case ID`, docketNumber, District = Code))
myFeds %>% select(-defendant) %>% write.csv(paste0("tPP Federal Cases w docketNumber ",Sys.Date(),".csv"), row.names = F)
table(myFeds$DOCKET == myFeds$docketNumber)

sheet_write(myFeds, tPP_sheet, "Federal Cases w Docket Numbers")

federal$`Case ID`[duplicated(federal$`Case ID`)]
table(federal$DOCKET[2:13]==topResults$docketNumber)
bad <- federal[2:13,][federal$DOCKET[2:13]!=topResults$docketNumber,]
