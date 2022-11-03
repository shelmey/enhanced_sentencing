library(httr)
library(dplyr)
library(stringr)
library(googlesheets4)
# jan6df_in <- "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0"
# df<- read_sheet(jan6df_in)
# df$Federal[df$`Case ID` == "08282020_BMS"] <- "Non-Federal" 
TOFFLVL <- jan6_idb[,grepl("TOFFLVL",names(jan6_idb))]
jan6_idb$Felony <- apply(TOFFLVL,1,function(x){ifelse(sum(is.na(x))==5,NA,4 %in% x)})

federal <- jan6_idb %>% filter(Felony)

myToken <- "b22efa2b23592979fa0fef3735a356df4c3723f9"

allResults = NULL

# federal$`Location: state`[!federal$`Location: state` %in% state.name]
for(n in 1:251){
myState <- "DC"
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
