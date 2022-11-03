library(httr)
library(dplyr)

myToken <- "b22efa2b23592979fa0fef3735a356df4c3723f9"

courtsURL <- "https://www.courtlistener.com/api/rest/v3/courts"
courtsResp <- GET(courtsURL ,
                  add_headers(Authorization = paste("Token", myToken)))

courtsContent <- content(courtsResp)
courtsResults <- courtsContent$results
Courts <- bind_rows(lapply(courtsResults, bind_cols))
more <- !is.null(courtsContent$`next`)

while(more){
  courtsResp <- GET(courtsContent$`next`,
                    add_headers(Authorization = paste("Token", myToken)))
  
  courtsContent <- content(courtsResp)
  courtsResults <- courtsContent$results
  courts <- bind_rows(lapply(courtsResults, bind_cols))
  Courts <- bind_rows(Courts, courts)
  more <- !is.null(courtsContent$`next`)
}

Courts %>% write.csv("courts.csv", row.names = F)
