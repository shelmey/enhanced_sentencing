
myTables <- tabulizer::extract_tables("https://storage.courtlistener.com/recap/gov.uscourts.dcd.226943/gov.uscourts.dcd.226943.111.0.pdf")

myTables <- myTables[3:length(myTables)]

myTable <- myTables %>% 
  lapply(as.data.frame) %>% 
  bind_rows()

myNames <- as.character(myTable[1,])
myTable <- myTable[myTable$V2 != "Charge",]

myNames[1] <- 'Defendant & Case Number'

names(myTable) <- myNames

badBreak <- grepl("[,-]$",myTable$`Defendant & Case Number`)
badBreak[1] <- TRUE

myTable$`Defendant & Case Number`[which(badBreak)] <- paste(myTable$`Defendant & Case Number`[which(badBreak)], myTable$`Defendant & Case Number`[which(badBreak)+1])
myTable$`Defendant & Case Number`[which(badBreak)+1] <- ""
table(between(nchar(myTable$`Defendant & Case Number`),1,4))

myTable$`Defendant & Case Number` <- gsub("- ","-", myTable$`Defendant & Case Number`)

rownames(myTable) <- 1:nrow(myTable)

unbreak <- which(myTable$`Defendant & Case Number` != "")

fillme <- lapply(1:(length(unbreak)-1), function(n){unbreak[n+1]-1})

fillme <- c(fillme, nrow(myTable))
fillme <- unlist(fillme)

for(n in 1:length(unbreak)){
  myTable$`Offense Conduct`[unbreak[n]] <- paste(myTable$`Offense Conduct`[unbreak[n]:fillme[n]][myTable$`Offense Conduct`[unbreak[n]:fillme[n]] != ""], collapse = " ")
  myTable$Sentence[unbreak[n]] <- paste(myTable$Sentence[unbreak[n]:fillme[n]][myTable$Sentence[unbreak[n]:fillme[n]] != ""], collapse = " ")
  myTable$Charge[unbreak[n]] <- paste(myTable$Charge[unbreak[n]:fillme[n]][myTable$Charge[unbreak[n]:fillme[n]] != ""], collapse = " ")
  
}

myTable <- myTable[unbreak,]

myTable <- apply(myTable, 2, function(x)gsub("\\\r",", ",x))
myTable <- as.data.frame(myTable)
myTable <- apply(myTable, 2, function(x)gsub(",+",",",x))
myTable <- as.data.frame(myTable)

myTable %>% 
  sheet_write(ss = "1k7KxDCjrlJxPl4Wl2zoZtC3sDNfVCiM7dwuw_0uU3xY",
              sheet = "Hale-Cusanelli Misdemeanants")

