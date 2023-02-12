myTables <- tabulizer::extract_tables("https://storage.courtlistener.com/recap/gov.uscourts.dcd.232699/gov.uscourts.dcd.232699.39.1.pdf")
myTables[[1]] %>% str
myTable <- myTables %>% lapply(as.data.frame) %>% bind_rows()

chaos <- myTable$V3 == myTable$V4 & myTable$V3 != ""

myTable$V4[chaos] <- myTable$V5[chaos]
myTable$V5[chaos] <- myTable$V6[chaos]
myTable$V6[chaos] <- NA
myTable$V6[myTable$V6 == ""] <- NA
table(is.na(myTable$V6))

myTable <- myTable[,1:5]
myNames <- as.character(myTable[1,])
myTable <- myTable[myTable$V2 != "Case Number",]

myNames <- gsub("\\\r"," ",myNames)
names(myTable) <- myNames

myTable$`Defendant Name` <- trimws(myTable$`Defendant Name`)

badBreak <- grepl(",$",myTable$`Defendant Name`)

myTable$`Defendant Name`[which(badBreak)] <- paste(myTable$`Defendant Name`[which(badBreak)], myTable$`Defendant Name`[which(badBreak)+1])
myTable$`Defendant Name`[which(badBreak)+1] <- ""
table(between(nchar(myTable$`Defendant Name`),1,4))

myTable$`Defendant Name`[which(myTable$`Defendant Name` == "IV")-1] <- paste(myTable$`Defendant Name`[which(myTable$`Defendant Name` == "IV")-1], myTable$`Defendant Name`[which(myTable$`Defendant Name` == "IV")])
myTable$`Defendant Name`[which(myTable$`Defendant Name` == "IV")] <- ""

rownames(myTable) <- 1:nrow(myTable)

unbreak <- which(myTable$`Defendant Name` != "")

fillme <- lapply(1:(length(unbreak)-1), function(n){unbreak[n+1]-1})

fillme <- c(fillme, nrow(myTable))
fillme <- unlist(fillme)
for(n in 1:length(unbreak)){
  myTable$`Offense of Conviction`[unbreak[n]] <- paste(myTable$`Offense of Conviction`[unbreak[n]:fillme[n]][myTable$`Offense of Conviction`[unbreak[n]:fillme[n]] != ""], collapse = ", ")
  myTable$`Government Recommendation`[unbreak[n]] <- paste(myTable$`Government Recommendation`[unbreak[n]:fillme[n]][myTable$`Government Recommendation`[unbreak[n]:fillme[n]] != ""], collapse = ", ")
  myTable$`Sentence Imposed`[unbreak[n]] <- paste(myTable$`Sentence Imposed`[unbreak[n]:fillme[n]][myTable$`Sentence Imposed`[unbreak[n]:fillme[n]] != ""], collapse = ", ")
  myTable$`Case Number`[unbreak[n]] <- paste(myTable$`Case Number`[unbreak[n]:fillme[n]], collapse = "")
  
}

myTable$`Offense of Conviction` <- gsub("§,", "§", myTable$`Offense of Conviction`)

myTable <- myTable[unbreak,]

myTable <- apply(myTable, 2, function(x)gsub("\\\r",", ",x))
myTable <- as.data.frame(myTable)
myTable <- apply(myTable, 2, function(x)gsub(",+",",",x))
myTable <- as.data.frame(myTable)

myTable %>% sheet_write(sheet = "Willden Misdemeanant Table")
