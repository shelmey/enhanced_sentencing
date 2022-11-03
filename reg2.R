library(haven)
library(dplyr)
library(googlesheets4)
# Need to discount the severity for conspiracy since the available max sentence is that of the offense they conspired to commit
setwd('/home/sam/Projects/RichardDefense/enhanced_sentencing')

cr17to22_in <- "Inputs/cr17to22.sas7bdat"
tPP_sheet <- "https://docs.google.com/spreadsheets/d/1jSN5ueyn5UpgUwnjEIOJumQst7VIjuou6qrgHCNuhIM/edit"

# Sentencing Guidelines from FJC IDB codebook
guidelines <- 
  rbind.data.frame(cbind("A", 0, 0 ),
                   cbind("B", 0, 6),
                   cbind("C", 7, 12),
                   cbind("0", 12, 24),
                   cbind("1", 24, 36),
                   cbind("2", 48, 60),
                   cbind("3", 72, 120),
                   cbind("4", 132, 180),
                   cbind("5", 192, 240),
                   cbind("6", 252, 300),
                   cbind("7", 300, 1200),
                   cbind("8", 10000, 10000),
                   cbind("9", 10000, 100000),
                   cbind("Z", NA, NA))
names(guidelines) <- c("SEV", "low", "high")
guidelines$low <- as.numeric(guidelines$low)
guidelines$high <- as.numeric(guidelines$high)

cr20to22 <- cr17to22_in %>%
  read_sas() %>% 
  filter(FILEDATE > as.Date("2020-01-01")) 
cr20to22 <- cr20to22 %>%
  arrange(paste(DISTRICT, DOCKET, OFFICE),
          desc(LOADDATE)) %>%
  filter(!duplicated(paste(DISTRICT, DOCKET, OFFICE)))


source("jan6.R")

df <- df %>%
  within({
    DOCKET <- paste0(`Case Number` %>% str_remove_all("^.*\\:") %>% str_remove_all("-.*$"),
                     `Case Number` %>% str_remove_all("^.*-") %>% str_pad(5, "left", "0"))
    
    DOCKET[nchar(DOCKET) < 7] <- paste0("2",DOCKET[nchar(DOCKET) < 7])
    DOCKET[nchar(DOCKET) > 7] <- NA
    
  })




jan6_idb <- df %>% 
  filter(grepl("cr",tolower(`Case Number`))) %>%
  left_join(filter(cr20to22, DISTRICT == "90" & OFFICE == "1"))

rm(df)

tPP <- tPP_sheet %>% read_sheet("Federal Cases w IDB cols 20220616")
tTitles <- c(tPP$TTITLE1,tPP$TTITLE2,tPP$TTITLE3,tPP$TTITLE4,tPP$TTITLE5)
j6Titles <- c(jan6_idb$TTITLE1, jan6_idb$TTITLE2, jan6_idb$TTITLE3, jan6_idb$TTITLE4, jan6_idb$TTITLE5)
tTitles <- tTitles[tTitles != -8]
tTitles <- tTitles[!is.na(tTitles)]
j6Titles <- j6Titles[j6Titles != -8]
j6Titles <- j6Titles[!is.na(j6Titles)]

DISP <- cr20to22[, grepl("DISP[0-9]", names(cr20to22))] %>% 
  mutate_all(function(x)case_when(x == -8 ~ NA_character_, is.na(x) ~ NA_character_, x == 4 ~ "Convicted/final plea of guilty", x == 1 ~ "Dismissed", x == 15 ~ "Dismissed without prejudice", x == 9 ~ "Convicted by jury after trial", x == 2 ~ "Acquitted by court", x == 9 ~ "Convicted by jury after trial", x == 11 ~ "Nolle prosequi", TRUE ~ as.character(x)) %>% factor(ordered = TRUE, levels = c("Convicted by jury after trial", "Convicted/final plea of guilty", "Acquitted by court", "Nolle prosequi", "Dismissed without prejudice", "Dismissed")))

cr20to22$DISPmax <-  apply(DISP, 1, function(x){x[order(x)][1]})

crSample <- cr20to22 %>%
  filter(DISPmax %in% c("Convicted by jury after trial", "Convicted/final plea of guilty"))
  # filter(!CASLGKY %in% tPP$CASLGKY)
  # filter(CASLGKY %in% tPP$CASLGKY | TTITLE1 %in% tTitles| TTITLE2 %in% tTitles | TTITLE3 %in% tTitles | TTITLE4 %in% tTitles | TTITLE5 %in% tTitles)

# crSample %>% group_by(TTITLE1) %>% summarise(Count = n()) %>% arrange(desc(Count))

# Take the variables related to severity of filing and termination offenses, and get just the prison term part of that coded variable (the first character)
PSEV <- crSample[,grepl("SEV", names(crSample))] %>%
  apply(1, substr, 1, 1) %>% t %>%
  apply(1, function(x)gsub("[^ABC0-9]","",x)) %>% t %>%
  apply(1, function(x)ifelse(x=="",NA,x)) %>% t %>%
  as.data.frame %>%
  mutate_all(factor, levels = c("A","B", "C", as.character(0:9))) 
names(PSEV) <- paste0("P",names(PSEV))

# Get the sum of the severity indicators for each charge
crSample$PFSEVTOT <- PSEV[,grepl("PFSEV",names(PSEV))] %>%
  mutate_all(function(x)case_when(is.na(x) ~ NA_real_, x == "A" ~ 0, x == "B" ~ 1, x == "C" ~ 2, TRUE ~ as.numeric(x)+3)) %>% 
  rowSums(na.rm = T)

crSample$PFSEVMAX <- PSEV[,grepl("PFSEV",names(PSEV))] %>%
  mutate_all(function(x)case_when(is.na(x) ~ NA_real_, x == "A" ~ 0, x == "B" ~ 1, x == "C" ~ 2, TRUE ~ as.numeric(x)+3)) %>% 
  apply(1,max,na.rm = T) %>% unlist

# Get the sum of the severity indicators for each charge
crSample$PTSEVTOT <- PSEV[,grepl("PTSEV",names(PSEV))] %>%
  mutate_all(function(x)case_when(is.na(x) ~ NA_real_, x == "A" ~ 0, x == "B" ~ 1, x == "C" ~ 2, TRUE ~ as.numeric(x)+3)) %>% 
  rowSums(na.rm = T)

crSample$PTSEVMAX <- PSEV[,grepl("PTSEV",names(PSEV))] %>%
  mutate_all(function(x)case_when(is.na(x) ~ NA_real_, x == "A" ~ 0, x == "B" ~ 1, x == "C" ~ 2, TRUE ~ as.numeric(x)+3)) %>% 
  apply(1,max,na.rm = T) %>% unlist


# Get the actual amount of prison time recommended for each charge according to severity. Add that to the tPP dataset
crSampleGuidelines <- 
  lapply(names(PSEV), function(n){
    myData <- as.character(PSEV[[n]])
    myData[is.na(myData)] <- "Z"
    myData[!myData %in% guidelines$SEV]
    myData <- bind_rows(lapply(myData,function(x)guidelines[guidelines$SEV == x,c(2,3)]))
    names(myData) <- paste(n, names(myData), sep = "_")
    myData
  }) %>% 
  bind_cols

# Get the maximums of the high and low bounds for the prison guidelines for all the charges for each case 
crSample$PTSEVh <- crSampleGuidelines[,grepl("T",names(crSampleGuidelines)) & grepl("high",names(crSampleGuidelines))] %>%
  apply(1, max, na.rm = T)

crSample$PTSEVh[is.infinite(crSample$PTSEVh)] <- NA
crSample$PTSEVl <- crSampleGuidelines[,grepl("T",names(crSampleGuidelines)) & grepl("low",names(crSampleGuidelines))] %>%
  apply(1, max, na.rm = T)
crSample$PTSEVl[is.infinite(crSample$PTSEVl)] <- NA
crSample <- crSample %>% cbind(PSEV)

# Replace the negative numbers representing NA values with NA 
crSample$PRISTOT <- ifelse(crSample$PRISTOT %in% c(-3,-8), NA, crSample$PRISTOT)
crSample$PRISTOT <- ifelse(crSample$PRISTOT == -1, 0, crSample$PRISTOT)

# table(tPP$PRISTOT <= tPP$PTSEVh & tPP$PRISTOT >= tPP$PTSEVl)
# table(tPP$PRISTOT <= tPP$PTSEVh)
# tPP %>% filter(PRISTOT > PTSEVh) %>% select(`Full legal name`,docketNumber, District, PTSEVh, PRISTOT)
# table(tPP$PRISTOT <= tPP$PTSEVl)
# summary(tPP$PTSEVh - tPP$PTSEVl)
# tPP[,c("PRISTOT", "PTSEVh", "PTSEVl")]

# Add on totals for offense levels too
FOFFLVL <- crSample[,grepl("FOFFLVL", names(crSample))] %>%
  mutate_all(function(x)ifelse(x<0,NA,x))

crSample$FOFFLVLTOT <- FOFFLVL %>% rowSums(na.rm = T)
crSample$FOFFLVLTOT <- ifelse(crSample$FOFFLVLTOT == 0, NA, crSample$FOFFLVLTOT)

FOFFLVL <- FOFFLVL %>% mutate_all(factor, levels = c(1,3,4))
crSample[,grepl("FOFFLVL", names(crSample))] <- FOFFLVL


FTITLE <- crSample[,grepl("FTITLE", names(tPP))] %>%
  mutate_all(function(x)ifelse(x==-8,NA,x))

lookup <- crSample %>% 
  filter(PRISTOT > PTSEVl) %>% 
  select(CIRCUIT,
         DISTRICT,
         DOCKET,
         `Prison Time (Months)` = PRISTOT, 
         `Guidelines Lower Bound` = PTSEVl,
         `Guidelines Upper Bound` = PTSEVh)
crSample$`Filing Offenses` <- crSample[,grepl("FTITLE", names(crSample))] %>%
  apply(1, function(x){
    x <- x[!is.na(x) & x !=-8]
    paste(x, collapse = ", ")})
# 521/26656
# 5/132
library(ggplot2)
# ggplot(data = crSample, aes(x = PRISTOT)) +
#   labs(x = "Total Prison Time (Months)", y = "Cases") +
#   geom_histogram(fill = 'gray', color = 'black', bins = length(unique(crSample$PRISTOT))) +
#   theme_minimal() + xlim(c(0,200))

crSample$protest = crSample$CASLGKY %in% tPP$CASLGKY
crSample$jan6 = crSample$CASLGKY %in% jan6_idb$CASLGKY

table(crSample$protest)
table(crSample$jan6)
crSample$Protestor <- ifelse(crSample$protest,"BLM",ifelse(crSample$jan6,"J6", "Control")) %>%
  factor(levels = c("Control","BLM","J6"))

crSample$Plea <- crSample$DISPmax == "Convicted/final plea of guilty"
myReg1 <- lm(log(PRISTOT+1) ~ log(PTSEVTOT+1) + log(Plea+1) + log(protest+1),
              filter(crSample, !is.na(PRISTOT)))
myReg2 <- lm(log(PRISTOT+1) ~ log(PTSEVTOT+1) + log(Plea+1) +  log(jan6+1),
             filter(crSample, !is.na(PRISTOT)))
myReg3 <- lm(PRISTOT ~ PTSEVTOT + Plea + Protestor, filter(crSample, !is.na(PRISTOT)))

summary(myReg2)

jan6Sentenced <- jan6_idb %>% filter(!is.na(PRISTOT) & PRISTOT>=0 )


descResiduals <- myReg$residuals[rev(order(abs(myReg$residuals)))]
residual_order <- crSample[as.numeric(names(descResiduals)),c("CIRCUIT", "DISTRICT", "DOCKET", "OFFICE", "PRISTOT", "protest")]
residual_order$Residual <- descResiduals

residual_order <- residual_order %>% left_join(unique(select(Courts, DISTRICT = fjc_court_id, Court = short_name)))

residual_order %>% head(100) %>% sheet_write(tPP_sheet, "Residuals desc PRISTOT ~ PTSEVTOT + DISPmax + protest")

# plot(crSample$PFSEVMAX, crSample$PRISTOT)
# plot(tPP$PTSEVl[tPP$PTSEVl<2000], tPP$PRISTOT[tPP$PTSEVl<2000])
# library(ggplot2)
protestResid <- residual_order %>% inner_join(select(tPP, -PRISTOT))

quantile((tPP$PRISTOT), na.rm = T)
tPP$DISP1 %>% table

