library(dplyr)
library(googlesheets4)

setwd('/home/sam/Projects/RichardDefense/enhanced_sentencing')

tPP_sheet <- "https://docs.google.com/spreadsheets/d/1jSN5ueyn5UpgUwnjEIOJumQst7VIjuou6qrgHCNuhIM/edit"

guidelines <- 
  rbind.data.frame(cbind("A", 0, 0 ),
          cbind("B", 0, 6),
          cbind("C", 7, 12),
          cbind("0", 12, 24),
          cbind(1, 24, 36),
          cbind(2, 48, 60),
          cbind(3, 72, 120),
          cbind(4, 132, 180),
          cbind(5, 192, 240),
          cbind("6", 252, 300),
          cbind("7", 300, 1200),
          cbind("8", 10000, 10000),
          cbind("9", 10000, 100000),
          cbind("Z", NA, NA))

names(guidelines) <- c("SEV", "low", "high")
guidelines$low <- as.numeric(guidelines$low)
guidelines$high <- as.numeric(guidelines$high)


tPP <- tPP_sheet %>% read_sheet("Federal Cases w IDB cols 20220616")

PSEV <- t(apply(tPP[,grepl("SEV", names(tPP))],1,substr, 1, 1))
PSEV <- t(apply(PSEV, 1, function(x)gsub("[^ABC0-9]","",x)))
PSEV <- t(apply(PSEV, 1, function(x)ifelse(x=="",NA,x)))
PSEV <- as.data.frame(PSEV)
PSEV <- PSEV %>%
  mutate_all(factor, levels = c("A","B", "C", as.character(0:9))) 
PSEVTOT <- PSEV %>%
  mutate_all(function(x)case_when(is.na(x) ~ NA_real_,
                                  x == "A" ~ 0,
                                  x == "B" ~ 1,
                                  x == "C" ~ 2,
                                  TRUE ~ as.numeric(x)+3))
PSEVTOT <- PSEVTOT %>% rowSums(na.rm = T)
names(PSEV) <- paste0("P",names(PSEV))
tPP$PFSEVTOT <- PFSEVTOT

tPPguidelines <- 
  lapply(names(PSEV), function(n){
    myData <- as.character(PSEV[[n]])
    myData[is.na(myData)] <- "Z"
    myData[!myData %in% guidelines$SEV]
    myData <- bind_rows(lapply(myData,function(x)guidelines[guidelines$SEV == x,c(2,3)]))
    names(myData) <- paste(n, names(myData), sep = "_")
    myData
    }) %>% bind_cols

tPP$PTSEVh <- apply(tPPguidelines[,grepl("T",names(tPPguidelines)) & grepl("high",names(tPPguidelines))], 1, max, na.rm = T)
tPP$PTSEVh[is.infinite(tPP$PTSEVh)] <- NA
tPP$PTSEVl <- apply(tPPguidelines[,grepl("T",names(tPPguidelines)) & grepl("low",names(tPPguidelines))], 1, max, na.rm = T)
tPP$PTSEVl[is.infinite(tPP$PTSEVl)] <- NA
tPP <- tPP %>% cbind(PFSEV)

tPP$PRISTOT <- ifelse(tPP$PRISTOT %in% c(-3,-8), NA, tPP$PRISTOT)


table(tPP$PRISTOT <= tPP$PTSEVh & tPP$PRISTOT >= tPP$PTSEVl)
table(tPP$PRISTOT <= tPP$PTSEVh)
table(tPP$PRISTOT > tPP$PTSEVl)
tPP %>% filter(PRISTOT > PTSEVh) %>% select(`Full legal name`,docketNumber, District, PTSEVh, PRISTOT)

table(tPP$PRISTOT <= tPP$PTSEVl)
summary(tPP$PTSEVh - tPP$PTSEVl)

tPP[,c("PRISTOT", "PTSEVh", "PTSEVl")]

FOFFLVL <- tPP[,grepl("FOFFLVL", names(tPP))] %>%
  mutate_all(function(x)ifelse(x<0,NA,x))

FOFFLVLTOT <- FOFFLVL %>% rowSums(na.rm = T)

FOFFLVL <- FOFFLVL %>% mutate_all(factor, levels = c(1,3,4))

tPP[,grepl("FOFFLVL", names(tPP))] <- FOFFLVL
tPP$FOFFLVLTOT <- FOFFLVLTOT
tPP$PRISTOT <- ifelse(tPP$PRISTOT %in% c(-3,-8), NA, tPP$PRISTOT)
tPP$PRISTOT <- ifelse(tPP$PRISTOT == -1, 0, tPP$PRISTOT)
tPP$FOFFLVLTOT <- ifelse(tPP$FOFFLVLTOT == 0, NA, tPP$FOFFLVLTOT)


PRISCDs <- tPP[,grepl("PRISCD", names(tPP))]
# apply(PRISCDs,1,function(x)"S" %in% x)
tPPreg <- lm(tPP$PRISTOT ~ tPP$PFSEVTOT)
tPPreg2 <- lm(tPP$PRISTOT ~ tPP$PTSEVl)
summary(tPPreg)
descResiduals <- tPPreg$residuals[rev(order(tPPreg$residuals))]
residual_order <- tPP[as.numeric(names(descResiduals)),c("Full legal name","docketNumber", "DISTRICT", "PRISTOT")]
residual_order$Residual <- descResiduals

residual_order <- residual_order %>% left_join(unique(select(Courts, DISTRICT = fjc_court_id, Court = short_name)))

residual_order %>% sheet_write(tPP_sheet, "Residuals desc PRISTOT~SEVTOT")

plot(tPP$PFSEVTOT, tPP$PRISTOT)
plot(tPP$PTSEVl[tPP$PTSEVl<2000], tPP$PRISTOT[tPP$PTSEVl<2000])
library(ggplot2)


quantile((tPP$PRISTOT), na.rm = T)
tPP$DISP1 %>% table
