
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(kableExtra)
library(stringr)
library(reshape2)
jan6_sheet <- "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0"
jan6All <- read_sheet(jan6_sheet)
# jan6 <- jan6All %>% filter(Felony)

disps <- cbind(Name = jan6All$Name, jan6All[, grepl("DISP[0-9]", names(jan6All))])
names(disps) <- names(disps) %>% str_remove_all("DISP")
TOFFLVL <- cbind(Name = jan6All$Name, jan6All[,grepl("TOFFLVL",names(jan6All))])
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
  group_by(Name) %>%
  summarise(Tfelony = max(Tfelony,na.rm = T))

jan6Pleas <- read_sheet(jan6_sheet, "Plea Agreements")
jan6Pleas <- jan6Pleas %>% left_join(disps_max)
jan6Pleas$Tfelony <- as.logical(jan6Pleas$Tfelony)
range_write(ss=jan6_sheet, sheet = "Plea Agreements", data = jan6Pleas[,"Tfelony"], range = "L1")

table(apply(jan6Pleas[jan6Pleas$Count3A1.4 == 1, grepl("TOFFLVL", names(jan6Pleas))],1,function(x)sum(is.na(x))))
table(apply(jan6Pleas[jan6Pleas$Count3A1.4 == 1, grepl("TOFFLVL", names(jan6Pleas))],1,function(x)sum(x==-8)))


TSEV <- cbind(Name = jan6All$Name, jan6All[, grepl("TSEV[0-9]", names(jan6All))])
names(TSEV) <- names(TSEV) %>% str_remove_all("TSEV")
TSEV <- TSEV %>% melt(id = "Name")
names(TSEV)[2] <- "Offense"
names(TSEV)[3] <- "TSEV"
TSEV$Type <- as.numeric(substr(TSEV$TSEV,2,2))
disps <- disps %>% left_join(TSEV)

type_max <- disps %>%
  filter(DISP %in% c(4,9)) %>%
  group_by(Name) %>%
  summarise(Type = max(Type,na.rm = T))

types <- cbind.data.frame(Type = c(0:3),Offense_Type = c("None","Moral Terpitude","Property","Personal"))

type_max <- type_max %>% left_join(types)

jan6Pleas <- read_sheet(jan6_sheet, "Plea Agreements")
jan6Pleas <- jan6Pleas %>% left_join(type_max)

range_write(ss=jan6_sheet, sheet = "Plea Agreements", data = jan6Pleas[,"Offense_Type"], range = "M1")
