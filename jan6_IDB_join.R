library(haven)
library(stringr)
library(googlesheets4)
cr17to22_in <- "/home/sam/enhanced_sentencing/Inputs/cr17to22.sas7bdat"
cr20to22 <- read_sas(cr17to22_in) %>% 
  filter(FILEDATE > as.Date("2020-01-01")) 

cr20to22DC <- cr20to22 %>%
  filter(DISTRICT == "90") %>%
  arrange(paste(DISTRICT, OFFICE, DOCKET),
          desc(LOADDATE)) %>%
  filter(!duplicated(paste(DISTRICT, OFFICE, DOCKET)))

jan6_idb <- df %>% 
  left_join(cr20to22DC) %>%
  mutate(terminated = TERMDATE > as.Date("1900-01-01"),# dummy variable for terminated
         sentenced = SENTDATE > as.Date("1900-01-01")) # dummy variable for sentenced

disps <- jan6_idb[, grepl("DISP[0-9]", names(jan6_idb))]

jan6_idb$relevantDisp <- apply(disps,1,function(x){
  x <- as.vector(x)
  4 %in% x|8 %in% x|9 %in% x})

relevantDispCases <- jan6_idb %>% filter(relevantDisp)

termTitles <- jan6_idb[, grepl("TTITLE[0-9]", names(jan6_idb))]

filingTitles <- jan6_idb[, grepl("FTITLE[0-9]", names(jan6_idb))]

jan6_idb$relevantTitle <- apply(termTitles,1,function(x){
  x <- as.vector(x)
  sum(grepl("18:111|18:1361", x), na.rm = T)>0})

jan6_idb$relevantFiling <- apply(filingTitles,1,function(x){
  x <- as.vector(x)
  sum(grepl("18:111|18:1361", x), na.rm = T)>0})

relevantCases <- jan6_idb %>% filter(relevantDisp & relevantTitle)

relevantTitleCases <- jan6_idb %>% filter(relevantTitle)

relevantFilingCases <- jan6_idb %>%
  filter(relevantFiling) %>%
  filter(terminated)
googledrive::drive_ls()
gs4_create(paste0("January 6 cases as of ", Sys.Date()), sheets = c("Source data", "Relevant Charges & Disps", "Relevant Disps", "Relevant Filings", "Relevant Charges"))
googledrive::drive_mv(file = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", path = "1ypbLLn8kiymTMUFgSJ96B9nIEqivd0CX")
jan6_idb %>% sheet_write(ss = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", sheet = "Source data")

relevantCases %>%
  select(Name, `Case Number`, `Charge(s)`, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", sheet = "Relevant Charges & Disps")

relevantDispCases %>%
  select(Name, `Case Number`, `Charge(s)`, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", sheet = "Relevant Disps")

relevantFilingCases %>%
  select(Name, `Case Number`, `Charge(s)`, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", sheet = "Relevant Filings")

relevantTitleCases %>%
  select(Name, `Case Number`, `Charge(s)`, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", sheet = "Relevant Charges")

PRISTIM <- jan6_idb[,grepl("PRISTIM",names(jan6_idb))]

# Longest Sentences
jan6_idb[rev(order(apply(PRISTIM, 1, max, na.rm = T))),] %>% 
  head(10) %>%
  select(Name, `Case Number`, `Charge(s)`, TYPEREG, DOCKET, FISCALYR, FILEDATE, TERMDATE,
         FTITLE1, FTITLE2, FTITLE3, FTITLE4, FTITLE5,
         TTITLE1, TTITLE2, TTITLE3, TTITLE4, TTITLE5,
         DISP1, DISP2, DISP3, DISP4,
         PRISTIM1, PRISTIM2, PRISTIM3, PRISTIM4, PRISTIM5,
         FSEV1, FSEV2, FSEV3, FSEV4, FSEV5,
         FOFFLVL1, FOFFLVL2, FOFFLVL3, FOFFLVL4, FOFFLVL5,
         TSEV1, TSEV2, TSEV3, TSEV4, TSEV5,
         TOFFLVL1, TOFFLVL2, TOFFLVL3, TOFFLVL4, TOFFLVL5
  ) %>%
  sheet_write(ss = "1Uib3M6B7PYW6PZaOPZjhivzwJgE-bz2VqF5NSD9wmS0", sheet = "Longest Single Sentences")

