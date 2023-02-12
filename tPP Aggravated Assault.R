library(googlesheets4)
library(dplyr)
library(stringr)

tPP_sheet <- "1g_kBPD5BiAf0hhXjjLgJojgJy_1_HKOO--YkIRZJ_nA"
alltPP <- read_sheet(tPP_sheet, "Source data working") %>%
  filter(!grepl("Carrillo",`Full legal name`))
tPP <- tPP_sheet %>% read_sheet("Source data")   %>%
  filter(!grepl("Carrillo",`Full legal name`))

tPP <- tPP %>% left_join(courts)
# Read courts
{courts <- read.csv("courts.csv", stringsAsFactors = F, strip.white = T) %>% 
  select(court_id = id, court_full_name = full_name, Court = short_name, DISTRICT = fjc_court_id) %>% 
  filter(!duplicated(DISTRICT))
}

fc2 <- tPP_sheet %>% 
  read_sheet("Felony Convictions 2") %>%
  filter(!grepl("Carrillo",`Full legal name`)) %>%
  within({
    violent <- Violent
    destructive <- Destructive
    Destructive <- case_when(is.na(Destructive) ~ "",
                             !is.na(Destructive) & Destructive ~ "Yes",
                             !is.na(Destructive) & !Destructive ~ "No")
    Violent <- case_when(is.na(Violent) ~ "",
                         !is.na(Violent) & Violent ~ "Yes",
                         !is.na(Violent) & !Violent ~ "No")})
    
# Aggravated Assault Titles
{aatitles <- c("18:111", 
              "18:112", 
              "18:113(a)(2)", 
              "18:113(a)(3)", 
              "18:113(a)(6)",
              "18:113(a)(8)", 
              "18:113(a)(3)", 
              "18:114", 
              "18:115(a)", 
              "18:115(b)(1)", 
              "18:351(e)", 
              "18:1751(e)", 
              "18:1841(a)(2)(C)", 
              "18:1992(a)(7)", 
              "18:2199", 
              "18:2291", 
              "18:2332b(a)(1)", 
              "18:2340A")
}


myTitles <- aatitles %>% 
  str_remove_all(" .*$") %>%
  toupper() %>%
  str_replace_all("[)(]+","\\.") %>%
  str_remove_all("\\.$")

ttitles <- fc2[, grepl("TTITLE[0-9]", names(fc2))]
ftitles <- fc2[, grepl("FTITLE[0-9]", names(fc2))]

aaMat <- apply(ftitles, 2, function(x)unlist(lapply(x,function(y)str_extract_all(y,"18\\:[0-9]+") %in% str_extract_all(myTitles,"18\\:[0-9]+"))))

aa <- rowSums(aaMat)>0
fc2$`Full legal name`[aa]
