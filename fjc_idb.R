input <- 'Inputs/cr12to22.sas7bdat'

require(haven)
require(dplyr)

cr <- read_sas(input)

R <- cr %>% filter(DOCKET == "2000466" & DISTRICT == "3E")
R_Offenses <- unique(R[,grepl("^FOFFCD[123]", names(cr))]) %>% as.character()
R_Lvls <- unique(R[,grepl("^FOFFLVL[123]", names(cr))]) %>% as.character()


R_Type_Cases_1 <- cr %>% 
  filter(FOFFCD1 %in% Richard_Offenses)%>% 
  filter(FOFFCD2 %in% Richard_Offenses)%>% 
  filter(FOFFCD3 %in% Richard_Offenses) %>%
  filter(FOFFCD3 != FOFFCD1 & FOFFCD2 != FOFFCD1 & FOFFCD2 != FOFFCD3)# %>%
filter(DISP1==4|DISP2==4|DISP3==4)

R_Type_Cases <- cr %>% 
  filter(paste(FOFFCD1, FOFFLVL1) %in% paste(Richard_Offenses, Richard_Lvls))%>% 
  filter(paste(FOFFCD2, FOFFLVL2) %in% paste(Richard_Offenses, Richard_Lvls))%>% 
  filter(paste(FOFFCD3, FOFFLVL3) %in% paste(Richard_Offenses, Richard_Lvls)) %>%
  filter(FOFFCD3 != FOFFCD1 & FOFFCD2 != FOFFCD1) %>%
  filter(DISP1==4|DISP2==4|DISP3==4)


R_Type_Cases$PRISTOT %>% summary
R_Type_Cases$FISCALYR %>% table


R_Type_Cases$DOCKET

t(R[20,])

