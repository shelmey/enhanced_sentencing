library(pdftools)
library(stringr)

Ch3_Offender_Based2020 <- "https://www.ussc.gov/sites/default/files/pdf/research-and-publications/federal-sentencing-statistics/guideline-application-frequencies/Ch3_Offender_Based.pdf"

Ch3_Offender_Based2020 <- pdf_text(Ch3_Offender_Based2020)


Ch3_Offender_Based2020_lines <- str_split(Ch3_Offender_Based2020,"\n")

Ch3_Offender_Based2020_lines[[4]][grepl("3A1\\.4 Terrorism Adjustment \\(12 levels, or increase to level 32\\)", Ch3_Offender_Based2020_lines[[4]])]
Ch3_Offender_Based2020_lines[[4]]

Offenses <- Reduce(rbind, lapply(Ch3_Offender_Based2020_lines,function(x){
  hatecrime <- which(grepl("Hate Crime Adjustment",x))
  if(length(hatecrime)==0){return(NULL)}else{
  guideline <- which(grepl("Guideline and Adjustment",x))
  y <- str_split(x[hatecrime-2],"  +")
  
  y <- unlist(y)
  if(hatecrime-guideline == 5)y[1] <- paste(x[hatecrime-3], y[1])
  
  y <- y[y!=""]
  z <- str_split(x[grepl("3A1\\.4 Terrorism Adjustment \\(12 levels, or increase to level 32\\)",x)],"  +")
  z <- unlist(z)
  z <- z[z!=""]
  return(c(y,z))
  }
})) %>% 
  as.data.frame() %>% 
  mutate_all(trimws) %>% 
  select(Offense = V1, Total = V2, Terror = V5) %>%
  filter(Offense != "All Offenses") %>%
  mutate_at(2:3, function(x)as.numeric(str_remove_all(x,"[^0-9\\.]")))

  