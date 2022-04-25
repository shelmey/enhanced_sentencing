library(pdftools)
library(stringr)
codebook <- "https://www.fjc.gov/sites/default/files/idb/codebooks/Criminal%20Code%20Book%201996%20Forward.pdf"
distPages <- tempfile(fileext = ".pdf")
pdf_subset(codebook, 20:21, distPages)

codebooktext <- pdf_text(distPages) 
codebooktext <-codebooktext %>% paste(collapse = "\n")  
codebooktext <-gsub("^.*DISTRICT\\)\\n\\nConforms with the format established in Volume XI, Guide to Judiciary Policies and Procedures,\\nAppendix A\\.\\n\\n","",codebooktext)

codebooktext <-gsub("\\n\\nOFFICE.*$","",codebooktext)

Dists <- codebooktext %>% str_split("  +|\\n") %>% unlist
Dists <- Dists[grepl("-",Dists)]
DistCodes <- str_remove(Dists," - .*$| = .*$")
DistNames <- str_remove(Dists,"^[^ - ]* - |^[^ - ]* = ")
Districts <- cbind.data.frame(Code = DistCodes,Name = DistNames)
Districts$Region <- Districts$Name %>% str_remove("^.* - |^.* - ")
Districts$State <- Districts$Name %>% str_remove(" - .*$| - .*$")
Districts$Region[!grepl(" - | - ", Districts$Name)] <- ""
Region.Abbrev <- substr(Districts$Region,1,1)
Region.Abbrev[Region.Abbrev!=""] <- paste0(Region.Abbrev[Region.Abbrev!=""],".")
Districts$short_name <- paste0(Region.Abbrev,"D. ",Districts$State)
Districts$short_name[Districts$short_name=="D. District of Columbia"] <- "District of Columbia"
Districts %>% write.csv("districts.csv", row.names = F)

# table(topResults$court %in% paste("District Court,", Districts$short_name))
