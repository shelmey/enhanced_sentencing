# devtools::install_github("hadley/haven")
# require(haven)
setwd('/home/sam/Projects/RichardDefense/enhanced_sentencing')


# install.packages("SAScii")
library(SAScii)
input <- "Inputs/opafy20nid/opafy20nid.dat"
instructions <- "Inputs/opafy20nid/opafy20nid.sas"

parse.SAScii(instructions, beginline = 25, lrecl=34448)
# once you see that that's printed the column names and widths correctly, you can use the ?read.SAScii function to directly read your text file into an R data frame
opafy20nid <- read.SAScii(input, instructions, beginline = 25, n = 10)

# Load Packages
library(readr)
library(SAScii)
library(data.table)

Instructions <- readLines(instructions)
instructions_temp <- tempfile()
writeLines(gsub("[0-9]+\\-","",Instructions), instructions_temp)


# Parse input file
instructions_DT <- parse.SAScii(instructions_temp)

setDT(instructions_DT) # convert to data.table

# read to data frame
opafy20nid <- read_fwf(input, 
                     fwf_widths(dput(instructions_DT$width),
                                col_names=(dput(instructions_DT$varname))),
                     progress = interactive()
)

write.csv(opafy20nid, "Inputs/opafy20nid/opafy20nid.csv", row.names = F)

USSCIDN
