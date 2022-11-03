setwd('/home/sam/Projects/RichardDefense/enhanced_sentencing')

# Load Packages
library(readr)
library(SAScii)
library(data.table)

############
### 2020 ###
############

input <- "Inputs/opafy20nid/opafy20nid.dat"
instructions <- "Inputs/opafy20nid/opafy20nid.sas"

############
### 2021 ###
############

input <- "Inputs/opafy21nid/opafy21nid.dat"
instructions <- "Inputs/opafy21nid/opafy21nid.sas"

# parse.SAScii(instructions, beginline = 25, lrecl=34448)

# once you see that that's printed the column names and widths correctly, you can use the ?read.SAScii function to directly read your text file into an R data frame
# opa <- read.SAScii(input, instructions, beginline = 25, n = 10)

Instructions <- readLines(instructions)
instructions_temp <- tempfile()
writeLines(gsub("[0-9]+\\-","",Instructions), instructions_temp)

# Parse input file
instructions_DT <- parse.SAScii(instructions_temp)

setDT(instructions_DT) # convert to data.table

# read to data frame
opa <- read_fwf(input,
                fwf_widths(dput(instructions_DT$width),
                           col_names=(dput(instructions_DT$varname))),
                progress = interactive())

# table(is.na(opa$TEROR1))

terrorNames <- which(grepl("TEROR", names(opa)))

anyNotNA <- function(x){
  sum(is.na(x)) < length(x)
}
terrorNotNA <- apply(opa[,terrorNames],1,anyNotNA)
acccatNotNA <- !is.na(opa$ACCCAT)
table(acccatNotNA)
