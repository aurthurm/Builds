rm(list = ls())
library(reshape)
library(lubridate)

source("./builds/bika_lims_libraries.R")


## Calling Functions from # source("./builds/bika_lims_libraries.R")
##
data <- LimsModifier(LimsCombineCSV())

## This function will display the statistcs on the console
LimsAphlStats(data)
