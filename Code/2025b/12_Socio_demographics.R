######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Socio-demographic descriptives
## Date: 24 July 2023
## Authors: Philip Clare and Neta Hagani
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration:https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------


workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/Data/"

libs <- c("haven","plyr","dplyr","tidyr", "summarytools")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)


# When you have a RDS file which is a list of variables with 3 dimantions. YOu want to first turn it to a 2 dimention so you can use the data.
# the 3-dimention data name is "all cause analysis - pr"
# we want to take from it the first dimention so we need to add [[1]]
# the new data will be called "socio"
socio <- `all cause analysis - pr`[[1]]
# set out the libraray to save it
setwd("R:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/Data")
# save as csv
write.csv(socio, file = "socio.csv", row.names = FALSE)


# load data
socio_data <- read.csv("socio.csv")


library(readr)
socio <- read_csv("R:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/Data/socio.csv")
View(socio)


# wave 1 baseline variables
subset_data <- socio[, c("idproj", "b_country", "b_educ", "b_language", "b_depression_ever", 
                         "b_anxiety_ever", "mos_long2", "age2", "employ2", "mnstrs2", "live_alone2", 
                         "alcliferisk2", "alcepisrisk2", "pcsa2", "mcsa2", "gh2", "pf2")]


view(dfSummary(subset_data)) 


# taking variable from the imputation data



# wave 2 baseline variables

socio_data2 <- subset(imp_data, wave == 2)

view(dfSummary(socio_data2)) 
