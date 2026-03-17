######################################################################################
##   
## Effects of physical activity on incident obesity
## Process linearity tests and get best fitting model for each outcome
## Date: 19 June 2025
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://osf.io/fyszg
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
start_time <- Sys.time()

# 1.1. Specify paths to Katana/windows/Mac paths based on system
if (Sys.info()[['sysname']]=="Linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/Obesity/"
} else if (Sys.info()[['sysname']]=="Windows") {
  workdir <- "Y:/PRJ-prc_alswh/Paper 3 - Obesity/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/Paper 3 - Obesity/" # MAC
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("arm","dplyr","fastDummies","gam","ltmle","openxlsx","parallel","ranger","SuperLearner")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Pool and get lowest AIC
#-------------------------------------------------------------------------------------

names_pr <- list.files(paste0(workdir,"Results/Katana Output/Linearity/"), pattern = "bin*", recursive = TRUE, full.names = TRUE)
names_pr <- names_pr[stringr::str_order(names_pr, numeric=T)]
fit_pr <- as.data.frame(do.call(rbind,lapply(names_pr, readRDS)))


fit_pr$V2 <- as.numeric(fit_pr$V2)
fit_pr$V3 <- as.numeric(fit_pr$V3)

paste0("For the primary analysis, based on AIC the best fitting model is: ",fit_pr$V1[which(fit_pr$V2==min(fit_pr$V2))])
paste0("For the primary analysis, based on BIC the best fitting model is: ",fit_pr$V1[which(fit_pr$V3==min(fit_pr$V3))])

######################################################################################
# 3. Pool and get lowest AIC
#-------------------------------------------------------------------------------------

names_cat <- list.files(paste0(workdir,"Results/Katana Output/Linearity/"), pattern = "cat*", recursive = TRUE, full.names = TRUE)
names_cat <- names_cat[stringr::str_order(names_cat, numeric=T)]
fit_cat <- as.data.frame(do.call(rbind,lapply(names_cat, readRDS)))

fit_cat$V2 <- as.numeric(fit_cat$V2)
fit_cat$V3 <- as.numeric(fit_cat$V3)

paste0("For the categorical analysis, based on AIC the best fitting model is: ",fit_cat$V1[which(fit_cat$V2==min(fit_cat$V2))])
paste0("For the categorical analysis, based on BIC the best fitting model is: ",fit_cat$V1[which(fit_cat$V3==min(fit_cat$V3))])

######################################################################################
# 4. Pool and get lowest AIC
#-------------------------------------------------------------------------------------

names_sev <- list.files(paste0(workdir,"Results/Katana Output/Linearity/"), pattern = "sev*", recursive = TRUE, full.names = TRUE)
names_sev <- names_sev[stringr::str_order(names_sev, numeric=T)]
fit_sev <- as.data.frame(do.call(rbind,lapply(names_sev, readRDS)))

fit_sev$V2 <- as.numeric(fit_sev$V2)
fit_sev$V3 <- as.numeric(fit_sev$V3)

paste0("For the analysis of severe obesity, based on AIC the best fitting model is: ",fit_sev$V1[which(fit_sev$V2==min(fit_sev$V2))])
paste0("For the analysis of severe obesity, based on BIC the best fitting model is: ",fit_sev$V1[which(fit_sev$V3==min(fit_sev$V3))])

######################################################################################
# 5. Pool and get lowest AIC
#-------------------------------------------------------------------------------------

names_five <- list.files(paste0(workdir,"Results/Katana Output/Linearity/"), pattern = "five*", recursive = TRUE, full.names = TRUE)
names_five <- names_five[stringr::str_order(names_five, numeric=T)]
fit_five <- as.data.frame(do.call(rbind,lapply(names_five, readRDS)))

fit_five$V2 <- as.numeric(fit_five$V2)
fit_five$V3 <- as.numeric(fit_five$V3)

paste0("For the analysis of 5% weight gain, based on AIC the best fitting model is: ",fit_five$V1[which(fit_five$V2==min(fit_five$V2))])
paste0("For the analysis of 5% weight gain, based on BIC the best fitting model is: ",fit_five$V1[which(fit_five$V3==min(fit_five$V3))])

######################################################################################
# 6. Pool and get lowest AIC
#-------------------------------------------------------------------------------------

names_ten <- list.files(paste0(workdir,"Results/Katana Output/Linearity/"), pattern = "ten*", recursive = TRUE, full.names = TRUE)
names_ten <- names_ten[stringr::str_order(names_ten, numeric=T)]
fit_ten <- as.data.frame(do.call(rbind,lapply(names_ten, readRDS)))

fit_ten$V2 <- as.numeric(fit_ten$V2)
fit_ten$V3 <- as.numeric(fit_ten$V3)

paste0("For the analysis of 10% weight gain, based on AIC the best fitting model is: ",fit_ten$V1[which(fit_ten$V2==min(fit_ten$V2))])
paste0("For the analysis of 10% weight gain, based on BIC the best fitting model is: ",fit_ten$V1[which(fit_ten$V3==min(fit_ten$V3))])

######################################################################################
# 7. Save results to excel for tables
#-------------------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb,"Primary")
addWorksheet(wb,"Categorical")
addWorksheet(wb,"Severe")
addWorksheet(wb,"Five_percent")
addWorksheet(wb,"Ten_percent")

writeData(wb,sheet="Primary",fit_pr,startRow=1)
writeData(wb,sheet="Categorical",fit_cat,startRow=1)
writeData(wb,sheet="Severe",fit_sev,startRow=1)
writeData(wb,sheet="Five_percent",fit_five,startRow=1)
writeData(wb,sheet="Ten_percent",fit_ten,startRow=1)

saveWorkbook(wb, file = paste0(workdir,"Results/model-fit.xlsx"), overwrite = TRUE)
