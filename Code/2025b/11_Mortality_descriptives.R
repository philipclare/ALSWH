######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Merge data into combined, wide-form data, ready for imputation
## Date: 24 July 2023
## Authors: Philip Clare and Neta Hagani
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration:https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"

libs <- c("openxlsx","survey")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Define function to get unweighted/weighted risk of death
#-------------------------------------------------------------------------------------

get_descriptives <- function (x) {

  p4 <- table(x$censored4,x$death4)[2,2]/(table(x$censored4,x$death4)[2,1]+table(x$censored4,x$death4)[2,2])
  se4 <- sqrt((p4*(1-p4))/(table(x$censored4,x$death4)[2,1]+table(x$censored4,x$death4)[2,2]))
  p5 <- table(x$censored5,x$death5)[2,2]/(table(x$censored5,x$death5)[2,1]+table(x$censored5,x$death5)[2,2])
  se5 <- sqrt((p5*(1-p5))/(table(x$censored5,x$death5)[2,1]+table(x$censored5,x$death5)[2,2]))
  p6 <- table(x$censored6,x$death6)[2,2]/(table(x$censored6,x$death6)[2,1]+table(x$censored6,x$death6)[2,2])
  se6 <- sqrt((p6*(1-p6))/(table(x$censored6,x$death6)[2,1]+table(x$censored6,x$death6)[2,2]))
  p7 <- table(x$censored7,x$death7)[2,2]/(table(x$censored7,x$death7)[2,1]+table(x$censored7,x$death7)[2,2])
  se7 <- sqrt((p7*(1-p7))/(table(x$censored7,x$death7)[2,1]+table(x$censored7,x$death7)[2,2]))
  p8 <- table(x$censored8,x$death8)[2,2]/(table(x$censored8,x$death8)[2,1]+table(x$censored8,x$death8)[2,2])
  se8 <- sqrt((p8*(1-p8))/(table(x$censored8,x$death8)[2,1]+table(x$censored8,x$death8)[2,2]))
  p9 <- table(x$censored9,x$death9)[2,2]/(table(x$censored9,x$death9)[2,1]+table(x$censored9,x$death9)[2,2])
  se9 <- sqrt((p9*(1-p9))/(table(x$censored9,x$death9)[2,1]+table(x$censored9,x$death9)[2,2]))
  
  po <- table(x$death9)[2]/nrow(x)
  seo <- sqrt((po*(1-po))/nrow(x))
  
  x$deathtemp <- ifelse(x$death9==0 | is.na(x$death9),0,1)
  
  svy <- svydesign(ids = ~idproj,
                   weights = ~wtarea,
                   data = x)
  
  wp4 <- svyby(~death4, ~censored4, svy, svymean, na.rm=TRUE)[2,2:3]
  wp5 <- svyby(~death5, ~censored5, svy, svymean, na.rm=TRUE)[2,2:3]
  wp6 <- svyby(~death6, ~censored6, svy, svymean, na.rm=TRUE)[2,2:3]
  wp7 <- svyby(~death7, ~censored7, svy, svymean, na.rm=TRUE)[2,2:3]
  wp8 <- svyby(~death8, ~censored8, svy, svymean, na.rm=TRUE)[2,2:3]
  wp9 <- svyby(~death9, ~censored9, svy, svymean, na.rm=TRUE)[2,2:3]
  
  wpo <- svymean(~deathtemp, svy, na.rm=TRUE)
  
  matrix(c(p4,p5,p6,p7,p8,p9,po,       wp4[1],wp5[1],wp6[1],wp7[1],wp8[1],wp9[1],wpo,
                      se4,se5,se6,se7,se8,se9,seo,wp4[2],wp5[2],wp6[2],wp7[2],wp8[2],wp9[2],attr(wpo,"var")),
                    nrow=14)
  
}

######################################################################################
# 3. Estimate unadjusted risk of death, unweighted and weighted
#-------------------------------------------------------------------------------------

ac_p <- readRDS(file=paste0(workdir,"Data/all cause analysis - pr.rds"))
ac_risk <- get_descriptives(ac_p[[30]])

######################################################################################
# 4. Combine into single table and output to excel
#-------------------------------------------------------------------------------------

mortality_desc <- cbind(ac_risk[1:7,1:2],ac_risk[8:14,1:2])
rownames(mortality_desc) <- c("4","5","6","7","8","9","total")
colnames(mortality_desc) <- c("ac_unw_est","ac_unw_se","ac_wt_est","ac_wt_se")

wb <- createWorkbook()
addWorksheet(wb,"Mortality")
writeData(wb,sheet="Mortality",mortality_desc,startRow=1,rowNames=TRUE)
saveWorkbook(wb, file = paste0(workdir,"Results/mortality-descriptives.xlsx"), overwrite = TRUE)
