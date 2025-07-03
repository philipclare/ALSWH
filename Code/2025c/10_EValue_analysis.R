######################################################################################
##   
## Effects of physical activity on incident obesity
## Extract difference results and calculate E-Values
## Date: 17 June 2025
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://osf.io/fyszg
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Specify working directory
if (Sys.info()[['sysname']]=="Linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/Obesity/"
} else if (Sys.info()[['sysname']]=="Windows") {
  workdir <- "Y:/PRJ-prc_alswh/Paper 3 - Obesity/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/Paper 3 - Obesity/" # MAC
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("tidyverse","EValue","XLConnect")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

# 1.3. List outcomes
names <- c("all-cause","CVD","cancer")

######################################################################################
# 2. Get model results
#-------------------------------------------------------------------------------------

# 2.1. Load all-cause results
res_rr <- readRDS(file=paste0(workdir,"Results/Processed/rrs - binary.rds"))[,1:3]
res_rd <- readRDS(file=paste0(workdir,"Results/Processed/rds - binary.rds"))[,1:3]

res <- merge(res_rd,res_rr,by=c("num"))
colnames(res) <- c("num","rd_est","rd_se","rr_est","rr_se")

res$rr_lb <- res$rr_est-qnorm(0.9975)*res$rr_se
res$rr_ub <- res$rr_est+qnorm(0.9975)*res$rr_se

rm(res_rr,res_rd)

######################################################################################
# 3. Calculate E-Values
#-------------------------------------------------------------------------------------

evalues_RD <- do.call(rbind,lapply(seq(1,8), function (x) {
  
  evalues.MD(est=as.numeric(res[x,2]),se=as.numeric(res[x,3]))[2,]
  
}))

evalues_RR <- do.call(rbind,lapply(seq(1,8), function (x) {
  
  evalues.RR(est=as.numeric(res[x,4]),lo=as.numeric(res[x,6]),hi=as.numeric(res[x,7]))[2,]
  
}))

######################################################################################
# 4. Save results to excel
#-------------------------------------------------------------------------------------

wb <- XLConnect::loadWorkbook(paste0(workdir,"Results/Processed/EValues.xlsx"), create=TRUE)
createSheet(wb, name="rr")
writeWorksheet(wb,evalues_RR,sheet="rr")
createSheet(wb, name="rd")
writeWorksheet(wb,evalues_RD,sheet="rd")
XLConnect::saveWorkbook(wb)