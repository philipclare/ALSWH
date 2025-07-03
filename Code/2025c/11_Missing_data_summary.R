######################################################################################
##   
## Effects of physical activity on incident obesity
## Summarise missing data for appendix
## Date: 03 July 2025
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
libs <- c("dplyr","naniar","VIM")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load and process data
#-------------------------------------------------------------------------------------

imp_data <- readRDS(file=paste0(workdir,"Data/imputation data.rds"))

# Sort data from most to least missing, saving order to return data to original order if needed
res <- summary(aggr(imp_data,plot=FALSE))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- imp_data[,res$Variable]

######################################################################################
# 3. Check and summarise missingness
#-------------------------------------------------------------------------------------

by_wave <- imp_data %>%  
  group_by(wave)

p2 <- by_wave %>% summarise(
  o1 = sum(is.na(bmi)),
  e1 = sum(is.na(weighted_activity_time)),
  c1 = sum(is.na(vegetables)),
  c2 = sum(is.na(fruit)),
  c3 = sum(is.na(alcfq)),
  c4 = sum(is.na(alcbng)),
  c5 = sum(is.na(whobmigroup)),
  c6 = sum(is.na(employ)),
  c7 = sum(is.na(cesd10)),
  c8 = sum(is.na(seifadis)),
  c9 = sum(is.na(marital)),
  c10 = sum(is.na(smokst)),
  c11 = sum(is.na(live_u18)),
  c12 = sum(is.na(live_o18)),
  c13 = sum(is.na(mnstrs)),
  c14 = sum(is.na(ariapgp)),
  c15 = sum(is.na(age)),
  c16 = sum(is.na(heartdis_3yr)),
  c17 = sum(is.na(stroke_3yr)),
  c18 = sum(is.na(cancer_3yr)),
  c19 = sum(is.na(arthritis_3yr)),
  c20 = sum(is.na(depression_3yr)),
  c21 = sum(is.na(anxiety_3yr)),
  c22 = sum(is.na(pcsa)),
  c23 = sum(is.na(mcsa))
)

p3 <- by_wave %>% summarise(
  b1 = sum(is.na(b_cobcat)),
  b2 = sum(is.na(b_educ)),
  b3 = sum(is.na(b_metmin)),
  b4 = sum(is.na(b_heartdis_ever)),
  b5 = sum(is.na(b_stroke_ever)),
  b6 = sum(is.na(b_cancer_ever)),
  b7 = sum(is.na(b_depression_ever)),
  b8 = sum(is.na(b_anxiety_ever))
)


miss <- rbind(t(p2),t(p3)[-1,])
View(miss)

######################################################################################
# 4. Plot missing data patterns
#-------------------------------------------------------------------------------------

jpeg(file=paste0(workdir,"Results/Appendix Figure X1.jpg"),
     width=15,
     height=12,
     units="cm",
     res=300)
gg_miss_upset(dataimp,
              nintersects = 15)
dev.off()
