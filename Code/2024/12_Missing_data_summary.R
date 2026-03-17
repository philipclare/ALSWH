######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Summarise missing data for appendix
## Date: 29 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Specify paths to Katana/windows PC paths based on whether NCPUS is detected
workdir <- "R:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"

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

load(paste0(workdir,"Data/imputation data.RData"))

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

p1 <- by_wave %>% summarise(
  o1 = sum(is.na(pcsa)),
  o2 = sum(is.na(mcsa)),
  o3 = sum(is.na(pf)),
  o4 = sum(is.na(rp)),
  o5 = sum(is.na(bp)),
  o6 = sum(is.na(vt)),
  o7 = sum(is.na(gh)),
  o8 = sum(is.na(re)),
  o9 = sum(is.na(sf)),
  o10 = sum(is.na(mh))
)

p2 <- by_wave %>% summarise(
  c1 = sum(is.na(weighted_activity_time)),
  c2 = sum(is.na(vegetables)),
  c3 = sum(is.na(fruit)),
  c4 = sum(is.na(alcfq)),
  c5 = sum(is.na(alcbng)),
  c6 = sum(is.na(whobmigroup)),
  c7 = sum(is.na(employ)),
  c8 = sum(is.na(cesd10)),
  c9 = sum(is.na(seifadis)),
  c10 = sum(is.na(marital)),
  c11 = sum(is.na(smokst)),
  c12 = sum(is.na(live_u18)),
  c13 = sum(is.na(live_o18)),
  c14 = sum(is.na(mnstrs)),
  c15 = sum(is.na(ariapgp)),
  c16 = sum(is.na(age)),
  c17 = sum(is.na(heartdis_3yr)),
  c18 = sum(is.na(stroke_3yr)),
  c19 = sum(is.na(cancer_3yr)),
  c20 = sum(is.na(arthritis_3yr)),
  c21 = sum(is.na(depression_3yr)),
  c22 = sum(is.na(anxiety_3yr))
)

p3 <- by_wave %>% summarise(
  b1 = sum(is.na(b_heartdis_ever)),
  b2 = sum(is.na(b_stroke_ever)),
  b3 = sum(is.na(b_cancer_ever)),
  b4 = sum(is.na(b_depression_ever)),
  b5 = sum(is.na(b_anxiety_ever)),
  b6 = sum(is.na(b_cobcat)),
  b7 = sum(is.na(b_educ))
)

miss <- rbind(t(p1),t(p2)[-1,],t(p3)[-1,])
View(miss)

######################################################################################
# 4. Plot missing data patterns
#-------------------------------------------------------------------------------------

gg_miss_upset(dataimp,
              nintersects = 15)

