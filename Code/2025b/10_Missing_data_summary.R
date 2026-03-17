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
workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"

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
# 3. Check missingness
#-------------------------------------------------------------------------------------

outcome_data <- imp_data[which(imp_data$wave==9 | imp_data$wave==2),c("death")]
exposure_data <- imp_data[which(imp_data$wave<=8 & imp_data$wave>=3),c("lonely_category")]
confounder_data <- imp_data[which(imp_data$wave<=7 & imp_data$wave>=2),c("pf","mh","gh","re","rp","sf","bp","vt","pcsa","mcsa",
                                                                         "mnstrs","smokst","age","mstat","live_alone","employ",
                                                                         "whobmigroup","ariapgp","seifadis","alcfq","alcbng")]
diag_data <- imp_data[which(imp_data$wave<=7 & imp_data$wave>=3),c("depression_3yr","anxiety_3yr")]
base_data <- imp_data[which(imp_data$wave==2),c("b_educ","b_wtarea","b_language","b_country")]

res_out <- summary(aggr(outcome_data))$missings
res_exp <- summary(aggr(exposure_data))$missings
res_con <- summary(aggr(confounder_data))$missings
res_dia <- summary(aggr(diag_data))$missings
res_bse <- summary(aggr(base_data))$missings

mean(is.na(dataimp))

gg_miss_upset(dataimp,
              nintersects = 15)

######################################################################################
# 4. Summarise missingness
#-------------------------------------------------------------------------------------

by_wave <- imp_data %>%  
  group_by(wave)

p1 <- by_wave %>% summarise(
  c1 = sum(is.na(lonely_category)),
  c2 = sum(is.na(alcfq)),
  c3 = sum(is.na(alcbng)),
  c4 = sum(is.na(whobmigroup)),
  c5 = sum(is.na(employ)),
  c6 = sum(is.na(mos_short)),
  c7 = sum(is.na(seifadis)),
  c8 = sum(is.na(mstat)),
  c9 = sum(is.na(smokst)),
  c10 = sum(is.na(mnstrs)),
  c11 = sum(is.na(ariapgp)),
  c12 = sum(is.na(age)),
  c13 = sum(is.na(depression_3yr)),
  c14 = sum(is.na(anxiety_3yr)),
  c15 = sum(is.na(pcsa)),
  c16 = sum(is.na(mcsa)),
  c17 = sum(is.na(pf)),
  c18 = sum(is.na(rp)),
  c19 = sum(is.na(bp)),
  c20 = sum(is.na(vt)),
  c21 = sum(is.na(gh)),
  c22 = sum(is.na(re)),
  c23 = sum(is.na(sf)),
  c24 = sum(is.na(mh))
)

p2 <- by_wave %>% summarise(
  b1 = sum(is.na(b_depression_ever)),
  b2 = sum(is.na(b_anxiety_ever)),
  b3 = sum(is.na(b_language)),
  b4 = sum(is.na(b_country)),
  b5 = sum(is.na(b_educ))
)

miss <- rbind(t(p1),t(p2)[-1,])
View(miss)

######################################################################################
# 4. Plot missing data patterns
#-------------------------------------------------------------------------------------

gg_miss_upset(dataimp,
              nintersects = 15)

