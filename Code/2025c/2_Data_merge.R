######################################################################################
##   
## Effects of physical activity on incident obesity
## Merge data into combined, long-form data, ready for imputation
## Date: 6 June 2025
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://osf.io/fyszg
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Specify paths to Katana/windows/Mac paths based on system
if (Sys.info()[['sysname']]=="Linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh_lonely/"
} else if (Sys.info()[['sysname']]=="Windows") {
  workdir <- "Y:/PRJ-prc_alswh/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/" # MAC
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("haven","dplyr","tidyr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load individual wave data and merge into long form
#-------------------------------------------------------------------------------------

w1data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w1 bmi.rds"))
w2datat <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w2t bmi.rds"))
w2datab <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w2b bmi.rds"))
w3data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w3 bmi.rds"))
w4data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w4 bmi.rds"))
w5data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w5 bmi.rds"))
w6data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w6 bmi.rds"))
w7data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w7 bmi.rds"))
w8data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w8 bmi.rds"))
w9data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w9 bmi.rds"))
w10data <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/w10 bmi.rds"))
death <- readRDS(file=paste0(workdir,"Paper 3 - Obesity/Data/deathdata bmi.rds"))

long_data <- bind_rows(w2datat,w3data,w4data,w5data,w6data,w7data,w8data,w9data,w10data)

long_data <- merge(w1data,long_data,by="idproj",all.x=TRUE)
long_data <- merge(w2datab,long_data,by="idproj",all.x=TRUE)

rm(w1data,w2datat,w2datab,w3data,w4data,w5data,w6data,w7data,w8data,w9data)

######################################################################################
# 3. Master coded/derived variables
#-------------------------------------------------------------------------------------

long_data <- long_data %>% mutate(ariapgp = case_match(ariapgp, 
                                            1 ~ 0,
                                            2 ~ 1,
                                            3 ~ 1,
                                            4 ~ 2,
                                            5 ~ 2,
                                            6 ~ NA))
long_data$ariapgp <- factor(long_data$ariapgp,labels=c("Major city","Regional","Remote"))

long_data <- long_data %>% mutate(marital = case_match(marital, 
                                                   1 ~ 0,
                                                   2 ~ 0,
                                                   3 ~ 1,
                                                   4 ~ 1,
                                                   5 ~ 2,
                                                   6 ~ 1))
long_data$marital <- factor(long_data$marital,labels=c("Married/de facto","Separated/divorced/never married","Widowed"))

long_data <- long_data %>% mutate(smokst = case_match(smokst, 
                                                  1 ~ 0,
                                                  2 ~ 1,
                                                  3 ~ 2,
                                                  4 ~ 2,
                                                  5 ~ 2,
                                                  6 ~ NA))
long_data$smokst <- factor(long_data$smokst,labels=c("Never smoker","Ex smoker","Current smoker"))

long_data <- long_data %>% mutate(vitamins = case_match(vitamins, 
                                                      1 ~ 0,
                                                      2 ~ 0,
                                                      3 ~ 1,
                                                      4 ~ 1))
long_data$vitamins <- factor(long_data$vitamins, labels=c("Never/rarely","Sometimes/often"))

long_data$alcfq <- long_data$alcfre * long_data$alcqnt

long_data$walking_time <- ifelse(long_data$walking_time>840,840,long_data$walking_time)
long_data$moderate_time <- ifelse(long_data$moderate_time>840,840,long_data$moderate_time)
long_data$vig_leis_time <- ifelse(long_data$vig_leis_time>840,840,long_data$vig_leis_time)
long_data$vig_hous_time <- ifelse(long_data$vig_hous_time>840,840,long_data$vig_hous_time)

long_data$weighted_activity_time <- (1*long_data$walking_time) + (1*long_data$moderate_time) + (2*long_data$vig_leis_time)

ny_list <- c("heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","live_u18","live_o18","vegetables","fruit","dentist")
long_data[,ny_list] <- lapply(long_data[,ny_list], factor, labels=c("No","Yes"))
long_data$whobmigroup <- factor(long_data$whobmigroup, labels=c("Underweight","Healthy","Overweight","Obese"))
long_data$pap <- factor(long_data$pap, labels=c("Last 2 years","Longer ago","Never"))
long_data$mam <- factor(long_data$mam, labels=c("Last 2 years","Longer ago","Never"))


long_data <- subset(long_data, select = -c(alcfre,alcqnt,walking_time,moderate_time,vig_leis_time,vig_hous_time))

long_data <- long_data %>% 
  group_by(idproj) %>% 
  arrange(wave) %>%
  fill(menstatgp)

long_data$menstatgp <- ifelse((long_data$wave==7 | long_data$wave==8 | long_data$wave==9 | long_data$wave==10) & (long_data$menstatgp==4 | long_data$menstatgp==5),6,long_data$menstatgp)
long_data$menstatgp <- ifelse((long_data$wave==7 | long_data$wave==8 | long_data$wave==9 | long_data$wave==10) & (long_data$hrt==1 & !is.na(long_data$hrt)),2,long_data$menstatgp)
long_data$menstatgp <- ifelse(long_data$wave<7 & long_data$menstatgp==3,4,long_data$menstatgp)
long_data$menstatgp <- ifelse(long_data$wave>=7 & long_data$menstatgp==3,6,long_data$menstatgp)

long_data$menopause <- ifelse(long_data$menstatgp==1,3,ifelse(long_data$menstatgp==4,1,2))
long_data$hrt <- ifelse(long_data$menstatgp==2,1,0)
long_data <- subset(long_data, select = -c(menstatgp))

long_data$menopause <- factor(long_data$menopause,
                              labels=c("Pre","Peri/post","Surgical"))
long_data$hrt <- factor(long_data$hrt,
                              labels=c("No","Yes"))

long_data <- data.frame(long_data)

######################################################################################
# 4. Full data - reshape to Wide and Back to long 
#-------------------------------------------------------------------------------------

# 4.1 Generate indicator of whether wave was completed, then reshape to wide
wide_data <- reshape(long_data,
                     timevar=c("wave"), 
                     idvar=c("idproj"),
                     v.names=c("inarea","marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
                               "heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                               "cesd10","mnstrs","vegetables","fruit","alcfq","alcbng","smokst","finfinc","menopause","hrt",
                               "weighted_activity_time","pcsa","mcsa","sleep_cat","sleep_prob",
                               "whobmigroup","bmi","htcm","wtkg",
                               "dentist","vitamins","pap","mam"),
                     sep = "",
                     dir="wide")

# 4.2 Drop participants with low functioning at wave 2, and dead before wave 3
wide_data <- wide_data[which(wide_data$b_pf>=54.8), ]
wide_data <- wide_data[which(wide_data$whobmigroup2=="Healthy"), ]

# 4.3 Reshape back to long
imp_data <- reshape(wide_data,
                     timevar=c("wave"), 
                     idvar=c("idproj"),
                     v.names=c("marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
                               "heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                               "cesd10","mnstrs","vegetables","fruit","alcfq","alcbng","smokst","finfinc","menopause","hrt",
                               "weighted_activity_time","pcsa","mcsa","sleep_cat","sleep_prob",
                               "whobmigroup","bmi","htcm","wtkg",
                               "dentist","vitamins","pap","mam"),
                     sep = "",
                     dir="long")

imp_data <- merge(imp_data,death,by=c("idproj","wave"))

table(imp_data$whobmigroup,imp_data$wave)

sens_imp <- imp_data
imp_data <- subset(imp_data, select = -c(dentist,vitamins,pap,mam))

######################################################################################
# 5. Save data in long form, ready for imputation
#-------------------------------------------------------------------------------------

saveRDS(imp_data,file=paste0(workdir,"Paper 3 - Obesity/Data/imputation data.rds"))
saveRDS(sens_imp,file=paste0(workdir,"Paper 3 - Obesity/Data/imputation data for sensitivity.rds"))

