######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Merge data into combined, long-form data, ready for imputation
## Date: 29 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Specify paths to Katana/windows PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh/"
} else { # Manually defined for PC
  workdir <- "R:/PRJ-prc_alswh/"
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("haven","plyr","dplyr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load individual wave data and merge into long form
#-------------------------------------------------------------------------------------

load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w1 pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w2t pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w2b pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w3 pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w4 pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w5 pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w6 pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w7 pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w8 pa.RData"))
load(file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w9 pa.RData"))

long_data <- rbind.fill(w2datat,w3data,w4data,w5data,w6data,w7data,w8data,w9data)

long_data <- merge(w1data,long_data,by="idproj",all.x=TRUE)
long_data <- merge(w2datab,long_data,by="idproj",all.x=TRUE)

rm(w1data,w2datat,w2datab,w3data,w4data,w5data,w6data,w7data,w8data,w9data)

######################################################################################
# 3. Master coded/derived variables
#-------------------------------------------------------------------------------------

long_data <- long_data %>% mutate(ariapgp = recode(ariapgp, 
                                            `1` = 0,
                                            `2` = 1,
                                            `3` = 1,
                                            `4` = 2,
                                            `5` = 2,
                                            `6` = NA_real_))
long_data$ariapgp <- factor(long_data$ariapgp,labels=c("Major city","Regional","Remote"))

long_data <- long_data %>% mutate(marital = recode(marital, 
                                                   `1` = 0,
                                                   `2` = 0,
                                                   `3` = 1,
                                                   `4` = 1,
                                                   `5` = 2,
                                                   `6` = 1))
long_data$marital <- factor(long_data$marital,labels=c("Married/de facto","Separated/divorced/never married","Widowed"))

long_data <- long_data %>% mutate(smokst = recode(smokst, 
                                                  `1` = 0,
                                                  `2` = 1,
                                                  `3` = 2,
                                                  `4` = 2,
                                                  `5` = 2,
                                                  `6` = NA_real_))
long_data$smokst <- factor(long_data$smokst,labels=c("Never smoker","Ex smoker","Current smoker"))

long_data$alcfq <- long_data$alcfre * long_data$alcqnt

long_data$walking_time <- ifelse(long_data$walking_time>840,840,long_data$walking_time)
long_data$moderate_time <- ifelse(long_data$moderate_time>840,840,long_data$moderate_time)
long_data$vig_leis_time <- ifelse(long_data$vig_leis_time>840,840,long_data$vig_leis_time)
long_data$vig_hous_time <- ifelse(long_data$vig_hous_time>840,840,long_data$vig_hous_time)

long_data$weighted_activity_time <- (1*long_data$walking_time) + (1*long_data$moderate_time) + (2*long_data$vig_leis_time)

ny_list <- c("heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","live_u18","live_o18","vegetables","fruit")
long_data[,ny_list] <- lapply(long_data[,ny_list], factor, labels=c("No","Yes"))
long_data$whobmigroup <- factor(long_data$whobmigroup, labels=c("Underweight","Healthy","Overweight","Obese"))

long_data <- subset(long_data, select = -c(alcfre,alcqnt,walking_time,moderate_time,vig_leis_time,vig_hous_time))

######################################################################################
# 4. Reshape to Wide and Back to long 
#-------------------------------------------------------------------------------------

# 4.1 Generate indicator of whether wave was completed, then reshape to wide
long_data$censored <- 1
wide_data <- reshape(long_data,
                     timevar=c("wave"), 
                     idvar=c("idproj"),
                     v.names=c("censored","marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
                               "heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                               "cesd10","mnstrs","whobmigroup","vegetables","fruit","alcfq","alcbng","smokst",
                               "weighted_activity_time","pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"),
                     sep = "",
                     dir="wide")

# 4.2 Recode missing censored variables to 0 where NA to indicate wave was not completed
c_vars <- c("censored2","censored3","censored4","censored5","censored6","censored7","censored8","censored9")
wide_data[,c_vars] <- lapply(wide_data[,c_vars],function (x) {
  ifelse(is.na(x),0,x)
})

# 4.3 Then pass over waves to treat any observations subsequent to first loss as also lost
for (i in 7:1) {
  wide_data[,c_vars[i]] <- ifelse(wide_data[,c_vars[i+1]]==1,1,wide_data[,c_vars[i]])
}

# 4.4 Drop participants with low functioning at wave 2, and dead before wave 3
wide_data <- wide_data[which(wide_data$b_pf>=54.8), ]

# 4.5 Reshape back to long
imp_data <- reshape(wide_data,
                     timevar=c("wave"), 
                     idvar=c("idproj"),
                     v.names=c("censored","marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
                               "heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                               "cesd10","mnstrs","whobmigroup","vegetables","fruit","alcfq","alcbng","smokst",
                               "weighted_activity_time","pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"),
                     sep = "",
                     dir="long")

# 4.6 Create alternative long form data with censored waves excluded
imp_data_alt <- imp_data[which(imp_data$censored==1),]
imp_data_alt <- subset(imp_data_alt, select=-c(censored))

######################################################################################
# 5. Save data in long form, ready for imputation
#-------------------------------------------------------------------------------------

save(imp_data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/imputation data.RData"))
save(imp_data_alt,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/imputation data no censoring.RData"))
