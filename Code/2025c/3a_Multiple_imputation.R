######################################################################################
##   
## Effects of physical activity on incident obesity
## Multiple imputation using random forests
## Date: 27 August 2024
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
  workdir <- "Y:/PRJ-prc_alswh/Paper 3 - Obesity/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/Paper 3 - Obesity/" # MAC
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("furrr","mice","miceadds","VIM","UpSetR","ggplot2","naniar")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

# 1.3. Define arguments
# args <- as.numeric(commandArgs(trailingOnly = TRUE))

# 1.4. Set random seed
set.seed(432455)

######################################################################################
# 2. Load and process data
#-------------------------------------------------------------------------------------

# 2.1. Load long-form data
imp_data <- readRDS(file=paste0(workdir,"Data/imputation data.rds"))

# 2.2. Sort data from most to least missing, saving order to return data to original order if needed
res <- summary(aggr(imp_data,plot=FALSE))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- imp_data[,res$Variable]

######################################################################################
# 3. Check missingness
#-------------------------------------------------------------------------------------

outcome_data <- imp_data[which(imp_data$wave<=10 & imp_data$wave>=4),c("whobmigroup")]
exposure_data <- imp_data[which(imp_data$wave<=9 & imp_data$wave>=3),c("weighted_activity_time")]
confounder_data <- imp_data[which(imp_data$wave<=7 & imp_data$wave>=2),c("marital","age","ariapgp","employ","seifadis","live_u18","live_o18","menopause","hrt",
                                                                         "cesd10","mnstrs","whobmigroup","vegetables","fruit","alcfq","alcbng","smokst")]
diag_data <- imp_data[which(imp_data$wave<=7 & imp_data$wave>=3),c("heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr")]
base_data <- imp_data[which(imp_data$wave==2),c("b_cancer_ever","b_depression_ever","b_anxiety_ever","b_cobcat","b_educ")]

res_out <- summary(aggr(outcome_data))$missings
res_exp <- summary(aggr(exposure_data))$missings
res_con <- summary(aggr(confounder_data))$missings
res_dia <- summary(aggr(diag_data))$missings
res_bse <- summary(aggr(base_data))$missings

mean(is.na(dataimp))

gg_miss_upset(dataimp,
              nintersects = 15)

######################################################################################
# 4. Define Imputation Parameters
#-------------------------------------------------------------------------------------

m <- 25 # number of imputations
n <- 5 # number of cores for parlmice to use
nimpcore <- m/n
maxit <- 20; # Number of mice iterations
default <- c("rf","rf","rf","rf") # Manually defined list of methods for each variable type

######################################################################################
# 5. Run multiple imputation using MICE with random forests
#-------------------------------------------------------------------------------------

imp_mice <- futuremice(data=dataimp,
                       m=m,
                       n.core=n,
                       maxit=maxit,
                       defaultMethod=default,
                       parallelseed=216136)

imp <- mids2datlist(imp_mice)

imp[[26]] <- dataimp

imp <- lapply(imp, function (x,varorder) {
  x <- x[,varorder]
},varorder=varorder)

######################################################################################
# 6. Save data
#-------------------------------------------------------------------------------------

saveRDS(imp,paste0(workdir,"Data/imputed data - long form.rds"))

