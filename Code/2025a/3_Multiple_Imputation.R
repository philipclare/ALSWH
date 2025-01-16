######################################################################################
##   
## Effects of physical activity on mortality
## Multiple imputation using random forests
## Date: 25 November 2022
## OSF Registration: https://osf.io/pytzx
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "//surefsn025/ProfileR025$/philipclare/Documents/ALSWH/"

libs <- c("mice","miceadds","VIM","UpSetR","ggplot2","parallel","naniar")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

set.seed(432455)

######################################################################################
# 2. Load and process data
#-------------------------------------------------------------------------------------

load(paste0(workdir,"Data/imputation data.RData"))

# Sort data from most to least missing, saving order to return data to original order if needed
res<-summary(aggr(imp_data))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- imp_data[,res$Variable]

######################################################################################
# 3. Load and process data
#-------------------------------------------------------------------------------------

outcome_data <- imp_data[which(imp_data$wave==9 | imp_data$wave==2),c("pcsa", "mcsa", "pf", "rp", "bp", "vt", "gh", "re", "sf", "mh")]
exposure_data <- imp_data[which(imp_data$wave<=8 & imp_data$wave>=3),c("weighted_activity_time")]
confounder_data <- imp_data[which(imp_data$wave<=7 & imp_data$wave>=2),c("marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
                                                                         "cesd10","mnstrs","whobmigroup","vegetables","fruit","alcfq","alcbng","smokst")]
diag_data <- imp_data[which(imp_data$wave<=7 & imp_data$wave>=3),c("cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr")]
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
# 3. Define Imputation Parameters
#-------------------------------------------------------------------------------------

m <- 40 # number of imputations
n <- 2 # number of cores for parlmice to use
nimpcore <- m/n
maxit <- 20; # Number of mice iterations
default <- c("rf","rf","rf","rf") # Manually defined list of methods for each variable type

######################################################################################
# 4. Run multiple imputation using MICE with random forests
#-------------------------------------------------------------------------------------

imp_mice <- parlmice(data=dataimp,
                     m=m,
                     n.core=n,
                     n.imp.core=nimpcore,
                     maxit=maxit,
                     defaultMethod=default,
                     clusterseed=216136)

imp <- mids2datlist(imp_mice)

######################################################################################
# 5. Save data
#-------------------------------------------------------------------------------------

save(imp,file=paste0(workdir,"Data/imputed data - long form.RData"))
