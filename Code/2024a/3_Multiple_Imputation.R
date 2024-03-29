######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Multiple imputation using random forests
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
  workdir <- "R:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("furrr","mice","miceadds","VIM","UpSetR","ggplot2","naniar")
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
res <- summary(aggr(imp_data,plot=FALSE))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- imp_data[,res$Variable]

######################################################################################
# 3. Check missingness
#-------------------------------------------------------------------------------------

outcome_data <- imp_data[which(imp_data$wave==9 | imp_data$wave==2),c("pcsa", "mcsa", "pf", "rp", "bp", "vt", "gh", "re", "sf", "mh")]
exposure_data <- imp_data[which(imp_data$wave<=8 & imp_data$wave>=3),c("weighted_activity_time")]
confounder_data <- imp_data[which(imp_data$wave<=7 & imp_data$wave>=2),c("marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
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

m <- 40 # number of imputations
n <- 8 # number of cores for parlmice to use
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

imp[[41]] <- dataimp

imp <- lapply(imp, function (x,varorder) {
  x <- x[,varorder]
},varorder=varorder)

######################################################################################
# 6. Save data
#-------------------------------------------------------------------------------------

save(imp,file=paste0(workdir,"Data/imputed data - long form.RData"))
