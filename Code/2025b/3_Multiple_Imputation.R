######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Multiple imputation using random forests
## Date: 24 July 2023
## Authors: Philip Clare and Neta Hagani
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Specify paths to Katana/windows PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh_lonely/"
  
} else { # Manually defined for PC
  workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"
}

libs <- c("dplyr","ggplot2","haven","mice","miceadds","naniar","parallel","purrr","UpSetR","VIM")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

args <- as.numeric(commandArgs(trailingOnly = TRUE))

set.seed(432455)
seeds <- sample.int(100000, 30)
set.seed(seeds[args])

######################################################################################
# 2. Load and process data
#-------------------------------------------------------------------------------------

load(paste0(workdir,"Data/imputation data.RData"))

imp_data <- zap_labels(imp_data)
imp_data <- zap_label(imp_data)
imp_data <- zap_formats(imp_data)

# Sort data from most to least missing, saving order to return data to original order if needed
res<-summary(aggr(imp_data))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- imp_data[,res$Variable]

######################################################################################
# 3. Define Imputation Parameters
#-------------------------------------------------------------------------------------

m <- 1 # number of imputations
# n <- 30 # number of cores for futuremice to use
maxit <- 100 # Number of mice iterations
default <- c("rf","rf","rf","rf") # Manually defined list of methods for each variable type

######################################################################################
# 4. Run multiple imputation using MICE with random forests
#-------------------------------------------------------------------------------------

start <- Sys.time()
imp_mice <- mice(data=dataimp,
                 m=m,
                 maxit=maxit,
                 defaultMethod=default)

imp <- mids2datlist(imp_mice)

end <- Sys.time()
end-start

######################################################################################
# 5. Save data
#-------------------------------------------------------------------------------------

saveRDS(imp,file=paste0(workdir,"Data/imputed data,",args,".rds"))
