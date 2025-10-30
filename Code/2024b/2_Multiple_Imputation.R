######################################################################################
##   
## Syntax File 2
## Physical Activity Latent Class Analysis
## Multiple imputation using random forests
## Date: 6 October 2022
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "R:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/"

libs <- c("mice","miceadds","VIM","UpSetR","ggplot2","naniar")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

set.seed(966495)

######################################################################################
# 2. Load and process data
#-------------------------------------------------------------------------------------

load(paste0(workdir,"Data/imputation data.RData"))

######################################################################################
# 3. Process data and assess missingness
#-------------------------------------------------------------------------------------

# 3.1 Sort data from most to least missing, saving order to return data to original order if needed
res<-summary(aggr(imp_data))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- imp_data[,res$Variable]

# 3.2 Assess amount and patterns of missing data
mean(is.na(dataimp))
mean(complete.cases(dataimp))

gg_miss_upset(dataimp,
              nintersects = 15)

######################################################################################
# 4. Define Imputation Parameters
#-------------------------------------------------------------------------------------

m <- 20 # number of imputations
n <- 4 # number of cores for parlmice to use
nimpcore <- m/n
maxit <- 50; # Number of mice iterations
default <- c("rf","rf","rf","rf") # Manually defined list of methods for each variable type

######################################################################################
# 5. Run multiple imputation using MICE with random forests
#-------------------------------------------------------------------------------------

imp_mice <- parlmice(data=dataimp,
                     m=m,
                     n.core=n,
                     n.imp.core=nimpcore,
                     maxit=maxit,
                     defaultMethod=default,
                     clusterseed=564110)
end <- Sys.time()

imp <- mids2datlist(imp_mice)

######################################################################################
# 6. Save data
#-------------------------------------------------------------------------------------

save(imp,file=paste0(workdir,"Data/imputed data.RData"))
