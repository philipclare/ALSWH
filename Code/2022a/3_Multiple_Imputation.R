######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Multiple Imputation using Random Forests
## Date: 2 September 2022
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/"

libs <- c("mice","miceadds","VIM")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

set.seed(966495)

##############################################################################
# 2. Load and process data
#-----------------------------------------------------------------------------

load(paste0(workdir,"Physical activity trajectories/Data/imputation data.RData.RData"))

# Sort data from most to least missing, saving order to return data to original order if needed
res<-summary(aggr(imp_data))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- imp_data[,res$Variable]

##############################################################################
# 3. Define Imputation Parameters
#-----------------------------------------------------------------------------

m <- 40 # number of imputations
n <- 4 # number of cores for parlmice to use
nimpcore <- m/n
maxit <- 2; # Number of mice iterations
default <- c("rf","rf","rf","rf") # Manually defined list of methods for each variable type

##############################################################################
# 4. Run multiple imputation using MICE with random forests
#-----------------------------------------------------------------------------

# 4.1 Imputation using mice

imp_mice <- parlmice(data=dataimp,
                     m=m,
                     n.core=n,
                     n.imp.core=nimpcore,
                     maxit=maxit,
                     defaultMethod=default,
                     clusterseed=767693)

imp <- mids2datlist(imp_mice)

##############################################################################
# 5. Save data for use in R or Stata
#-----------------------------------------------------------------------------

save(imp,file=paste0(workdir,"Physical activity trajectories/Data/imputed data - long form.RData"))

