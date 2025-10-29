##############################################################################
##  
## Project: Trends in loneliness and isolation in Australia
## Program: C2 - Missing Data.R
## Purpose: Calculate percentage of missing information to see if imputation is needed
## Author: Philip Clare
## Date: 24 October 2023
## OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
##  
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------
# 1.0 Time Imputation
start_time <- Sys.time()

# 1.1 Specify paths
workdir <- "Y:/PRJ-hilda_data/Loneliness trends/"

# 1.2 Check install and load required libraries
libs <- c("mice","haven","naniar","parameters","rpart","VIM")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}

library(parallel)
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2.Load data
#-----------------------------------------------------------------------------

datalong <- read_dta(file=paste0(workdir,"Data/Combined data.dta")) # In this case, the data frame object in this file is named 'datalong'

datalong <- zap_formats(datalong)
datalong <- zap_labels(datalong)
datalong <- zap_label(datalong)

datalong <- datalong[,-c(13,15,16)]

f_list <- c("agecat","sex","educ","cob","language","marital","living","aria","seifa","fiprosp")

datalong[,f_list] <- lapply(datalong[f_list] , factor)

# datalong$hgage <- datalong$hgage-16

res<-summary(aggr(datalong))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
datalong <- datalong[,res$Variable]
gg_miss_upset(datalong, nintersects = 30)
sum(is.na(datalong))/(ncol(datalong)*nrow(datalong))

##############################################################################
# 3. Define Imputation Parameters
#-----------------------------------------------------------------------------

m <- 4 # Number of imputations
maxit <- 10; # Number of mice iterations
cluster.seed <- 91624 # Needs to be set within function for parallel computing
numcores <- 4 #as.numeric(Sys.getenv('NCPUS')) # Number of cores to use (default = ncore-1)
method <- c("rf","rf","rf","rf")

##############################################################################
# 4. Imputation
#-----------------------------------------------------------------------------

# 3.3 Parallel imputation using parlmice
imp_mice <- futuremice(m = m,
                       data = datalong,
                       parallelseed = cluster.seed,
                       n.core = numcores,
                       maxit = maxit,
                       defaultmethod = method)

imp_mice <- mids2datlist(imp_mice)

##############################################################################
# 5. Save
# 5.1 Save imputation
save(imp_mice, file=paste0(workdir,"imputed-mice ",args[1],".RData"))

# 5.2 Calculate Time
end_time <- Sys.time()
time_taken_mice <- end_time - start_time

cat('Using mice, ', m, 'imputations with ', maxit, 'iterations took:', time_taken_mice, attr(time_taken_mice,"units"), ".","\n")

##############################################################################
################################### END ######################################
##############################################################################