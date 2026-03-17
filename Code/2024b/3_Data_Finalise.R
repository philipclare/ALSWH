######################################################################################
##   
## Syntax File 3
## Physical Activity Latent Class Analysis
## Finalise data after imputation and format for analysis
## Date: 6 October 2022
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "R:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/"

libs <- c("plyr","dplyr","ltmle","gtools","haven")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load imputed data
#-------------------------------------------------------------------------------------

load(file=paste0(workdir,"Data/imputed data.RData"))
load(paste0(workdir,"Data/imputation data.RData"))

######################################################################################
# 3. Create computed/derived variables
#-------------------------------------------------------------------------------------

# 3.1 Add variable identifying which imputation, and merge in the unimputed data

imp_data$imp <- 0
for (i in seq(1,m)) {
  imp[[i]]$imp <- i
}
imp[[21]] <- imp_data

# 3.2 Create derived variables and drop originals

imp <- lapply(imp,function (x) {
  x$b_alcliferisk <- ifelse(x$b_alcfq>10,1,0)
  x$b_alcepisrisk <- ifelse(x$b_alcbng>1,1,0)
  x$b_alcrisk <- ifelse(x$b_alcliferisk==1 | x$b_alcepisrisk==1,1,0)
  x$activity_bin3 <- ifelse(x$weighted_activity_time3>=150,1,0)
  x$activity_bin4 <- ifelse(x$weighted_activity_time4>=150,1,0)
  x$activity_bin5 <- ifelse(x$weighted_activity_time5>=150,1,0)
  x$activity_bin6 <- ifelse(x$weighted_activity_time6>=150,1,0)
  x$activity_bin7 <- ifelse(x$weighted_activity_time7>=150,1,0)
  x$activity_bin8 <- ifelse(x$weighted_activity_time8>=150,1,0)
  x$activity_bin_sens3 <- ifelse(x$weighted_activity_time3>=75,1,0)
  x$activity_bin_sens4 <- ifelse(x$weighted_activity_time4>=75,1,0)
  x$activity_bin_sens5 <- ifelse(x$weighted_activity_time5>=75,1,0)
  x$activity_bin_sens6 <- ifelse(x$weighted_activity_time6>=75,1,0)
  x$activity_bin_sens7 <- ifelse(x$weighted_activity_time7>=75,1,0)
  x$activity_bin_sens8 <- ifelse(x$weighted_activity_time8>=75,1,0)

  x[,c("b_alcliferisk","b_alcepisrisk","b_alcrisk")] <- lapply(x[,c("b_alcliferisk","b_alcepisrisk","b_alcrisk")], factor, labels=c("No","Yes"))
  x <- subset(x, select = -c(b_alcfq,b_alcbng,weighted_activity_time3,weighted_activity_time4,weighted_activity_time5,
                             weighted_activity_time6,weighted_activity_time7,weighted_activity_time8,inarea))
  x
})

######################################################################################
# 4. Create two versions of the data, with primary and sensitivity activity variables
#-------------------------------------------------------------------------------------

# 4.1 Create two separate lists
imp_primary <- lapply(imp, function (x) {
  subset(x, select=-c(activity_bin_sens3,activity_bin_sens4,activity_bin_sens5,activity_bin_sens6,activity_bin_sens7,activity_bin_sens8))
})

imp_sensitivity <- lapply(imp, function (x) {
  x <- subset(x, select=-c(activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens3,
           activity_bin4 = activity_bin_sens4,
           activity_bin5 = activity_bin_sens5,
           activity_bin6 = activity_bin_sens6,
           activity_bin7 = activity_bin_sens7,
           activity_bin8 = activity_bin_sens8)
})

# 4.2 Collapse into an expanded data frame for export to Stata

imp_primary_stata <- do.call(rbind,imp_primary)
imp_sensitivity_stata <- do.call(rbind,imp_sensitivity)

######################################################################################
# 5. Save final, analysis-ready dataset
#-------------------------------------------------------------------------------------

# 5.1 Save R data as lists
save(imp_primary,file=paste0(workdir,"Data/primary imputed data.RData"))
save(imp_sensitivity,file=paste0(workdir,"Data/sensitivity imputed data.RData"))

# 5.2 Save Stata data as dataframes
write_dta(imp_primary_stata,path=paste0(workdir,"Data/primary imputed data.dta"))
write_dta(imp_sensitivity_stata,path=paste0(workdir,"Data/sensitivity imputed data.dta"))
