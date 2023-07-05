######################################################################################
##   
## Effects of physical activity on mortality
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 19 December 2022
## OSF Registration: https://osf.io/pytzx
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
workdir <- "//surefsn025/ProfileR025$/philipclare/Documents/ALSWH/"

libs <- c("Amelia")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

names <- c("primary_prob","primary_se","s1_prob","s1_se","s2_prob","s2_se")

######################################################################################
# 2. All-cause analysis
#-------------------------------------------------------------------------------------

# 2.1 Load saved results
res_ac_p <- readRDS(file=paste0(workdir,"Results/all-cause-primary-results.rds"))
res_ac_s1 <- readRDS(file=paste0(workdir,"Results/all-cause-sens-1-results.rds"))
res_ac_s2 <- readRDS(file=paste0(workdir,"Results/all-cause-sens-2-results.rds"))

# 2.2 Combine and restructure results for figures
primary_res <- list(res_ac_p,res_ac_s1,res_ac_s2)

primary_mi_res <- do.call(cbind,lapply(primary_res, function (y) {
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q = coef,
                                 se = se)))
}))

colnames(primary_mi_res) <- names
primary_mi_res_sust <- as.data.frame(primary_mi_res[1:6,])
primary_mi_res_init <- as.data.frame(primary_mi_res[c(6:10,1),])
primary_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
primary_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 3. CVD analysis
#-------------------------------------------------------------------------------------

# 3.1 Load saved results
res_cvd_p <- readRDS(file=paste0(workdir,"Results/CVD-primary-results.rds"))
res_cvd_s1 <- readRDS(file=paste0(workdir,"Results/CVD-sens-1-results.rds"))
res_cvd_s2 <- readRDS(file=paste0(workdir,"Results/CVD-sens-2-results.rds"))

# 3.2 Combine and restructure results for figures
cvd_res <- list(res_cvd_p,res_cvd_s1,res_cvd_s2)

cvd_mi_res <- do.call(cbind,lapply(cvd_res, function (y) {
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q = coef,
                                 se = se)))
}))

colnames(cvd_mi_res) <- names
cvd_mi_res_sust <- as.data.frame(cvd_mi_res[1:6,])
cvd_mi_res_init <- as.data.frame(cvd_mi_res[c(6:10,1),])
cvd_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
cvd_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 4. CVD analysis
#-------------------------------------------------------------------------------------

# 4.1 Load saved results
res_cancer_p <- readRDS(file=paste0(workdir,"Results/cancer-primary-results.rds"))
res_cancer_s1 <- readRDS(file=paste0(workdir,"Results/cancer-sens-1-results.rds"))
res_cancer_s2 <- readRDS(file=paste0(workdir,"Results/cancer-sens-2-results.rds"))

# 3.2 Combine and restructure results for figures
cancer_res <- list(res_cancer_p,res_cancer_s1,res_cancer_s2)

cancer_mi_res <- do.call(cbind,lapply(cancer_res, function (y) {
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q = coef,
                                 se = se)))
}))

colnames(cancer_mi_res) <- names
cancer_mi_res_sust <- as.data.frame(cancer_mi_res[1:6,])
cancer_mi_res_init <- as.data.frame(cancer_mi_res[c(6:10,1),])
cancer_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
cancer_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 5. Save results
#-------------------------------------------------------------------------------------

saveRDS(primary_mi_res_sust,file=paste0(workdir,"Results/primary results - sustained.rds"))
saveRDS(primary_mi_res_init,file=paste0(workdir,"Results/primary results - initiation.rds"))

saveRDS(cvd_mi_res_sust,file=paste0(workdir,"Results/cvd results - sustained.rds"))
saveRDS(cvd_mi_res_init,file=paste0(workdir,"Results/cvd results - initiation.rds"))

saveRDS(cancer_mi_res_sust,file=paste0(workdir,"Results/cancer results - sustained.rds"))
saveRDS(cancer_mi_res_init,file=paste0(workdir,"Results/cancer results - initiation.rds"))
