######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 4 October 2022
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
  workdir <- "Y:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("ltmle","parallel","Amelia","ggplot2")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

names <- c("pcsa_mean","pcsa_se","mcsa_mean","mcsa_se",
           "pf_mean","pf_se","rp_mean","rp_se",
           "bp_mean","bp_se","gh_mean","gh_se",
           "vt_mean","vt_se","sf_mean","sf_se",
           "re_mean","re_se","mh_mean","mh_se")

######################################################################################
# 2. Primary Analysis
#-------------------------------------------------------------------------------------

# 2.1. Load model fit objects 
p_pcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pcsa9-1.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pcsa9-2.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pcsa9-3.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pcsa9-4.rds")))
p_mcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mcsa9-1.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mcsa9-2.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mcsa9-3.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mcsa9-4.rds")))
p_pf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pf9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pf9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pf9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-pf9-4.rds")))
p_rp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-rp9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-rp9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-rp9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-rp9-4.rds")))
p_bp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-bp9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-bp9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-bp9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-bp9-4.rds")))
p_gh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-gh9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-gh9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-gh9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-gh9-4.rds")))
p_vt_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-vt9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-vt9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-vt9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-vt9-4.rds")))
p_sf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-sf9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-sf9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-sf9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-sf9-4.rds")))
p_re_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-re9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-re9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-re9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-re9-4.rds")))
p_mh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mh9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mh9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mh9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-means-mh9-4.rds")))

# 2.2. Combine into single list of results
primary_res <- list(p_pcsa_res,p_mcsa_res,
                    p_pf_res,p_rp_res,p_bp_res,p_gh_res,
                    p_vt_res,p_sf_res,p_re_res,p_mh_res)

# 2.3. Pool MI results using Rubin's rules
primary_mi_res <- do.call(cbind,lapply(primary_res, function (y) {
  
  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
}))

# 2.3 Assign column names and separate out sustained and initiation results
colnames(primary_mi_res) <- names
primary_mi_res_sust <- as.data.frame(primary_mi_res[1:6,])
primary_mi_res_init <- as.data.frame(primary_mi_res[c(6:10,1),])
primary_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
primary_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 3. Sensitivity analysis 1
#-------------------------------------------------------------------------------------

# 3.1. Load model fit objects 
s1_pcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-1.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-2.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-3.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-4.rds")))
s1_mcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-1.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-2.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-3.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-4.rds")))
s1_pf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-4.rds")))
s1_rp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-4.rds")))
s1_bp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-4.rds")))
s1_gh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-4.rds")))
s1_vt_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-4.rds")))
s1_sf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-4.rds")))
s1_re_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-4.rds")))
s1_mh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-4.rds")))

# 3.2. Combine into single list of results
sensitivity_1_res <- list(s1_pcsa_res,s1_mcsa_res,
                          s1_pf_res,s1_rp_res,s1_bp_res,s1_gh_res,
                          s1_vt_res,s1_sf_res,s1_re_res,s1_mh_res)

# 3.3. Pool MI results using Rubin's rules
sensitivity_1_mi_res <- do.call(cbind,lapply(sensitivity_1_res, function (y) {
  
  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
}))

# 3.4 Assign column names and separate out sustained and inititiation results
colnames(sensitivity_1_mi_res) <- names
sensitivity_1_mi_res_sust <- as.data.frame(sensitivity_1_mi_res[1:6,])
sensitivity_1_mi_res_init <- as.data.frame(sensitivity_1_mi_res[c(6:10,1),])
sensitivity_1_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
sensitivity_1_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 4. Sensitivity analysis 2
#-------------------------------------------------------------------------------------

# 4.1. Load model fit objects 
s2_pcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-1.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-2.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-3.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-4.rds")))
s2_mcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-1.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-2.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-3.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-4.rds")))
s2_pf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-4.rds")))
s2_rp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-4.rds")))
s2_bp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-4.rds")))
s2_gh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-4.rds")))
s2_vt_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-4.rds")))
s2_sf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-4.rds")))
s2_re_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-4.rds")))
s2_mh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-4.rds")))

# 4.2. Combine into single list of results
sensitivity_2_res <- list(s2_pcsa_res,s2_mcsa_res,
                          s2_pf_res,s2_rp_res,s2_bp_res,s2_gh_res,
                          s2_vt_res,s2_sf_res,s2_re_res,s2_mh_res)

# 4.3. Pool MI results using Rubin's rules
sensitivity_2_mi_res <- do.call(cbind,lapply(sensitivity_2_res, function (y) {
  
  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
}))

# 4.4 Assign column names and separate out sustained and inititiation results
colnames(sensitivity_2_mi_res) <- names
sensitivity_2_mi_res_sust <- as.data.frame(sensitivity_2_mi_res[1:6,])
sensitivity_2_mi_res_init <- as.data.frame(sensitivity_2_mi_res[c(6:10,1),])
sensitivity_2_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
sensitivity_2_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 5. Sensitivity analysis 3
#-------------------------------------------------------------------------------------

# 5.1. Load model fit objects 
s3_pcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pcsa9-1.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pcsa9-2.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pcsa9-3.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pcsa9-4.rds")))
s3_mcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mcsa9-1.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mcsa9-2.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mcsa9-3.rds")),
                     readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mcsa9-4.rds")))
s3_pf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pf9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pf9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pf9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-pf9-4.rds")))
s3_rp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-rp9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-rp9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-rp9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-rp9-4.rds")))
s3_bp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-bp9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-bp9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-bp9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-bp9-4.rds")))
s3_gh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-gh9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-gh9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-gh9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-gh9-4.rds")))
s3_vt_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-vt9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-vt9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-vt9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-vt9-4.rds")))
s3_sf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-sf9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-sf9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-sf9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-sf9-4.rds")))
s3_re_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-re9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-re9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-re9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-re9-4.rds")))
s3_mh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mh9-1.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mh9-2.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mh9-3.rds")),
                   readRDS(file=paste0(workdir,"Results/Katana output/sensitivity-3-results-mh9-4.rds")))

# 5.2. Combine into single list of results
sensitivity_3_res <- list(s3_pcsa_res,s3_mcsa_res,
                          s3_pf_res,s3_rp_res,s3_bp_res,s3_gh_res,
                          s3_vt_res,s3_sf_res,s3_re_res,s3_mh_res)

# 5.3. Pool MI results using Rubin's rules
sensitivity_3_mi_res <- do.call(cbind,lapply(sensitivity_3_res, function (y) {
  
  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
}))

# 5.4 Assign column names and separate out sustained and inititiation results
colnames(sensitivity_3_mi_res) <- names
sensitivity_3_mi_res_sust <- as.data.frame(sensitivity_3_mi_res[1:6,])
sensitivity_3_mi_res_init <- as.data.frame(sensitivity_3_mi_res[c(6:10,1),])
sensitivity_3_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
sensitivity_3_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 6. Save results
#-------------------------------------------------------------------------------------

saveRDS(primary_mi_res_sust,file=paste0(workdir,"Results/Processed/primary-sustained-results.rds"))
saveRDS(primary_mi_res_init,file=paste0(workdir,"Results/Processed/primary-iniation-results.rds"))

saveRDS(sensitivity_1_mi_res_sust,file=paste0(workdir,"Results/Processed/sensitivity_1-sustained-results.rds"))
saveRDS(sensitivity_1_mi_res_init,file=paste0(workdir,"Results/Processed/sensitivity_1-iniation-results.rds"))

saveRDS(sensitivity_2_mi_res_sust,file=paste0(workdir,"Results/Processed/sensitivity_2-sustained-results.rds"))
saveRDS(sensitivity_2_mi_res_init,file=paste0(workdir,"Results/Processed/sensitivity_2-iniation-results.rds"))

saveRDS(sensitivity_3_mi_res_sust,file=paste0(workdir,"Results/Processed/sensitivity_3-sustained-results.rds"))
saveRDS(sensitivity_3_mi_res_init,file=paste0(workdir,"Results/Processed/sensitivity_3-iniation-results.rds"))

