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

workdir <- "R:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"

libs <- c("ltmle","parallel","Amelia","ggplot2")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

read_rdata <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"])
}

names <- c("pcsa_mean","pcsa_se","mcsa_mean","mcsa_se",
           "pf_mean","pf_se","rp_mean","rp_se",
           "bp_mean","bp_se","gh_mean","gh_se",
           "vt_mean","vt_se","sf_mean","sf_se",
           "re_mean","re_se","mh_mean","mh_se")

######################################################################################
# 2. Primary Analysis
#-------------------------------------------------------------------------------------

# 2.1. Load model fit objects 
p_pcsa_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pcsa9-1.RData")),
                 read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pcsa9-2.RData")),
                 read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pcsa9-3.RData")),
                 read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pcsa9-4.RData")))
p_mcsa_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mcsa9-1.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mcsa9-2.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mcsa9-3.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mcsa9-4.RData")))
p_pf_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pf9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pf9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pf9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-pf9-4.RData")))
p_rp_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-rp9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-rp9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-rp9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-rp9-4.RData")))
p_bp_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-bp9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-bp9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-bp9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-bp9-4.RData")))
p_gh_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-gh9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-gh9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-gh9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-gh9-4.RData")))
p_vt_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-vt9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-vt9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-vt9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-vt9-4.RData")))
p_sf_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-sf9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-sf9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-sf9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-sf9-4.RData")))
p_re_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-re9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-re9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-re9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-re9-4.RData")))
p_mh_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mh9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mh9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mh9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/primary-results-mh9-4.RData")))

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

# 2.3 Assign column names and separate out sustained and inititiation results
colnames(primary_mi_res) <- names
primary_mi_res_sust <- as.data.frame(primary_mi_res[1:6,])
primary_mi_res_init <- as.data.frame(primary_mi_res[c(6:10,1),])
primary_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
primary_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 3. Sensitivity analysis 1
#-------------------------------------------------------------------------------------

# 3.1. Load model fit objects 
s1_pcsa_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-1.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-2.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-3.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pcsa9-4.RData")))
s1_mcsa_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-1.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-2.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-3.RData")),
                  read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mcsa9-4.RData")))
s1_pf_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-pf9-4.RData")))
s1_rp_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-rp9-4.RData")))
s1_bp_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-bp9-4.RData")))
s1_gh_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-gh9-4.RData")))
s1_vt_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-vt9-4.RData")))
s1_sf_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-sf9-4.RData")))
s1_re_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-re9-4.RData")))
s1_mh_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-1.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-2.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-3.RData")),
                read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-1-results-mh9-4.RData")))

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
s2_pcsa_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-1.RData")),
                     read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-2.RData")),
                     read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-3.RData")),
                     read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pcsa9-4.RData")))
s2_mcsa_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-1.RData")),
                     read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-2.RData")),
                     read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-3.RData")),
                     read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mcsa9-4.RData")))
s2_pf_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-pf9-4.RData")))
s2_rp_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-rp9-4.RData")))
s2_bp_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-bp9-4.RData")))
s2_gh_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-gh9-4.RData")))
s2_vt_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-vt9-4.RData")))
s2_sf_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-sf9-4.RData")))
s2_re_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-re9-4.RData")))
s2_mh_res <- rbind(read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-1.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-2.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-3.RData")),
                   read_rdata(file=paste0(workdir,"Results/Katana output/sensitivity-2-results-mh9-4.RData")))

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
# 5. Save results
#-------------------------------------------------------------------------------------

save(primary_mi_res_sust,file=paste0(workdir,"Results/primary-sustained-results.RData"))
save(primary_mi_res_init,file=paste0(workdir,"Results/primary-iniation-results.RData"))

save(sensitivity_1_mi_res_sust,file=paste0(workdir,"Results/sensitivity_1-sustained-results.RData"))
save(sensitivity_1_mi_res_init,file=paste0(workdir,"Results/sensitivity_1-iniation-results.RData"))

save(sensitivity_2_mi_res_sust,file=paste0(workdir,"Results/sensitivity_2-sustained-results.RData"))
save(sensitivity_2_mi_res_init,file=paste0(workdir,"Results/sensitivity_2-iniation-results.RData"))

