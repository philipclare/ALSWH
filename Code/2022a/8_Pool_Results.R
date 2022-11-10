######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 16 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/Physical activity trajectories/"

libs <- c("ltmle","parallel","threadr","Amelia","ggplot2")
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
primary_fit <- list(pcsa_fit = read_rdata(file=paste0(workdir,"Results/primary-results-pcsa9.RData")),
                 mcsa_fit = read_rdata(file=paste0(workdir,"Results/primary-results-mcsa9.RData")),
                 pf_fit = read_rdata(file=paste0(workdir,"Results/primary-results-pf9.RData")),
                 rp_fit = read_rdata(file=paste0(workdir,"Results/primary-results-rp9.RData")),
                 bp_fit = read_rdata(file=paste0(workdir,"Results/primary-results-bp9.RData")),
                 gh_fit = read_rdata(file=paste0(workdir,"Results/primary-results-gh9.RData")),
                 vt_fit = read_rdata(file=paste0(workdir,"Results/primary-results-vt9.RData")),
                 sf_fit = read_rdata(file=paste0(workdir,"Results/primary-results-sf9.RData")),
                 re_fit = read_rdata(file=paste0(workdir,"Results/primary-results-re9.RData")),
                 mh_fit = read_rdata(file=paste0(workdir,"Results/primary-results-mh9.RData")))

# 2.2. Extract means and SEs from model fits
primary_res <- lapply(primary_fit, function (y) {
  
  res <- lapply(y,function (x) {
    
    res <- c(c(plogis(summary(x)$cmat[1,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])))
    
    cov.mat <- var(x$IC)
    
    gradient <- list(c(res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[2]*(1-res[2]), res[2]*(1-res[2]), 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[3]*(1-res[3]), 0, res[3]*(1-res[3]), 0, 0, 0, 0, 0, 0, 0),
                     c(res[4]*(1-res[4]), 0, 0, res[4]*(1-res[4]), 0, 0, 0, 0, 0, 0),
                     c(res[5]*(1-res[5]), 0, 0, 0, res[5]*(1-res[5]), 0, 0, 0, 0, 0),
                     c(res[6]*(1-res[6]), 0, 0, 0, 0, res[6]*(1-res[6]), 0, 0, 0, 0),
                     c(res[7]*(1-res[7]), 0, 0, 0, 0, 0, res[7]*(1-res[7]), 0, 0, 0),
                     c(res[8]*(1-res[8]), 0, 0, 0, 0, 0, 0, res[8]*(1-res[8]), 0, 0),
                     c(res[9]*(1-res[9]), 0, 0, 0, 0, 0, 0, 0, res[9]*(1-res[9]), 0),
                     c(res[10]*(1-res[10]), 0, 0, 0, 0, 0, 0, 0, 0, res[10]*(1-res[10])))
    
    v <- do.call(rbind,lapply(gradient, function(z,x) {
      v <- t(z) %*% cov.mat %*% z
      std.dev <- sqrt(v[1, 1] / dim(x$IC)[1])*100
    },x=x))
    
    coef <- c(plogis(summary(x)$cmat[1,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])*100)
    
    fin_res <- matrix(cbind(coef,v),nrow=10)
    
  })
  
})

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

# 2.4 Assign column names and separate out sustained and inititiation results
colnames(primary_mi_res) <- names
primary_mi_res_sust <- as.data.frame(primary_mi_res[1:6,])
primary_mi_res_init <- as.data.frame(primary_mi_res[c(6:10,1),])
primary_mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
primary_mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

######################################################################################
# 3. Sensitivity analysis 1
#-------------------------------------------------------------------------------------

# 3.1. Load model fit objects 
sensitivity_1_fit <- list(pcsa_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-pcsa9.RData")),
                    mcsa_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-mcsa9.RData")),
                    pf_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-pf9.RData")),
                    rp_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-rp9.RData")),
                    bp_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-bp9.RData")),
                    gh_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-gh9.RData")),
                    vt_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-vt9.RData")),
                    sf_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-sf9.RData")),
                    re_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-re9.RData")),
                    mh_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-1-results-mh9.RData")))

# 3.2. Extract means and SEs from model fits
sensitivity_1_res <- lapply(sensitivity_1_fit, function (y) {
  
  res <- lapply(y,function (x) {
    
    res <- c(c(plogis(summary(x)$cmat[1,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])))
    
    cov.mat <- var(x$IC)
    
    gradient <- list(c(res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[2]*(1-res[2]), res[2]*(1-res[2]), 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[3]*(1-res[3]), 0, res[3]*(1-res[3]), 0, 0, 0, 0, 0, 0, 0),
                     c(res[4]*(1-res[4]), 0, 0, res[4]*(1-res[4]), 0, 0, 0, 0, 0, 0),
                     c(res[5]*(1-res[5]), 0, 0, 0, res[5]*(1-res[5]), 0, 0, 0, 0, 0),
                     c(res[6]*(1-res[6]), 0, 0, 0, 0, res[6]*(1-res[6]), 0, 0, 0, 0),
                     c(res[7]*(1-res[7]), 0, 0, 0, 0, 0, res[7]*(1-res[7]), 0, 0, 0),
                     c(res[8]*(1-res[8]), 0, 0, 0, 0, 0, 0, res[8]*(1-res[8]), 0, 0),
                     c(res[9]*(1-res[9]), 0, 0, 0, 0, 0, 0, 0, res[9]*(1-res[9]), 0),
                     c(res[10]*(1-res[10]), 0, 0, 0, 0, 0, 0, 0, 0, res[10]*(1-res[10])))
    
    v <- do.call(rbind,lapply(gradient, function(x) {
      v <- t(x) %*% cov.mat %*% x
      std.dev <- sqrt(v[1, 1] / dim(ltmle_data)[1])*100
    }))
    
    coef <- c(plogis(summary(x)$cmat[1,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])*100)
    
    fin_res <- matrix(cbind(coef,v),nrow=10)
    
  })
  
})

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
sensitivity_2_fit <- list(pcsa_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-pcsa9.RData")),
                          mcsa_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-mcsa9.RData")),
                          pf_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-pf9.RData")),
                          rp_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-rp9.RData")),
                          bp_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-bp9.RData")),
                          gh_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-gh9.RData")),
                          vt_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-vt9.RData")),
                          sf_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-sf9.RData")),
                          re_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-re9.RData")),
                          mh_fit = read_rdata(file=paste0(workdir,"Results/sensitivity-2-results-mh9.RData")))

# 4.2. Extract means and SEs from model fits
sensitivity_2_res <- lapply(sensitivity_2_fit, function (y) {
  
  res <- lapply(y,function (x) {
    
    res <- c(c(plogis(summary(x)$cmat[1,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])))
    
    cov.mat <- var(x$IC)
    
    gradient <- list(c(res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[2]*(1-res[2]), res[2]*(1-res[2]), 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[3]*(1-res[3]), 0, res[3]*(1-res[3]), 0, 0, 0, 0, 0, 0, 0),
                     c(res[4]*(1-res[4]), 0, 0, res[4]*(1-res[4]), 0, 0, 0, 0, 0, 0),
                     c(res[5]*(1-res[5]), 0, 0, 0, res[5]*(1-res[5]), 0, 0, 0, 0, 0),
                     c(res[6]*(1-res[6]), 0, 0, 0, 0, res[6]*(1-res[6]), 0, 0, 0, 0),
                     c(res[7]*(1-res[7]), 0, 0, 0, 0, 0, res[7]*(1-res[7]), 0, 0, 0),
                     c(res[8]*(1-res[8]), 0, 0, 0, 0, 0, 0, res[8]*(1-res[8]), 0, 0),
                     c(res[9]*(1-res[9]), 0, 0, 0, 0, 0, 0, 0, res[9]*(1-res[9]), 0),
                     c(res[10]*(1-res[10]), 0, 0, 0, 0, 0, 0, 0, 0, res[10]*(1-res[10])))
    
    v <- do.call(rbind,lapply(gradient, function(x) {
      v <- t(x) %*% cov.mat %*% x
      std.dev <- sqrt(v[1, 1] / dim(ltmle_data)[1])*100
    }))
    
    coef <- c(plogis(summary(x)$cmat[1,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])*100)
    
    fin_res <- matrix(cbind(coef,v),nrow=10)
    
  })
  
})

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

