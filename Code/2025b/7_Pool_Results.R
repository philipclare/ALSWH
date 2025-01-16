######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 15 November 2023
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
# workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"
workdir <- "/Volumes/research-data/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"

libs <- c("Amelia")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

names <- c("num_waves_prob","num_waves_lb","num_waves_ub","consec_waves_prob","consec_waves_lb","consec_waves_ub")

######################################################################################
# 2. Primary analysis
#-------------------------------------------------------------------------------------

# 2.1 Load model predicted means
num_mn_p <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-pr-mn-num-",x,".rds"))
})

consec_mn_p <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-pr-mn-consec-",x,".rds"))
})

# 2.2 Combine and restructure results for figures
res_mn_p <- list(num_mn_p,consec_mn_p)

mi_mn_p <- do.call(cbind,lapply(res_mn_p, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se_ic <- do.call(rbind,lapply(y, function (x) {
    x[,2]
  }))
  se_tmle <- do.call(rbind,lapply(y, function (x) {
    x[,3]
  }))
  se_max <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  ic <- cbind(coef,se_ic)[complete.cases(cbind(coef,se_tmle)),]
  tmle <- cbind(coef,se_tmle)[complete.cases(cbind(coef,se_tmle)),]
  max <- cbind(coef,se_max)[complete.cases(cbind(coef,se_tmle)),]
  
  res_ic <- t(do.call(rbind,mi.meld(q = ic[,1:7],
                                    se = ic[,8:14])))
  res_tmle <- t(do.call(rbind,mi.meld(q = tmle[,1:7],
                                      se = tmle[,8:14])))
  res_max <- t(do.call(rbind,mi.meld(q = max[,1:7],
                                     se = max[,8:14])))
  
  res <- cbind(res_ic,res_tmle[,2],res_max[,2])
  
}))

mi_mn_p <- cbind(mi_mn_p[,1],mi_mn_p[,1]-qnorm(0.9975)*mi_mn_p[,4],mi_mn_p[,1]+qnorm(0.9975)*mi_mn_p[,4],
                 mi_mn_p[,5],mi_mn_p[,5]-qnorm(0.9975)*mi_mn_p[,8],mi_mn_p[,5]+qnorm(0.9975)*mi_mn_p[,8])

colnames(mi_mn_p) <- names
mi_mn_p <- cbind(num=c(0,1,2,3,4,5,6),
                 mi_mn_p)

# 2.3 Load model relative risks
num_rr_p <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-pr-rr-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rr_p <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-pr-rr-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

# 2.4 Combine and restructure RRs for tables
res_rr_p <- list(num_rr_p,consec_rr_p)

mi_rr_p <- do.call(cbind,lapply(res_rr_p, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
}))

mi_rr_p <- cbind(mi_rr_p[,1],exp(log(mi_rr_p[,1])-qnorm(0.9975)*mi_rr_p[,2]),exp(log(mi_rr_p[,1])+qnorm(0.9975)*mi_rr_p[,2]),
                 mi_rr_p[,3],exp(log(mi_rr_p[,3])-qnorm(0.9975)*mi_rr_p[,4]),exp(log(mi_rr_p[,3])+qnorm(0.9975)*mi_rr_p[,4]))

colnames(mi_rr_p) <- names
mi_rr_p <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                 mi_rr_p)

# 2.5 Load model risk differences
num_rd_p <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-pr-rd-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rd_p <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-pr-rd-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

# 2.6 Combine and restructure RDs for tables
res_rd_p <- list(num_rd_p,consec_rd_p)

mi_rd_p <- do.call(cbind,lapply(res_rd_p, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
  
}))

mi_rd_p <- cbind(mi_rd_p[,1],exp(log(mi_rd_p[,1])-qnorm(0.9975)*mi_rd_p[,2]),exp(log(mi_rd_p[,1])+qnorm(0.9975)*mi_rd_p[,2]),
                 mi_rd_p[,3],exp(log(mi_rd_p[,3])-qnorm(0.9975)*mi_rd_p[,4]),exp(log(mi_rd_p[,3])+qnorm(0.9975)*mi_rd_p[,4]))

colnames(mi_rd_p) <- names
mi_rd_p <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                 mi_rd_p)

######################################################################################
# 3. Sensitivity analysis 1 - lower cut-point for loneliness
#-------------------------------------------------------------------------------------

# 3.1 Load model predicted means
num_mn_s1 <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-s1-mn-num-",x,".rds"))
})

consec_mn_s1 <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-s1-mn-consec-",x,".rds"))
})

# 3.2 Combine and restructure results for figures
res_mn_s1 <- list(num_mn_s1,consec_mn_s1)

mi_mn_s1 <- do.call(cbind,lapply(res_mn_s1, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se_ic <- do.call(rbind,lapply(y, function (x) {
    x[,2]
  }))
  se_tmle <- do.call(rbind,lapply(y, function (x) {
    x[,3]
  }))
  se_max <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  ic <- cbind(coef,se_ic)[complete.cases(cbind(coef,se_tmle)),]
  tmle <- cbind(coef,se_tmle)[complete.cases(cbind(coef,se_tmle)),]
  max <- cbind(coef,se_max)[complete.cases(cbind(coef,se_tmle)),]
  
  res_ic <- t(do.call(rbind,mi.meld(q = ic[,1:7],
                                    se = ic[,8:14])))
  res_tmle <- t(do.call(rbind,mi.meld(q = tmle[,1:7],
                                      se = tmle[,8:14])))
  res_max <- t(do.call(rbind,mi.meld(q = max[,1:7],
                                     se = max[,8:14])))
  
  res <- cbind(res_ic,res_tmle[,2],res_max[,2])
  
}))

mi_mn_s1 <- cbind(mi_mn_s1[,1],mi_mn_s1[,1]-qnorm(0.9975)*mi_mn_s1[,4],mi_mn_s1[,1]+qnorm(0.9975)*mi_mn_s1[,4],
                  mi_mn_s1[,5],mi_mn_s1[,5]-qnorm(0.9975)*mi_mn_s1[,8],mi_mn_s1[,5]+qnorm(0.9975)*mi_mn_s1[,8])

colnames(mi_mn_s1) <- names
mi_mn_s1 <- cbind(num=c(0,1,2,3,4,5,6),
                  mi_mn_s1)

# 3.3 Load model relative risks
num_rr_s1 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s1-rr-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rr_s1 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s1-rr-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})


# 3.4 Combine and restructure RRs for tables
res_rr_s1 <- list(num_rr_s1,consec_rr_s1)

mi_rr_s1 <- do.call(cbind,lapply(res_rr_s1, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
  
}))

mi_rr_s1 <- cbind(mi_rr_s1[,1],exp(log(mi_rr_s1[,1])-qnorm(0.9975)*mi_rr_s1[,2]),exp(log(mi_rr_s1[,1])+qnorm(0.9975)*mi_rr_s1[,2]),
                  mi_rr_s1[,3],exp(log(mi_rr_s1[,3])-qnorm(0.9975)*mi_rr_s1[,4]),exp(log(mi_rr_s1[,3])+qnorm(0.9975)*mi_rr_s1[,4]))

colnames(mi_rr_s1) <- names
mi_rr_s1 <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                  mi_rr_s1)

# 3.5 Load model risk differences
num_rd_s1 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s1-rd-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rd_s1 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s1-rd-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})


# 3.6 Combine and restructure RRs for tables
res_rd_s1 <- list(num_rd_s1,consec_rd_s1)

mi_rd_s1 <- do.call(cbind,lapply(res_rd_s1, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
  
}))

mi_rd_s1 <- cbind(mi_rd_s1[,1],exp(log(mi_rd_s1[,1])-qnorm(0.9975)*mi_rd_s1[,2]),exp(log(mi_rd_s1[,1])+qnorm(0.9975)*mi_rd_s1[,2]),
                  mi_rd_s1[,3],exp(log(mi_rd_s1[,3])-qnorm(0.9975)*mi_rd_s1[,4]),exp(log(mi_rd_s1[,3])+qnorm(0.9975)*mi_rd_s1[,4]))

colnames(mi_rd_s1) <- names
mi_rd_s1 <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                  mi_rd_s1)

######################################################################################
# 4. Sensitivity analysis 2 - higher cut-point for loneliness
#-------------------------------------------------------------------------------------

# 4.1 Load model predicted means
num_mn_s2 <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-s2-mn-num-",x,".rds"))
})

consec_mn_s2 <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-s2-mn-consec-",x,".rds"))
})

# 4.2 Combine and restructure results for figures
res_mn_s2 <- list(num_mn_s2,consec_mn_s2)

mi_mn_s2 <- do.call(cbind,lapply(res_mn_s2, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se_ic <- do.call(rbind,lapply(y, function (x) {
    x[,2]
  }))
  se_tmle <- do.call(rbind,lapply(y, function (x) {
    x[,3]
  }))
  se_max <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  ic <- cbind(coef,se_ic)[complete.cases(cbind(coef,se_tmle)),]
  tmle <- cbind(coef,se_tmle)[complete.cases(cbind(coef,se_tmle)),]
  max <- cbind(coef,se_max)[complete.cases(cbind(coef,se_tmle)),]
  
  res_ic <- t(do.call(rbind,mi.meld(q = ic[,1:7],
                                    se = ic[,8:14])))
  res_tmle <- t(do.call(rbind,mi.meld(q = tmle[,1:7],
                                      se = tmle[,8:14])))
  res_max <- t(do.call(rbind,mi.meld(q = max[,1:7],
                                     se = max[,8:14])))
  
  res <- cbind(res_ic,res_tmle[,2],res_max[,2])
  
}))

mi_mn_s2 <- cbind(mi_mn_s2[,1],mi_mn_s2[,1]-qnorm(0.9975)*mi_mn_s2[,4],mi_mn_s2[,1]+qnorm(0.9975)*mi_mn_s2[,4],
                  mi_mn_s2[,5],mi_mn_s2[,5]-qnorm(0.9975)*mi_mn_s2[,8],mi_mn_s2[,5]+qnorm(0.9975)*mi_mn_s2[,8])

colnames(mi_mn_s2) <- names
mi_mn_s2 <- cbind(num=c(0,1,2,3,4,5,6),
                  mi_mn_s2)

# 4.3 Load model relative risks
num_rr_s2 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s2-rr-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rr_s2 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s2-rr-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

# 4.4 Combine and restructure RRs for tables
res_rr_s2 <- list(num_rr_s2,consec_rr_s2)

mi_rr_s2 <- do.call(cbind,lapply(res_rr_s2, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
  
}))

mi_rr_s2 <- cbind(mi_rr_s2[,1],exp(log(mi_rr_s2[,1])-qnorm(0.9975)*mi_rr_s2[,2]),exp(log(mi_rr_s2[,1])+qnorm(0.9975)*mi_rr_s2[,2]),
                  mi_rr_s2[,3],exp(log(mi_rr_s2[,3])-qnorm(0.9975)*mi_rr_s2[,4]),exp(log(mi_rr_s2[,3])+qnorm(0.9975)*mi_rr_s2[,4]))

colnames(mi_rr_s2) <- names
mi_rr_s2 <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                  mi_rr_s2)

# 4.5 Load model risk differences
num_rd_s2 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s2-rd-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rd_s2 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s2-rd-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

# 4.6 Combine and restructure RRs for tables
res_rd_s2 <- list(num_rd_s2,consec_rd_s2)

mi_rd_s2 <- do.call(cbind,lapply(res_rd_s2, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
  
}))

mi_rd_s2 <- cbind(mi_rd_s2[,1],exp(log(mi_rd_s2[,1])-qnorm(0.9975)*mi_rd_s2[,2]),exp(log(mi_rd_s2[,1])+qnorm(0.9975)*mi_rd_s2[,2]),
                  mi_rd_s2[,3],exp(log(mi_rd_s2[,3])-qnorm(0.9975)*mi_rd_s2[,4]),exp(log(mi_rd_s2[,3])+qnorm(0.9975)*mi_rd_s2[,4]))

colnames(mi_rd_s2) <- names
mi_rd_s2 <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                  mi_rd_s2)

######################################################################################
# 5. Sensitivity analysis 3 - controlling not excluding
#-------------------------------------------------------------------------------------

# 5.1 Load model predicted means
num_mn_s3 <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-s3-mn-num-",x,".rds"))
})

consec_mn_s3 <- lapply(seq(1,30), function (x) {
  readRDS(file=paste0(workdir,"Results/Katana/ltmle-s3-mn-consec-",x,".rds"))
})

# 5.2 Combine and restructure results for figures
res_mn_s3 <- list(num_mn_s3,consec_mn_s3)
# res_mn_s3 <- list(num_mn_s3,num_mn_s3)

mi_mn_s3 <- do.call(cbind,lapply(res_mn_s3, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se_ic <- do.call(rbind,lapply(y, function (x) {
    x[,2]
  }))
  se_tmle <- do.call(rbind,lapply(y, function (x) {
    x[,3]
  }))
  se_max <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  ic <- cbind(coef,se_ic)[complete.cases(cbind(coef,se_tmle)),]
  tmle <- cbind(coef,se_tmle)[complete.cases(cbind(coef,se_tmle)),]
  max <- cbind(coef,se_max)[complete.cases(cbind(coef,se_tmle)),]
  
  res_ic <- t(do.call(rbind,mi.meld(q = ic[,1:7],
                                    se = ic[,8:14])))
  res_tmle <- t(do.call(rbind,mi.meld(q = tmle[,1:7],
                                      se = tmle[,8:14])))
  res_max <- t(do.call(rbind,mi.meld(q = max[,1:7],
                                     se = max[,8:14])))
  
  res <- cbind(res_ic,res_tmle[,2],res_max[,2])
  
}))

mi_mn_s3 <- cbind(mi_mn_s3[,1],mi_mn_s3[,1]-qnorm(0.9975)*mi_mn_s3[,4],mi_mn_s3[,1]+qnorm(0.9975)*mi_mn_s3[,4],
                  mi_mn_s3[,5],mi_mn_s3[,5]-qnorm(0.9975)*mi_mn_s3[,8],mi_mn_s3[,5]+qnorm(0.9975)*mi_mn_s3[,8])

colnames(mi_mn_s3) <- names
mi_mn_s3 <- cbind(num=c(0,1,2,3,4,5,6),
                  mi_mn_s3)

# 5.3 Load model relative risks
num_rr_s3 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s3-rr-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rr_s3 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s3-rr-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

# 5.4 Combine and restructure RRs for tables
res_rr_s3 <- list(num_rr_s3,consec_rr_s3)
# res_rr_s3 <- list(num_rr_s3,num_rr_s3)

mi_rr_s3 <- do.call(cbind,lapply(res_rr_s3, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
  
}))

mi_rr_s3 <- cbind(mi_rr_s3[,1],exp(log(mi_rr_s3[,1])-qnorm(0.9975)*mi_rr_s3[,2]),exp(log(mi_rr_s3[,1])+qnorm(0.9975)*mi_rr_s3[,2]),
                  mi_rr_s3[,3],exp(log(mi_rr_s3[,3])-qnorm(0.9975)*mi_rr_s3[,4]),exp(log(mi_rr_s3[,3])+qnorm(0.9975)*mi_rr_s3[,4]))

colnames(mi_rr_s3) <- names
mi_rr_s3 <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                  mi_rr_s3)

# 5.5 Load model risk differences
num_rd_s3 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s3-rd-num-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

consec_rd_s3 <- lapply(seq(1,30), function (x) {
  y <- readRDS(file=paste0(workdir,"Results/Katana/ltmle-s3-rd-consec-",x,".rds"))
  y <- matrix(as.vector(y)[1:24],nrow=6)
})

# 5.6 Combine and restructure RRs for tables
res_rd_s3 <- list(num_rd_s3,consec_rd_s3)
# res_rd_s3 <- list(num_rd_s3,num_rd_s3)

mi_rd_s3 <- do.call(cbind,lapply(res_rd_s3, function (y) {
  
  coef <- do.call(rbind,lapply(y, function (x) {
    x[,1]
  }))
  
  se <- do.call(rbind,lapply(y, function (x) {
    x[,4]
  }))
  
  max <- cbind(coef,se)[complete.cases(cbind(coef,se)),]
  
  res <- t(do.call(rbind,mi.meld(q = max[,1:6],
                                 se = max[,7:12])))
  
}))

mi_rd_s3 <- cbind(mi_rd_s3[,1],exp(log(mi_rd_s3[,1])-qnorm(0.9975)*mi_rd_s3[,2]),exp(log(mi_rd_s3[,1])+qnorm(0.9975)*mi_rd_s3[,2]),
                  mi_rd_s3[,3],exp(log(mi_rd_s3[,3])-qnorm(0.9975)*mi_rd_s3[,4]),exp(log(mi_rd_s3[,3])+qnorm(0.9975)*mi_rd_s3[,4]))

colnames(mi_rd_s3) <- names
mi_rd_s3 <- cbind(comp=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0"),
                  mi_rd_s3)

######################################################################################
# 6. Save results
#-------------------------------------------------------------------------------------

saveRDS(mi_mn_p,file=paste0(workdir,"Results/Raw results/primary pooled means.rds"))
saveRDS(mi_rr_p,file=paste0(workdir,"Results/Raw results/primary pooled RRs.rds"))
saveRDS(mi_rd_p,file=paste0(workdir,"Results/Raw results/primary pooled RDs.rds"))
saveRDS(mi_mn_s1,file=paste0(workdir,"Results/Raw results/s1 pooled means.rds"))
saveRDS(mi_rr_s1,file=paste0(workdir,"Results/Raw results/s1 pooled RRs.rds"))
saveRDS(mi_rd_s1,file=paste0(workdir,"Results/Raw results/s1 pooled RDs.rds"))
saveRDS(mi_mn_s2,file=paste0(workdir,"Results/Raw results/s2 pooled means.rds"))
saveRDS(mi_rr_s2,file=paste0(workdir,"Results/Raw results/s2 pooled RRs.rds"))
saveRDS(mi_rd_s2,file=paste0(workdir,"Results/Raw results/s2 pooled RDs.rds"))
saveRDS(mi_mn_s3,file=paste0(workdir,"Results/Raw results/s3 pooled means.rds"))
saveRDS(mi_rr_s3,file=paste0(workdir,"Results/Raw results/s3 pooled RRs.rds"))
saveRDS(mi_rd_s3,file=paste0(workdir,"Results/Raw results/s3 pooled RDs.rds"))