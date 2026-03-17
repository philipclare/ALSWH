######################################################################################
##   
## Effects of physical activity on incident obesity
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 03 July 2025
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://osf.io/fyszg
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())

# 1.1. Specify paths to Katana/windows/Mac paths based on system
if (Sys.info()[['sysname']]=="Linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/Obesity/"
} else if (Sys.info()[['sysname']]=="Windows") {
  workdir <- "Y:/PRJ-prc_alswh/Paper 3 - Obesity/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/Paper 3 - Obesity/" # MAC
}

libs <- c("Amelia","openxlsx")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

num <- seq(0,7)

invlogit <- function (x) {
  exp(x) / (1 + exp(x))
}

get_res_bin <- function (summary) {
  
  n <- summary[[5]]
  sum <- summary[[1]]
  cov.mat_ic <- summary[[2]] / n
  cov.mat_tmle <- summary[[3]] / n
  cov.mat_max <- matrix(summary[[4]],nrow=nrow(cov.mat_tmle)) / n

  rownames(cov.mat_ic) <- colnames(cov.mat_ic) <- rownames(cov.mat_tmle) <- colnames(cov.mat_tmle) <- rownames(cov.mat_max) <- colnames(cov.mat_max) <- rownames(sum$cmat)
  
  coef <- setNames(sum$cmat[,1], rownames(sum$cmat))
  
  pr <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"exp(Intercept+x*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp:max_exp`)/(1+exp((Intercept)+x*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp:max_exp`))",vcov=cov.mat_ic)
  }))
  
  pr
  
  rr <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp:max_exp`)/(1+exp((Intercept)+x*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp:max_exp`)))/(exp((Intercept)+7*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp:max_exp`)/(1+exp((Intercept)+7*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  rr
  
  rd <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp:max_exp`)/(1+exp((Intercept)+x*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp:max_exp`)))-(exp((Intercept)+7*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp:max_exp`)/(1+exp((Intercept)+7*cum_exp+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  rd
  
  list(pr,rr,rd)
}

get_res_sev <- function (summary) {
  
  n <- summary[[5]]
  sum <- summary[[1]]
  cov.mat_ic <- summary[[2]] / n
  cov.mat_tmle <- summary[[3]] / n
  cov.mat_max <- matrix(summary[[4]],nrow=nrow(cov.mat_tmle)) / n
  
  rownames(cov.mat_ic) <- colnames(cov.mat_ic) <- rownames(cov.mat_tmle) <- colnames(cov.mat_tmle) <- rownames(cov.mat_max) <- colnames(cov.mat_max) <- rownames(sum$cmat)
  
  coef <- setNames(sum$cmat[,1], rownames(sum$cmat))
  
  pr <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"exp(Intercept+x*cum_exp+7*max_exp+x*7*`cum_exp:max_exp`)/(1+exp((Intercept)+x*cum_exp+7*max_exp+x*7*`cum_exp:max_exp`))",vcov=cov.mat_ic)
  }))
  
  pr
  
  rr <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp+7*max_exp+x*7*`cum_exp:max_exp`)/(1+exp((Intercept)+x*cum_exp+7*max_exp+x*7*`cum_exp:max_exp`)))/(exp((Intercept)+7*cum_exp+7*max_exp+7*7*`cum_exp:max_exp`)/(1+exp((Intercept)+7*cum_exp+7*max_exp+7*7*`cum_exp:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  rr
  
  rd <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp+7*max_exp+x*7*`cum_exp:max_exp`)/(1+exp((Intercept)+x*cum_exp+7*max_exp+x*7*`cum_exp:max_exp`)))-(exp((Intercept)+7*cum_exp+7*max_exp+7*7*`cum_exp:max_exp`)/(1+exp((Intercept)+7*cum_exp+7*max_exp+7*7*`cum_exp:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  rd
  
  list(pr,rr,rd)
}

get_res_cat <- function (summary) {
  
  n <- summary[[5]]
  sum <- summary[[1]]
  cov.mat_ic <- summary[[2]] / n
  cov.mat_tmle <- summary[[3]] / n
  cov.mat_max <- matrix(summary[[4]],nrow=nrow(cov.mat_tmle)) / n
  
  rownames(cov.mat_ic) <- colnames(cov.mat_ic) <- rownames(cov.mat_tmle) <- colnames(cov.mat_tmle) <- rownames(cov.mat_max) <- colnames(cov.mat_max) <- rownames(sum$cmat)
  
  coef <- setNames(sum$cmat[,1], rownames(sum$cmat))
  
  pr_150 <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"exp(Intercept+x*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_150:max_exp`)/(1+exp((Intercept)+x*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_150:max_exp`))",vcov=cov.mat_ic)
  }))
  
  pr_150
  
  rr_150 <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_150:max_exp`)/(1+exp((Intercept)+x*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_150:max_exp`)))/(exp((Intercept)+7*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_150:max_exp`)/(1+exp((Intercept)+7*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_150:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  rr_150
  
  rd_150 <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_150:max_exp`)/(1+exp((Intercept)+x*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_150:max_exp`)))-(exp((Intercept)+7*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_150:max_exp`)/(1+exp((Intercept)+7*cum_exp_150+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_150:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  pr_300 <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"exp(Intercept+x*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_300:max_exp`)/(1+exp((Intercept)+x*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_300:max_exp`))",vcov=cov.mat_ic)
  }))
  
  pr_300
  
  rr_300 <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_300:max_exp`)/(1+exp((Intercept)+x*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_300:max_exp`)))/(exp((Intercept)+7*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_300:max_exp`)/(1+exp((Intercept)+7*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_300:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  rr_300
  
  rd_300 <- do.call(rbind,lapply(seq(0,7), function (x) {
    car::deltaMethod(coef,"(exp(Intercept+x*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_300:max_exp`)/(1+exp((Intercept)+x*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+x*7*`cum_exp_300:max_exp`)))-(exp((Intercept)+7*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_300:max_exp`)/(1+exp((Intercept)+7*cum_exp_300+7*max_exp+7^2*`I(max_exp^2)`+7^3*`I(max_exp^3)`+7*7*`cum_exp_300:max_exp`)))",vcov=cov.mat_ic)
  }))
  
  list(pr_150,rr_150,rd_150,pr_300,rr_300,rd_300)
}

meld_res <- function(res_list) {
  res <- t(do.call(rbind,mi.meld(q = res_list[,c(1:8)],
                                        se = res_list[,c(9:16)])))
  res <- data.frame(cbind(num,res))
  colnames(res) <- c("num","est","se")
  res$lb <- res$est-(qnorm(0.9975)*res$se)
  res$ub <- res$est+(qnorm(0.9975)*res$se)
  
  res
}

######################################################################################
# 2. Primary analysis
#-------------------------------------------------------------------------------------

# 2.1 Load saved results
sum_pr <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/ltmle-bin-summary-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/primary analysis data - 20240827.rds"))[[x]])
  res
})

res_pr <- lapply(sum_pr,get_res_bin)

mn_pr <- do.call(rbind,lapply(res_pr, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
  }))

rr_pr <- do.call(rbind,lapply(res_pr, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_pr <- do.call(rbind,lapply(res_pr, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 2.2 Combine and restructure results for figures
res_mn_pr <- meld_res(mn_pr)
res_mn_pr$label <- paste0(sprintf('%.1f%%',res_mn_pr$est*100),"\n(",sprintf('%.1f%%',res_mn_pr$lb*100),", ",sprintf('%.1f%%',res_mn_pr$ub*100),")")

res_rr_pr <- meld_res(rr_pr)
res_rr_pr$label <- paste0(sprintf('%.1f',res_rr_pr$est),"\n(",sprintf('%.1f',res_rr_pr$lb),", ",sprintf('%.1f',res_rr_pr$ub),")")

res_rd_pr <- meld_res(rd_pr)
res_rd_pr$label <- paste0(sprintf('%.1f%%',res_rd_pr$est*100),"\n(",sprintf('%.1f%%',res_rd_pr$lb*100),", ",sprintf('%.1f%%',res_rd_pr$ub*100),")")

######################################################################################
# 3. Categorical exposure
#-------------------------------------------------------------------------------------

# 3.1 Load saved results
sum_cat <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/ltmle-cat-summary-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/categorical analysis data - 20240827.rds"))[[x]])
  res
})

res_cat <- lapply(sum_cat,get_res_cat)

mn_150 <- do.call(rbind,lapply(res_cat, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_150 <- do.call(rbind,lapply(res_cat, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_150 <- do.call(rbind,lapply(res_cat, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

mn_300 <- do.call(rbind,lapply(res_cat, function (x) {
  input <- x[[4]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_300 <- do.call(rbind,lapply(res_cat, function (x) {
  input <- x[[5]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_300 <- do.call(rbind,lapply(res_cat, function (x) {
  input <- x[[6]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 3.2 Combine and restructure results for figures
res_mn_150 <- meld_res(mn_150)
res_mn_150$exp <- "meet"
res_mn_300 <- meld_res(mn_300)
res_mn_300$exp <- "exceed"
res_mn_cat <- rbind(res_mn_150,res_mn_300)
res_mn_cat$label <- paste0(sprintf('%.1f%%',res_mn_cat$est*100),"\n(",sprintf('%.1f%%',res_mn_cat$lb*100),", ",sprintf('%.1f%%',res_mn_cat$ub*100),")")

res_rr_150 <- meld_res(rr_150)
res_rr_150$exp <- "meet"
res_rr_300 <- meld_res(rr_300)
res_rr_300$exp <- "exceed"
res_rr_cat <- rbind(res_rr_150,res_rr_300)
res_rr_cat$label <- paste0(sprintf('%.1f',res_rr_cat$est),"\n(",sprintf('%.1f',res_rr_cat$lb),", ",sprintf('%.1f',res_rr_cat$ub),")")

res_rd_150 <- meld_res(rd_150)
res_rd_150$exp <- "meet"
res_rd_300 <- meld_res(rd_300)
res_rd_300$exp <- "exceed"
res_rd_cat <- rbind(res_rd_150,res_rd_300)
res_rd_cat$label <- paste0(sprintf('%.1f%%',res_rd_cat$est*100),"\n(",sprintf('%.1f%%',res_rd_cat$lb*100),", ",sprintf('%.1f%%',res_rd_cat$ub*100),")")

######################################################################################
# 4. Secondary outcome analysis 1 - severe obesity
#-------------------------------------------------------------------------------------

# 4.1 Load saved results
sum_sev <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/severe-summary-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/severe obesity analysis data - 20240827.rds"))[[x]])
  res
})

res_sev <- lapply(sum_sev,get_res_sev)

mn_sev <- do.call(rbind,lapply(res_sev, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_sev <- do.call(rbind,lapply(res_sev, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_sev <- do.call(rbind,lapply(res_sev, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 4.2 Combine and restructure results for figures
res_mn_sev <- meld_res(mn_sev)
res_mn_sev$label <- paste0(sprintf('%.1f%%',res_mn_sev$est*100),"\n(",sprintf('%.1f%%',res_mn_sev$lb*100),", ",sprintf('%.1f%%',res_mn_sev$ub*100),")")

res_rr_sev <- meld_res(rr_sev)
res_rr_sev$label <- paste0(sprintf('%.1f',res_rr_sev$est),"\n(",sprintf('%.1f',res_rr_sev$lb),", ",sprintf('%.1f',res_rr_sev$ub),")")

res_rd_sev <- meld_res(rd_sev)
res_rd_sev$label <- paste0(sprintf('%.1f%%',res_rd_sev$est*100),"\n(",sprintf('%.1f%%',res_rd_sev$lb*100),", ",sprintf('%.1f%%',res_rd_sev$ub*100),")")

######################################################################################
# 5. Secondary outcome analysis 2 - 5% weight gain
#-------------------------------------------------------------------------------------

# 5.1 Load saved results
sum_five <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/five-percent-summary-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/weight gain a analysis data - 20240827.rds"))[[x]])
  res
})

res_five <- lapply(sum_five,get_res_bin)

mn_five <- do.call(rbind,lapply(res_five, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_five <- do.call(rbind,lapply(res_five, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_five <- do.call(rbind,lapply(res_five, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 5.2 Combine and restructure results for figures
res_mn_five <- meld_res(mn_five)
res_mn_five$label <- paste0(sprintf('%.1f%%',res_mn_five$est*100),"\n(",sprintf('%.1f%%',res_mn_five$lb*100),", ",sprintf('%.1f%%',res_mn_five$ub*100),")")

res_rr_five <- meld_res(rr_five)
res_rr_five$label <- paste0(sprintf('%.1f',res_rr_five$est),"\n(",sprintf('%.1f',res_rr_five$lb),", ",sprintf('%.1f',res_rr_five$ub),")")

res_rd_five <- meld_res(rd_five)
res_rd_five$label <- paste0(sprintf('%.1f%%',res_rd_five$est*100),"\n(",sprintf('%.1f%%',res_rd_five$lb*100),", ",sprintf('%.1f%%',res_rd_five$ub*100),")")

######################################################################################
# 6. Secondary outcome analysis 3 - 10% weight gain
#-------------------------------------------------------------------------------------

# 6.1 Load saved results
sum_ten <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/ten-percent-summary-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/weight gain b analysis data - 20240827.rds"))[[x]])
  res
})

res_ten <- lapply(sum_ten,get_res_bin)

mn_ten <- do.call(rbind,lapply(res_ten, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_ten <- do.call(rbind,lapply(res_ten, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_ten <- do.call(rbind,lapply(res_ten, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 6.2 Combine and restructure results for figures
res_mn_ten <- meld_res(mn_ten)
res_mn_ten$label <- paste0(sprintf('%.1f%%',res_mn_ten$est*100),"\n(",sprintf('%.1f%%',res_mn_ten$lb*100),", ",sprintf('%.1f%%',res_mn_ten$ub*100),")")

res_rr_ten <- meld_res(rr_ten)
res_rr_ten$label <- paste0(sprintf('%.1f',res_rr_ten$est),"\n(",sprintf('%.1f',res_rr_ten$lb),", ",sprintf('%.1f',res_rr_ten$ub),")")

res_rd_ten <- meld_res(rd_ten)
res_rd_ten$label <- paste0(sprintf('%.1f%%',res_rd_ten$est*100),"\n(",sprintf('%.1f%%',res_rd_ten$lb*100),", ",sprintf('%.1f%%',res_rd_ten$ub*100),")")

######################################################################################
# 7. Stratified analysis by education
#-------------------------------------------------------------------------------------

# 7.1 Load saved results
sum_educ_low <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/stratified-summary-low_educ-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/stratified analysis data - 20250606.rds"))[[x]][[1]])
  res
})

sum_educ_high <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/stratified-summary-high_educ-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/stratified analysis data - 20250606.rds"))[[x]][[2]])
  res
})

res_educ_low <- lapply(sum_educ_low,get_res_bin)
res_educ_high <- lapply(sum_educ_high,get_res_bin)

mn_educ_low <- do.call(rbind,lapply(res_educ_low, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_educ_low <- do.call(rbind,lapply(res_educ_low, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_educ_low <- do.call(rbind,lapply(res_educ_low, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

mn_educ_high <- do.call(rbind,lapply(res_educ_high, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_educ_high <- do.call(rbind,lapply(res_educ_high, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_educ_high <- do.call(rbind,lapply(res_educ_high, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 7.2 Combine and restructure results for figures
res_mn_educ_low <- meld_res(mn_educ_low)
res_mn_educ_low$educ <- "low"
res_mn_educ_high <- meld_res(mn_educ_high)
res_mn_educ_high$educ <- "high"
res_mn_strat <- rbind(res_mn_educ_low,res_mn_educ_high)
res_mn_strat$educ <- factor(res_mn_strat$educ,
                            levels=c("low","high"),
                            labels=c("High school or less","Tertiary/trade"))
res_mn_strat$label <- paste0(sprintf('%.1f%%',res_mn_strat$est*100),"\n(",sprintf('%.1f%%',res_mn_strat$lb*100),", ",sprintf('%.1f%%',res_mn_strat$ub*100),")")

res_rr_educ_low <- meld_res(rr_educ_low)
res_rr_educ_low$educ <- "low"
res_rr_educ_high <- meld_res(rr_educ_high)
res_rr_educ_high$educ <- "high"
res_rr_strat <- rbind(res_rr_educ_low,res_rr_educ_high)
res_rr_strat$educ <- factor(res_rr_strat$educ,
                            levels=c("low","high"),
                            labels=c("High school or less","Tertiary/trade"))
res_rr_strat$label <- paste0(sprintf('%.1f',res_rr_strat$est),"\n(",sprintf('%.1f',res_rr_strat$lb),", ",sprintf('%.1f',res_rr_strat$ub),")")

res_rd_educ_low <- meld_res(rd_educ_low)
res_rd_educ_low$educ <- "low"
res_rd_educ_high <- meld_res(rd_educ_high)
res_rd_educ_high$educ <- "high"
res_rd_strat <- rbind(res_rd_educ_low,res_rd_educ_high)
res_rd_strat$educ <- factor(res_rd_strat$educ,
                            levels=c("low","high"),
                            labels=c("High school or less","Tertiary/trade"))
res_rd_strat$label <- paste0(sprintf('%.1f%%',res_rd_strat$est*100),"\n(",sprintf('%.1f%%',res_rd_strat$lb*100),", ",sprintf('%.1f%%',res_rd_strat$ub*100),")")

######################################################################################
# 8. Sensitivity analysis 1 - Descendant adjustment
#-------------------------------------------------------------------------------------

# 8.1 Load saved results
sum_sns1 <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/sens1-summary-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/sensitivity analysis data - 20250606.rds"))[[x]])
  res
})

res_sns1 <- lapply(sum_sns1,get_res_bin)

mn_sns1 <- do.call(rbind,lapply(res_sns1, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_sns1 <- do.call(rbind,lapply(res_sns1, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_sns1 <- do.call(rbind,lapply(res_sns1, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 8.2 Combine and restructure results for figures
res_mn_sns1 <- meld_res(mn_sns1)
res_mn_sns1$label <- paste0(sprintf('%.1f%%',res_mn_sns1$est*100),"\n(",sprintf('%.1f%%',res_mn_sns1$lb*100),", ",sprintf('%.1f%%',res_mn_sns1$ub*100),")")

res_rr_sns1 <- meld_res(rr_sns1)
res_rr_sns1$label <- paste0(sprintf('%.1f',res_rr_sns1$est),"\n(",sprintf('%.1f',res_rr_sns1$lb),", ",sprintf('%.1f',res_rr_sns1$ub),")")

res_rd_sns1 <- meld_res(rd_sns1)
res_rd_sns1$label <- paste0(sprintf('%.1f%%',res_rd_sns1$est*100),"\n(",sprintf('%.1f%%',res_rd_sns1$lb*100),", ",sprintf('%.1f%%',res_rd_sns1$ub*100),")")

######################################################################################
# 9. Sensitivity analysis 2 - Omitting wholly imputed variables
#-------------------------------------------------------------------------------------

# 9.1 Load saved results
sum_sns2 <- lapply(seq(1,25), function (x) {
  res <- readRDS(paste0(workdir,"Results/Katana Output/sens2-summary-",x,".rds")) 
  res[[5]] <- nrow(readRDS(paste0(workdir,"Data/primary analysis data - 20240827.rds"))[[x]])
  res
})

res_sns2 <- lapply(sum_sns2,get_res_bin)

mn_sns2 <- do.call(rbind,lapply(res_sns2, function (x) {
  input <- x[[1]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rr_sns2 <- do.call(rbind,lapply(res_sns2, function (x) {
  input <- x[[2]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

rd_sns2 <- do.call(rbind,lapply(res_sns2, function (x) {
  input <- x[[3]]
  input <- c(as.vector(input[,1]),as.vector(input[,2]))
}))

# 9.2 Combine and restructure results for figures
res_mn_sns2 <- meld_res(mn_sns2)
res_mn_sns2$label <- paste0(sprintf('%.1f%%',res_mn_sns2$est*100),"\n(",sprintf('%.1f%%',res_mn_sns2$lb*100),", ",sprintf('%.1f%%',res_mn_sns2$ub*100),")")

res_rr_sns2 <- meld_res(rr_sns2)
res_rr_sns2$label <- paste0(sprintf('%.1f',res_rr_sns2$est),"\n(",sprintf('%.1f',res_rr_sns2$lb),", ",sprintf('%.1f',res_rr_sns2$ub),")")

res_rd_sns2 <- meld_res(rd_sns2)
res_rd_sns2$label <- paste0(sprintf('%.1f%%',res_rd_sns2$est*100),"\n(",sprintf('%.1f%%',res_rd_sns2$lb*100),", ",sprintf('%.1f%%',res_rd_sns2$ub*100),")")

######################################################################################
# 10. Save results
#-------------------------------------------------------------------------------------

saveRDS(res_mn_pr,file=paste0(workdir,"Results/Processed/means - binary.rds"))
saveRDS(res_rr_pr,file=paste0(workdir,"Results/Processed/rrs - binary.rds"))
saveRDS(res_rd_pr,file=paste0(workdir,"Results/Processed/rds - binary.rds"))

saveRDS(res_mn_cat,file=paste0(workdir,"Results/Processed/means - categorical.rds"))
saveRDS(res_rr_cat,file=paste0(workdir,"Results/Processed/rrs - categorical.rds"))
saveRDS(res_rd_cat,file=paste0(workdir,"Results/Processed/rds - categorical.rds"))

saveRDS(res_mn_sev,file=paste0(workdir,"Results/Processed/means - severe outcome.rds"))
saveRDS(res_rr_sev,file=paste0(workdir,"Results/Processed/rrs - severe outcome.rds"))
saveRDS(res_rd_sev,file=paste0(workdir,"Results/Processed/rds - severe outcome.rds"))

saveRDS(res_mn_five,file=paste0(workdir,"Results/Processed/means - five percent outcome.rds"))
saveRDS(res_rr_five,file=paste0(workdir,"Results/Processed/rrs - five percent outcome.rds"))
saveRDS(res_rd_five,file=paste0(workdir,"Results/Processed/rds - five percent outcome.rds"))

saveRDS(res_mn_ten,file=paste0(workdir,"Results/Processed/means - ten percent outcome.rds"))
saveRDS(res_rr_ten,file=paste0(workdir,"Results/Processed/rrs - ten percent outcome.rds"))
saveRDS(res_rd_ten,file=paste0(workdir,"Results/Processed/rds - ten percent outcome.rds"))

saveRDS(res_mn_strat,file=paste0(workdir,"Results/Processed/means - education stratified.rds"))
saveRDS(res_rr_strat,file=paste0(workdir,"Results/Processed/rrs - education stratified.rds"))
saveRDS(res_rd_strat,file=paste0(workdir,"Results/Processed/rds - education stratified.rds"))

saveRDS(res_mn_sns1,file=paste0(workdir,"Results/Processed/means - sensitivity 1 - descendant adjustment.rds"))
saveRDS(res_rr_sns1,file=paste0(workdir,"Results/Processed/rrs - sensitivity 1 - descendant adjustment.rds"))
saveRDS(res_rd_sns1,file=paste0(workdir,"Results/Processed/rds - sensitivity 1 - descendant adjustment.rds"))

saveRDS(res_mn_sns2,file=paste0(workdir,"Results/Processed/means - sensitivity 2 - drop fully imputed.rds"))
saveRDS(res_rr_sns2,file=paste0(workdir,"Results/Processed/rrs - sensitivity 2 - drop fully imputed.rds"))
saveRDS(res_rd_sns2,file=paste0(workdir,"Results/Processed/rds - sensitivity 2 - drop fully imputed.rds"))

######################################################################################
# 11. Save summary output to excel for tables
#-------------------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb,"Primary")
addWorksheet(wb,"Categorical")
addWorksheet(wb,"Severe")
addWorksheet(wb,"Five_percent")
addWorksheet(wb,"Ten_percent")
addWorksheet(wb,"Stratified")
addWorksheet(wb,"Sensitivity1")
addWorksheet(wb,"Sensitivity2")

writeData(wb,sheet="Primary","Probability",startRow=1)
writeData(wb,sheet="Primary",res_mn_pr,startRow=2)
writeData(wb,sheet="Primary","Risk ratio",startRow=12)
writeData(wb,sheet="Primary",res_rr_pr,startRow=13)
writeData(wb,sheet="Primary","Risk difference",startRow=23)
writeData(wb,sheet="Primary",res_rd_pr,startRow=24)

writeData(wb,sheet="Categorical","Probability",startRow=1)
writeData(wb,sheet="Categorical",res_mn_cat,startRow=2)
writeData(wb,sheet="Categorical","Risk ratio",startRow=20)
writeData(wb,sheet="Categorical",res_rr_cat,startRow=21)
writeData(wb,sheet="Categorical","Risk difference",startRow=39)
writeData(wb,sheet="Categorical",res_rd_cat,startRow=40)

writeData(wb,sheet="Severe","Probability",startRow=1)
writeData(wb,sheet="Severe",res_mn_sev,startRow=2)
writeData(wb,sheet="Severe","Risk ratio",startRow=12)
writeData(wb,sheet="Severe",res_rr_sev,startRow=13)
writeData(wb,sheet="Severe","Risk difference",startRow=23)
writeData(wb,sheet="Severe",res_rd_sev,startRow=24)

writeData(wb,sheet="Five_percent","Probability",startRow=1)
writeData(wb,sheet="Five_percent",res_mn_five,startRow=2)
writeData(wb,sheet="Five_percent","Risk ratio",startRow=12)
writeData(wb,sheet="Five_percent",res_rr_five,startRow=13)
writeData(wb,sheet="Five_percent","Risk difference",startRow=23)
writeData(wb,sheet="Five_percent",res_rd_five,startRow=24)

writeData(wb,sheet="Ten_percent","Probability",startRow=1)
writeData(wb,sheet="Ten_percent",res_mn_ten,startRow=2)
writeData(wb,sheet="Ten_percent","Risk ratio",startRow=12)
writeData(wb,sheet="Ten_percent",res_rr_ten,startRow=13)
writeData(wb,sheet="Ten_percent","Risk difference",startRow=23)
writeData(wb,sheet="Ten_percent",res_rd_ten,startRow=24)

writeData(wb,sheet="Stratified","Probability",startRow=1)
writeData(wb,sheet="Stratified",res_mn_strat,startRow=2)
writeData(wb,sheet="Stratified","Risk ratio",startRow=20)
writeData(wb,sheet="Stratified",res_rr_strat,startRow=21)
writeData(wb,sheet="Stratified","Risk difference",startRow=39)
writeData(wb,sheet="Stratified",res_rd_strat,startRow=40)

writeData(wb,sheet="Sensitivity1","Probability",startRow=1)
writeData(wb,sheet="Sensitivity1",res_mn_sns1,startRow=2)
writeData(wb,sheet="Sensitivity1","Risk ratio",startRow=12)
writeData(wb,sheet="Sensitivity1",res_rr_sns1,startRow=13)
writeData(wb,sheet="Sensitivity1","Risk difference",startRow=23)
writeData(wb,sheet="Sensitivity1",res_rd_sns1,startRow=24)

writeData(wb,sheet="Sensitivity2","Probability",startRow=1)
writeData(wb,sheet="Sensitivity2",res_mn_sns2,startRow=2)
writeData(wb,sheet="Sensitivity2","Risk ratio",startRow=12)
writeData(wb,sheet="Sensitivity2",res_rr_sns2,startRow=13)
writeData(wb,sheet="Sensitivity2","Risk difference",startRow=23)
writeData(wb,sheet="Sensitivity2",res_rd_sns2,startRow=24)

saveWorkbook(wb, file = paste0(workdir,"Results/model-results.xlsx"), overwrite = TRUE)

