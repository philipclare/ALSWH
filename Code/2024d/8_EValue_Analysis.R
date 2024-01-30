######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## EValue analysis for unmeasured confounding
## Date: 15 November 2023
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Set working directory
workdir <- "R:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("Amelia","EValue","xlsx")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load RRs and pool using rubin's rules
#-------------------------------------------------------------------------------------

# 2.1. Load model fit objects 
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

mi_rr_p <- as.data.frame(cbind(mi_rr_p[,1],mi_rr_p[,1]-qnorm(0.975)*mi_rr_p[,2],mi_rr_p[,1]+qnorm(0.975)*mi_rr_p[,2],
                               mi_rr_p[,3],mi_rr_p[,3]-qnorm(0.975)*mi_rr_p[,4],mi_rr_p[,3]+qnorm(0.975)*mi_rr_p[,4]))

colnames(mi_rr_p) <- c("num","low","high","consec","low","high")
mi_rr_p <- cbind(comp=factor(c(1,2,3,4,5,6),levels=c(1,2,3,4,5,6),labels=c("1vs0","2vs0","3vs0","4vs0","5vs0","6vs0")),
                 mi_rr_p)

######################################################################################
# 3. Calculate E-Values
#-------------------------------------------------------------------------------------

evalues_pr <- do.call(rbind,lapply(seq(1,6), function (x) {
  
  c(evalues.RR(mi_rr_p[x,2],mi_rr_p[x,3],mi_rr_p[x,4])[2,],
    evalues.RR(mi_rr_p[x,5],mi_rr_p[x,6],mi_rr_p[x,7])[2,])
  
}))

######################################################################################
# 4. Create data summary matrix
#-------------------------------------------------------------------------------------

evalue_summary <- evalues_pr[,c(1,2,4,5)]

######################################################################################
# 5. Update excel file with evalue results
#-------------------------------------------------------------------------------------

wb <- loadWorkbook(paste0(workdir,"Results/Raw results/Evalue.xlsx"))
sheet <- getSheets(wb)$Raw
addDataFrame(evalue_summary,
                   sheet)
saveWorkbook(wb, paste0(workdir,"Results/Raw results/Evalue.xlsx"))
