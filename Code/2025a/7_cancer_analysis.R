######################################################################################
##   
## Effects of physical activity on mortality
## LTMLE MSM Analysis of dynamic regimes of physical activity on cancer mortality
## Date: 19 December 2022
## OSF Registration: https://osf.io/pytzx
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
workdir <- "//surefsn025/ProfileR025$/philipclare/Documents/ALSWH/"

libs <- c("SuperLearner","glmnet","ranger","arm","ltmle","parallel")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

set.seed(697924)

######################################################################################
# 2. Define LTMLE parameters 
#-------------------------------------------------------------------------------------

theta.set <- c(45,50,55,60,65,70)

sum.measures <- array(dim = c(10, 10, 1))
sum.measures[, , 1] <- matrix(c(1,0,0,0,0,0,0,0,0,0,
                                0,1,0,0,0,0,0,0,0,0,
                                0,0,1,0,0,0,0,0,0,0,
                                0,0,0,1,0,0,0,0,0,0,
                                0,0,0,0,1,0,0,0,0,0,
                                0,0,0,0,0,1,0,0,0,0,
                                0,0,0,0,0,0,1,0,0,0,
                                0,0,0,0,0,0,0,1,0,0,
                                0,0,0,0,0,0,0,0,1,0,
                                0,0,0,0,0,0,0,0,0,1),
                              nrow=10,byrow=TRUE)
colnames(sum.measures) <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70","start_50","start_55","start_60","start_65")

msm.formula <- "Y ~ stop_50 + stop_55 + stop_60 + stop_65 + stop_70 + start_50 + start_55 + start_60 + start_65"

######################################################################################
# 3. Define included variables and functional forms 
#-------------------------------------------------------------------------------------

ynodes <- c("death4","death5","death6","death7","death8","death9")
cnodes <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")
anodes <- c("activity_bin3","activity_bin4","activity_bin5","activity_bin6","activity_bin7","activity_bin8")
lnodes <- c("age3","marital3","ariapgp3","employ3","seifadis3","live_u183","live_o183","cancer_3yr3","arthritis_3yr3","depression_3yr3","anxiety_3yr3","cesd103","mnstrs3","whobmigroup3","vegetables3","fruit3","alcliferisk3","alcepisrisk3","smokst3","pcsa3","mcsa3",
            "age4","marital4","ariapgp4","employ4","seifadis4","live_u184","live_o184","cancer_3yr4","arthritis_3yr4","depression_3yr4","anxiety_3yr4","cesd104","mnstrs4","whobmigroup4","vegetables4","fruit4","alcliferisk4","alcepisrisk4","smokst4","pcsa4","mcsa4",
            "age5","marital5","ariapgp5","employ5","seifadis5","live_u185","live_o185","cancer_3yr5","arthritis_3yr5","depression_3yr5","anxiety_3yr5","cesd105","mnstrs5","whobmigroup5","vegetables5","fruit5","alcliferisk5","alcepisrisk5","smokst5","pcsa5","mcsa5",
            "age6","marital6","ariapgp6","employ6","seifadis6","live_u186","live_o186","cancer_3yr6","arthritis_3yr6","depression_3yr6","anxiety_3yr6","cesd106","mnstrs6","whobmigroup6","vegetables6","fruit6","alcliferisk6","alcepisrisk6","smokst6","pcsa6","mcsa6",
            "age7","marital7","ariapgp7","employ7","seifadis7","live_u187","live_o187","cancer_3yr7","arthritis_3yr7","depression_3yr7","anxiety_3yr7","cesd107","mnstrs7","whobmigroup7","vegetables7","fruit7","alcliferisk7","alcepisrisk7","smokst7","pcsa7","mcsa7")

q_base <- "Q.kplus1 ~ b_pcsa + b_mcsa + b_gh + b_pf + b_re + b_rp + b_cobcat + b_bp + b_educ + b_mh + b_vt + b_sf + b_cancer_ever + b_depression_ever + b_anxiety_ever"
g_base <- "b_pcsa + b_mcsa + b_gh + b_pf + b_re + b_rp + b_cobcat + b_bp + b_educ + b_mh + b_vt + b_sf + b_cancer_ever + b_depression_ever + b_anxiety_ever"
w2_conf <- "marital2 + age2 + ariapgp2 + employ2 + seifadis2 + live_u182 + live_o182 + cesd102 + mnstrs2 + whobmigroup2 + vegetables2 + fruit2 + alcliferisk2 + alcepisrisk2 + smokst2"
w3_conf <- "marital3 + age3 + ariapgp3 + employ3 + seifadis3 + live_u183 + live_o183 + cancer_3yr3 + arthritis_3yr3 + depression_3yr3 + anxiety_3yr3 + cesd103 + mnstrs3 + whobmigroup3 + vegetables3 + fruit3 + alcliferisk3 + alcepisrisk3 + smokst3"
w4_conf <- "marital4 + age4 + ariapgp4 + employ4 + seifadis4 + live_u184 + live_o184 + cancer_3yr4 + arthritis_3yr4 + depression_3yr4 + anxiety_3yr4 + cesd104 + mnstrs4 + whobmigroup4 + vegetables4 + fruit4 + alcliferisk4 + alcepisrisk4 + smokst4"
w5_conf <- "marital5 + age5 + ariapgp5 + employ5 + seifadis5 + live_u185 + live_o185 + cancer_3yr5 + arthritis_3yr5 + depression_3yr5 + anxiety_3yr5 + cesd105 + mnstrs5 + whobmigroup5 + vegetables5 + fruit5 + alcliferisk5 + alcepisrisk5 + smokst5"
w6_conf <- "marital6 + age6 + ariapgp6 + employ6 + seifadis6 + live_u186 + live_o186 + cancer_3yr6 + arthritis_3yr6 + depression_3yr6 + anxiety_3yr6 + cesd106 + mnstrs6 + whobmigroup6 + vegetables6 + fruit6 + alcliferisk6 + alcepisrisk6 + smokst6"
w7_conf <- "marital7 + age7 + ariapgp7 + employ7 + seifadis7 + live_u187 + live_o187 + cancer_3yr7 + arthritis_3yr7 + depression_3yr7 + anxiety_3yr7 + cesd107 + mnstrs7 + whobmigroup7 + vegetables7 + fruit7 + alcliferisk7 + alcepisrisk7 + smokst7"

l3_form <- paste(q_base,                w2_conf,sep=" + ")
l4_form <- paste(q_base,"activity_bin3",w3_conf,sep=" + ")
l5_form <- paste(q_base,"activity_bin4",w4_conf,sep=" + ")
l6_form <- paste(q_base,"activity_bin5",w5_conf,sep=" + ")
l7_form <- paste(q_base,"activity_bin6",w6_conf,sep=" + ")

y4_form <- paste(q_base,"activity_bin3",w3_conf,sep=" + ")
y5_form <- paste(q_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + ")
y6_form <- paste(q_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + ")
y7_form <- paste(q_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + ")
y8_form <- paste(q_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,"activity_bin5",w5_conf,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + ")
y9_form <- paste(q_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,"activity_bin5",w5_conf,"activity_bin6",w6_conf,"activity_bin7",w7_conf,"activity_bin8",sep=" + ")

c3_form <- paste0("censored3 ~ ",    paste(g_base,                                        w2_conf,sep=" + "))
g3_form <- paste0("activity_bin3 ~ ",paste(g_base,                                        w2_conf,sep=" + "))
c4_form <- paste0("censored4 ~ ",    paste(g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + "))
g4_form <- paste0("activity_bin4 ~ ",paste(g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + "))
c5_form <- paste0("censored5 ~ ",    paste(g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + "))
g5_form <- paste0("activity_bin5 ~ ",paste(g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + "))
c6_form <- paste0("censored6 ~ ",    paste(g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + "))
g6_form <- paste0("activity_bin6 ~ ",paste(g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + "))
c7_form <- paste0("censored7 ~ ",    paste(g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + "))
g7_form <- paste0("activity_bin7 ~ ",paste(g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + "))
c8_form <- paste0("censored8 ~ ",    paste(g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + "))
g8_form <- paste0("activity_bin8 ~ ",paste(g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + "))
c9_form <- paste0("censored9 ~ ",    paste(g_base,"activity_bin7",w7_conf,"activity_bin8",sep=" + "))

qform <- c(               age3=l3_form,
                          death4=y4_form,age4=l4_form,
                          death5=y5_form,age5=l5_form,
                          death6=y6_form,age6=l6_form,
                          death7=y7_form,age7=l7_form,
                          death8=y8_form,
                          death9=y9_form)
gform <- c(censored3=c3_form,activity_bin3=g3_form,
           censored4=c4_form,activity_bin4=g4_form,
           censored5=c5_form,activity_bin5=g5_form,
           censored6=c6_form,activity_bin6=g6_form,
           censored7=c7_form,activity_bin7=g7_form,
           censored8=c8_form,activity_bin8=g8_form,
           censored9=c9_form)

######################################################################################
# 4. Run LTMLE models 
#-------------------------------------------------------------------------------------

load(paste0(workdir,"Data/cancer analysis - primary.RData"))
start_cancer_p <- Sys.time()
res_cancer_p <- lapply(cancer_p,function (x) {
  
  regimes <- array(dim = c(dim(x)[1], 6, 10)) #n x num.Anodes x num.regimes
  cnt <- 0
  for (theta.index in 1:6) {
    cnt <- cnt + 1
    regimes[, 1, cnt] <- x$age2 < theta.set[theta.index]
    regimes[, 2, cnt] <- x$age3 < theta.set[theta.index]
    regimes[, 3, cnt] <- x$age4 < theta.set[theta.index]
    regimes[, 4, cnt] <- x$age5 < theta.set[theta.index]
    regimes[, 5, cnt] <- x$age6 < theta.set[theta.index]
    regimes[, 6, cnt] <- x$age7 < theta.set[theta.index]
  }
  for (theta.index in 2:5) {
    cnt <- cnt + 1
    regimes[, 1, cnt] <- x$age2 > theta.set[theta.index]
    regimes[, 2, cnt] <- x$age3 > theta.set[theta.index]
    regimes[, 3, cnt] <- x$age4 > theta.set[theta.index]
    regimes[, 4, cnt] <- x$age5 > theta.set[theta.index]
    regimes[, 5, cnt] <- x$age6 > theta.set[theta.index]
    regimes[, 6, cnt] <- x$age7 > theta.set[theta.index]
  }
  
  model_fit_msm <- suppressWarnings(ltmleMSM(x,
                                             Cnodes = cnodes,
                                             Anodes = anodes,
                                             Lnodes = lnodes,
                                             Ynodes = ynodes,
                                             Qform = qform,
                                             gform = gform,
                                             regimes = regimes,
                                             summary.measures = sum.measures,
                                             working.msm = msm.formula,
                                             survivalOutcome = TRUE,
                                             observation.weights = x$b_wtarea))
  
  res <- c(c(plogis(summary(model_fit_msm)$cmat[1,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[2,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[3,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[4,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[5,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[6,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[7,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[8,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[9,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[10,1])))
  
  cov.mat <- var(model_fit_msm$IC)
  
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
  
  v <- do.call(rbind,lapply(gradient, function(z,model_fit_msm) {
    v <- t(z) %*% cov.mat %*% z
    std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
  },model_fit_msm=model_fit_msm))
  
  coef <- c(plogis(summary(model_fit_msm)$cmat[1,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[2,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[3,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[4,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[5,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[6,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[7,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[8,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[9,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[10,1]))
  
  fin_res <- matrix(cbind(coef,v),nrow=10)
  
})
end_cancer_p <- Sys.time()
rm(cancer_p)

load(paste0(workdir,"Data/cancer analysis - s1.RData"))
start_cancer_s1 <- Sys.time()
res_cancer_s1 <- lapply(cancer_s1,function (x) {
  
  regimes <- array(dim = c(dim(x)[1], 6, 10)) #n x num.Anodes x num.regimes
  cnt <- 0
  for (theta.index in 1:6) {
    cnt <- cnt + 1
    regimes[, 1, cnt] <- x$age2 < theta.set[theta.index]
    regimes[, 2, cnt] <- x$age3 < theta.set[theta.index]
    regimes[, 3, cnt] <- x$age4 < theta.set[theta.index]
    regimes[, 4, cnt] <- x$age5 < theta.set[theta.index]
    regimes[, 5, cnt] <- x$age6 < theta.set[theta.index]
    regimes[, 6, cnt] <- x$age7 < theta.set[theta.index]
  }
  for (theta.index in 2:5) {
    cnt <- cnt + 1
    regimes[, 1, cnt] <- x$age2 > theta.set[theta.index]
    regimes[, 2, cnt] <- x$age3 > theta.set[theta.index]
    regimes[, 3, cnt] <- x$age4 > theta.set[theta.index]
    regimes[, 4, cnt] <- x$age5 > theta.set[theta.index]
    regimes[, 5, cnt] <- x$age6 > theta.set[theta.index]
    regimes[, 6, cnt] <- x$age7 > theta.set[theta.index]
  }
  
  model_fit_msm <- suppressWarnings(ltmleMSM(x,
                                             Cnodes = cnodes,
                                             Anodes = anodes,
                                             Lnodes = lnodes,
                                             Ynodes = ynodes,
                                             Qform = qform,
                                             gform = gform,
                                             regimes = regimes,
                                             summary.measures = sum.measures,
                                             working.msm = msm.formula,
                                             survivalOutcome = TRUE,
                                             observation.weights = x$b_wtarea))
  
  res <- c(c(plogis(summary(model_fit_msm)$cmat[1,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[2,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[3,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[4,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[5,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[6,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[7,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[8,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[9,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[10,1])))
  
  cov.mat <- var(model_fit_msm$IC)
  
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
  
  v <- do.call(rbind,lapply(gradient, function(z,model_fit_msm) {
    v <- t(z) %*% cov.mat %*% z
    std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
  },model_fit_msm=model_fit_msm))
  
  coef <- c(plogis(summary(model_fit_msm)$cmat[1,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[2,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[3,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[4,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[5,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[6,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[7,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[8,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[9,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[10,1]))
  
  fin_res <- matrix(cbind(coef,v),nrow=10)
  
})
end_cancer_s1 <- Sys.time()
rm(cancer_s1)

load(paste0(workdir,"Data/cancer analysis - s2.RData"))
start_cancer_s2 <- Sys.time()
res_cancer_s2 <- lapply(cancer_s2,function (x) {
  
  regimes <- array(dim = c(dim(x)[1], 6, 10)) #n x num.Anodes x num.regimes
  cnt <- 0
  for (theta.index in 1:6) {
    cnt <- cnt + 1
    regimes[, 1, cnt] <- x$age2 < theta.set[theta.index]
    regimes[, 2, cnt] <- x$age3 < theta.set[theta.index]
    regimes[, 3, cnt] <- x$age4 < theta.set[theta.index]
    regimes[, 4, cnt] <- x$age5 < theta.set[theta.index]
    regimes[, 5, cnt] <- x$age6 < theta.set[theta.index]
    regimes[, 6, cnt] <- x$age7 < theta.set[theta.index]
  }
  for (theta.index in 2:5) {
    cnt <- cnt + 1
    regimes[, 1, cnt] <- x$age2 > theta.set[theta.index]
    regimes[, 2, cnt] <- x$age3 > theta.set[theta.index]
    regimes[, 3, cnt] <- x$age4 > theta.set[theta.index]
    regimes[, 4, cnt] <- x$age5 > theta.set[theta.index]
    regimes[, 5, cnt] <- x$age6 > theta.set[theta.index]
    regimes[, 6, cnt] <- x$age7 > theta.set[theta.index]
  }
  
  model_fit_msm <- suppressWarnings(ltmleMSM(x,
                                             Cnodes = cnodes,
                                             Anodes = anodes,
                                             Lnodes = lnodes,
                                             Ynodes = ynodes,
                                             Qform = qform,
                                             gform = gform,
                                             regimes = regimes,
                                             summary.measures = sum.measures,
                                             working.msm = msm.formula,
                                             survivalOutcome = TRUE,
                                             observation.weights = x$b_wtarea))
  
  res <- c(c(plogis(summary(model_fit_msm)$cmat[1,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[2,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[3,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[4,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[5,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[6,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[7,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[8,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[9,1]),
             plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[10,1])))
  
  cov.mat <- var(model_fit_msm$IC)
  
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
  
  v <- do.call(rbind,lapply(gradient, function(z,model_fit_msm) {
    v <- t(z) %*% cov.mat %*% z
    std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
  },model_fit_msm=model_fit_msm))
  
  coef <- c(plogis(summary(model_fit_msm)$cmat[1,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[2,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[3,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[4,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[5,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[6,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[7,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[8,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[9,1]),
            plogis(summary(model_fit_msm)$cmat[1,1]+summary(model_fit_msm)$cmat[10,1]))
  
  fin_res <- matrix(cbind(coef,v),nrow=10)
  
})
end_cancer_s2 <- Sys.time()
rm(cancer_s2)

######################################################################################
# 5. Save output
#-------------------------------------------------------------------------------------

saveRDS(res_cancer_p,file=paste0(workdir,"Results/cancer-primary-results.rds"))
saveRDS(res_cancer_s1,file=paste0(workdir,"Results/cancer-sens-1-results.rds"))
saveRDS(res_cancer_s2,file=paste0(workdir,"Results/cancer-sens-2-results.rds"))

cancertiming <- c(end_cancer_p-start_cancer_p,
                  end_cancer_s1-start_cancer_s1,
                  end_cancer_s2-start_cancer_s2)
cancertiming
saveRDS(cancertiming,file=paste0(workdir,"Results/cancer-timing-tests.rds"))