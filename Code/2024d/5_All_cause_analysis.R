######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## LTMLE MSM Analysis
## Date: 2 November 2023
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
start_time <- Sys.time()

# 1.1. Specify paths to Katana/windows PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh_lonely/"
  
} else { # Manually defined for PC
  workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("SuperLearner","glmnet","ranger","arm","ltmle","parallel")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

args <- as.numeric(commandArgs(trailingOnly = TRUE))

args <- c(args[1],
          ceiling(args[1]/60),
          ceiling((args[1]-((ceiling(args[1]/60)-1)*30))/(ceiling(args[1]/60)*30)),
          args[1]-((ceiling(args[1]/30)-1)*30))

set.seed(697924)
seeds <- sample.int(100000, 180)
set.seed(seeds[args[1]])

######################################################################################
# 2. Define LTMLE parameters 
#-------------------------------------------------------------------------------------

regime_list <- expand.grid(rep(list(0:1), 6))
regimes <- array(dim = c(11412, 6, 64)) #n x num.Anodes x num.regimes
for (cnt in 1:64) {
  regimes[, , cnt] <- matrix(rep(as.matrix(regime_list[cnt,]),11412),nrow=11412,byrow=TRUE)
}

regime_sum <- cbind(regime_list$Var1,
                    regime_list$Var1+regime_list$Var2,
                    regime_list$Var1+regime_list$Var2+regime_list$Var3,
                    regime_list$Var1+regime_list$Var2+regime_list$Var3+regime_list$Var4,
                    regime_list$Var1+regime_list$Var2+regime_list$Var3+regime_list$Var4+regime_list$Var5,
                    regime_list$Var1+regime_list$Var2+regime_list$Var3+regime_list$Var4+regime_list$Var5+regime_list$Var6)

consec_sum1 <- regime_list$Var1
consec_sum2 <- ifelse(regime_list$Var2==1,consec_sum1+regime_list$Var2,0)
consec_sum3 <- ifelse(regime_list$Var3==1,consec_sum2+regime_list$Var3,0)
consec_sum4 <- ifelse(regime_list$Var4==1,consec_sum3+regime_list$Var4,0)
consec_sum5 <- ifelse(regime_list$Var5==1,consec_sum4+regime_list$Var5,0)
consec_sum6 <- ifelse(regime_list$Var6==1,consec_sum5+regime_list$Var6,0)
consec_sum <- cbind(consec_sum1,consec_sum2,consec_sum3,consec_sum4,consec_sum5,consec_sum6)
rm(consec_sum1,consec_sum2,consec_sum3,consec_sum4,consec_sum5,consec_sum6)

temp <- cbind(regime_list,regime_sum,consec_sum)

sum.measures1 <- array(dim = c(64, 1, 6))
for (cnt in 1:6) {
  sum.measures1[, , cnt] <- matrix(regime_sum[,cnt],nrow=64)
}
colnames(sum.measures1) <- c("num_waves")
msm.formula1 <- "Y ~ num_waves"

sum.measures2 <- array(dim = c(64, 1, 6))
for (cnt in 1:6) {
  sum.measures2[, , cnt] <- matrix(consec_sum[,cnt],nrow=64)
}
colnames(sum.measures2) <- c("num_consec_waves")
msm.formula2 <- "Y ~ num_consec_waves"

sum.measures <- list(sum.measures1,sum.measures2)
msm.formula <- list(msm.formula1,msm.formula2)

data <- list("pr","s1","s2")
desc <- list("num","consec")

######################################################################################
# 3. Define included variables and functional forms 
#-------------------------------------------------------------------------------------

ynodes <- c("death4","death5","death6","death7","death8","death9")
cnodes <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")
anodes <- c("lonely_binary3","lonely_binary4","lonely_binary5","lonely_binary6","lonely_binary7","lonely_binary8")
lstubs <- c("mos_long","age","mstat_2","mstat_3","ariapgp_1","ariapgp_3","employ","seifadis_2","seifadis_3","mnstrs","whobmigroup_1","whobmigroup_3","whobmigroup_4","live_alone","alcliferisk","alcepisrisk","smokst_2","smokst_3","pcsa","mcsa","gh","pf","rp","bp","vt","re","mh","sf","depression_3yr","anxiety_3yr")
lnodes <- c(paste0(lstubs,"3"),
            paste0(lstubs,"4"),
            paste0(lstubs,"5"),
            paste0(lstubs,"6"),
            paste0(lstubs,"7"))

q_base <- "Q.kplus1 ~ b_country + b_educ + b_language + b_depression_ever + b_anxiety_ever"
g_base <- "b_country + b_educ + b_language + b_depression_ever + b_anxiety_ever"
w2_conf <- "mos_long2 + mstat_22 + mstat_32 + age2 + ariapgp_12 + ariapgp_32 + employ2 + seifadis_22 + seifadis_32 + mnstrs2 + whobmigroup_12 + whobmigroup_32 + whobmigroup_42 + live_alone2 + alcliferisk2 + alcepisrisk2 + smokst_22 + smokst_32 + pcsa2 + mcsa2 + gh2 + pf2 + rp2 + bp2 + vt2 + re2 + mh2 + sf2"
w3_conf <- "mos_long3 + mstat_23 + mstat_33 + age3 + ariapgp_13 + ariapgp_33 + employ3 + seifadis_23 + seifadis_33 + mnstrs3 + whobmigroup_13 + whobmigroup_33 + whobmigroup_43 + live_alone3 + alcliferisk3 + alcepisrisk3 + smokst_23 + smokst_33 + pcsa3 + mcsa3 + gh3 + pf3 + rp3 + bp3 + vt3 + re3 + mh3 + sf3 + depression_3yr3 + anxiety_3yr3"
w4_conf <- "mos_long4 + mstat_24 + mstat_34 + age4 + ariapgp_14 + ariapgp_34 + employ4 + seifadis_24 + seifadis_34 + mnstrs4 + whobmigroup_14 + whobmigroup_34 + whobmigroup_44 + live_alone4 + alcliferisk4 + alcepisrisk4 + smokst_24 + smokst_34 + pcsa4 + mcsa4 + gh4 + pf4 + rp4 + bp4 + vt4 + re4 + mh4 + sf4 + depression_3yr4 + anxiety_3yr4"
w5_conf <- "mos_long5 + mstat_25 + mstat_35 + age5 + ariapgp_15 + ariapgp_35 + employ5 + seifadis_25 + seifadis_35 + mnstrs5 + whobmigroup_15 + whobmigroup_35 + whobmigroup_45 + live_alone5 + alcliferisk5 + alcepisrisk5 + smokst_25 + smokst_35 + pcsa5 + mcsa5 + gh5 + pf5 + rp5 + bp5 + vt5 + re5 + mh5 + sf5 + depression_3yr5 + anxiety_3yr5"
w6_conf <- "mos_long6 + mstat_26 + mstat_36 + age6 + ariapgp_16 + ariapgp_36 + employ6 + seifadis_26 + seifadis_36 + mnstrs6 + whobmigroup_16 + whobmigroup_36 + whobmigroup_46 + live_alone6 + alcliferisk6 + alcepisrisk6 + smokst_26 + smokst_36 + pcsa6 + mcsa6 + gh6 + pf6 + rp6 + bp6 + vt6 + re6 + mh6 + sf6 + depression_3yr6 + anxiety_3yr6"
w7_conf <- "mos_long7 + mstat_27 + mstat_37 + age7 + ariapgp_17 + ariapgp_37 + employ7 + seifadis_27 + seifadis_37 + mnstrs7 + whobmigroup_17 + whobmigroup_37 + whobmigroup_47 + live_alone7 + alcliferisk7 + alcepisrisk7 + smokst_27 + smokst_37 + pcsa7 + mcsa7 + gh7 + pf7 + rp7 + bp7 + vt7 + re7 + mh7 + sf7 + depression_3yr7 + anxiety_3yr7"

l3_form <- paste(q_base,w2_conf,sep=" + ")
l4_form <- paste(q_base,"lonely_binary3",w3_conf,w2_conf,sep=" + ")
l5_form <- paste(q_base,"lonely_binary4",w4_conf,"lonely_binary3",w3_conf,sep=" + ")
l6_form <- paste(q_base,"lonely_binary5",w5_conf,"lonely_binary4",w4_conf,sep=" + ")
l7_form <- paste(q_base,"lonely_binary6",w6_conf,"lonely_binary5",w5_conf,sep=" + ")

y4_form <- paste(q_base,"lonely_binary3",w2_conf,sep=" + ")
y5_form <- paste(q_base,"lonely_binary3",w2_conf,"lonely_binary4",w3_conf,sep=" + ")
y6_form <- paste(q_base,"lonely_binary3",w2_conf,"lonely_binary4",w3_conf,"lonely_binary5",w4_conf,sep=" + ")
y7_form <- paste(q_base,"lonely_binary3",w2_conf,"lonely_binary4",w3_conf,"lonely_binary5",w4_conf,"lonely_binary6",w5_conf,sep=" + ")
y8_form <- paste(q_base,"lonely_binary3",w2_conf,"lonely_binary4",w3_conf,"lonely_binary5",w4_conf,"lonely_binary6",w5_conf,"lonely_binary7",w6_conf,sep=" + ")
y9_form <- paste(q_base,"lonely_binary3",w2_conf,"lonely_binary4",w3_conf,"lonely_binary5",w4_conf,"lonely_binary6",w5_conf,"lonely_binary7",w6_conf,"lonely_binary8",w7_conf,sep=" + ")

c3_form <- paste0("censored3 ~ ",    paste(g_base,                                        w2_conf,sep=" + "))
g3_form <- paste0("lonely_binary3 ~ ",paste(g_base,                                        w2_conf,sep=" + "))
c4_form <- paste0("censored4 ~ ",    paste(g_base,                w2_conf,"lonely_binary3",w3_conf,sep=" + "))
g4_form <- paste0("lonely_binary4 ~ ",paste(g_base,                w2_conf,"lonely_binary3",w3_conf,sep=" + "))
c5_form <- paste0("censored5 ~ ",    paste(g_base,"lonely_binary3",w3_conf,"lonely_binary4",w4_conf,sep=" + "))
g5_form <- paste0("lonely_binary5 ~ ",paste(g_base,"lonely_binary3",w3_conf,"lonely_binary4",w4_conf,sep=" + "))
c6_form <- paste0("censored6 ~ ",    paste(g_base,"lonely_binary4",w4_conf,"lonely_binary5",w5_conf,sep=" + "))
g6_form <- paste0("lonely_binary6 ~ ",paste(g_base,"lonely_binary4",w4_conf,"lonely_binary5",w5_conf,sep=" + "))
c7_form <- paste0("censored7 ~ ",    paste(g_base,"lonely_binary5",w5_conf,"lonely_binary6",w6_conf,sep=" + "))
g7_form <- paste0("lonely_binary7 ~ ",paste(g_base,"lonely_binary5",w5_conf,"lonely_binary6",w6_conf,sep=" + "))
c8_form <- paste0("censored8 ~ ",    paste(g_base,"lonely_binary6",w6_conf,"lonely_binary7",w7_conf,sep=" + "))
g8_form <- paste0("lonely_binary8 ~ ",paste(g_base,"lonely_binary6",w6_conf,"lonely_binary7",w7_conf,sep=" + "))
c9_form <- paste0("censored9 ~ ",    paste(g_base,"lonely_binary7",w7_conf,"lonely_binary8",sep=" + "))

qform <- c(               mos_long3=l3_form,
                          death4=y4_form,mos_long4=l4_form,
                          death5=y5_form,mos_long5=l5_form,
                          death6=y6_form,mos_long6=l6_form,
                          death7=y7_form,mos_long7=l7_form,
                          death8=y8_form,
                          death9=y9_form)
gform <- c(censored3=c3_form,lonely_binary3=g3_form,
           censored4=c4_form,lonely_binary4=g4_form,
           censored5=c5_form,lonely_binary5=g5_form,
           censored6=c6_form,lonely_binary6=g6_form,
           censored7=c7_form,lonely_binary7=g7_form,
           censored8=c8_form,lonely_binary8=g8_form,
           censored9=c9_form)

######################################################################################
# 4. Define SuperLearner Libraries
#-------------------------------------------------------------------------------------

ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128))

SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
              g=c("SL.mean","SL.glm","SL.gam",ranger_128$names))

######################################################################################
# 5. Load data
#-------------------------------------------------------------------------------------

analysis_data <- readRDS(paste0(workdir,"Data/all cause analysis - ",data[[args[2]]],".rds"))

analysis_data <- analysis_data[[args[4]]]

######################################################################################
# 6. Run LTMLE models 
#-------------------------------------------------------------------------------------

model_fit_msm <- ltmleMSM(analysis_data,
                          Cnodes = cnodes,
                          Anodes = anodes,
                          Lnodes = lnodes,
                          Ynodes = ynodes,
                          final.Ynodes = ynodes,
                          Qform = qform,
                          gform = gform,
                          regimes = regimes,
                          summary.measures = sum.measures[[args[3]]],
                          working.msm = msm.formula[[args[3]]],
                          survivalOutcome = TRUE,
                          observation.weights = analysis_data$wtarea,
                          SL.library = SLlib)

summary(model_fit_msm)

cov.mat_ic <- var(model_fit_msm$IC)
cov.mat_tmle <- model_fit_msm$variance.estimate

ic_temp <- as.vector(cov.mat_ic)
tmle_temp <- as.vector(cov.mat_tmle)

cov.mat_max <- ic_temp
for (i in seq_along(ic_temp)) {
  std.dev.diff <- abs(tmle_temp[[i]]) - abs(ic_temp[[i]])
  if (!is.na(std.dev.diff) && (std.dev.diff > 0)) { #can be NA if all Y_d are near 0 or 1
    cov.mat_max[[i]] <- tmle_temp[[i]]
  }
}
cov.mat_max <- matrix(cov.mat_max,nrow=2)
colnames(cov.mat_max) <- c("S1","S2")
rownames(cov.mat_max) <- c("S1","S2")

fin_sum <- list(summary(model_fit_msm),cov.mat_ic,cov.mat_tmle,cov.mat_max)

res <- c(c(plogis(summary(model_fit_msm)$cmat[1,1]),
           plogis(summary(model_fit_msm)$cmat[1,1]+1*summary(model_fit_msm)$cmat[2,1]),
           plogis(summary(model_fit_msm)$cmat[1,1]+2*summary(model_fit_msm)$cmat[2,1]),
           plogis(summary(model_fit_msm)$cmat[1,1]+3*summary(model_fit_msm)$cmat[2,1]),
           plogis(summary(model_fit_msm)$cmat[1,1]+4*summary(model_fit_msm)$cmat[2,1]),
           plogis(summary(model_fit_msm)$cmat[1,1]+5*summary(model_fit_msm)$cmat[2,1]),
           plogis(summary(model_fit_msm)$cmat[1,1]+6*summary(model_fit_msm)$cmat[2,1])))

gradient_mn <- list(c(res[1]*(1-res[1]), 0),
                    c(res[2]*(1-res[2]), res[2]*(1-res[2])),
                    c(res[3]*(1-res[3]), res[3]*(1-res[3])),
                    c(res[4]*(1-res[4]), res[4]*(1-res[4])),
                    c(res[5]*(1-res[5]), res[5]*(1-res[5])),
                    c(res[6]*(1-res[6]), res[6]*(1-res[6])),
                    c(res[7]*(1-res[7]), res[7]*(1-res[7])))

v_ic_mn <- do.call(rbind,lapply(gradient_mn, function(z, model_fit_msm) {
  v <- t(z) %*% cov.mat_ic %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
}, model_fit_msm= model_fit_msm))

v_tmle_mn <- do.call(rbind,lapply(gradient_mn, function(z,model_fit_msm) {
  v <- t(z) %*% cov.mat_tmle %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm =model_fit_msm))

v_max_mn <- do.call(rbind,lapply(gradient_mn, function(z,model_fit_msm) {
  v <- t(z) %*% cov.mat_max %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm =model_fit_msm))

fin_res_mn <- matrix(cbind(res,v_ic_mn,v_tmle_mn,v_max_mn),nrow=7)

gradient_rr <- list(c(res[1]-res[2], 1-res[2]),
                    c(res[1]-res[3], 1-res[3]),
                    c(res[1]-res[4], 1-res[4]),
                    c(res[1]-res[5], 1-res[5]),
                    c(res[1]-res[6], 1-res[6]),
                    c(res[1]-res[7], 1-res[7]))

v_ic_rr <- do.call(rbind,lapply(gradient_rr, function(z, model_fit_msm) {
  v <- t(z) %*% cov.mat_ic %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm=model_fit_msm))

v_tmle_rr <- do.call(rbind,lapply(gradient_rr, function(z,model_fit_msm) {
  v <- t(z) %*% cov.mat_tmle %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm=model_fit_msm))

v_max_rr <- do.call(rbind,lapply(gradient_rr, function(z,model_fit_msm) {
  v <- t(z) %*% cov.mat_max %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm=model_fit_msm))

rr <- c(res[2]/res[1],
        res[3]/res[1],
        res[4]/res[1],
        res[5]/res[1],
        res[6]/res[1],
        res[7]/res[1])

fin_res_rr <- matrix(cbind(rr,v_ic_rr,v_tmle_rr,v_max_rr),nrow=6)

gradient_rdiff <- list(c(res[2]*(1-res[2]) - res[1]*(1-res[1]), res[2]*(1-res[2])),
                       c(res[3]*(1-res[3]) - res[1]*(1-res[1]), res[3]*(1-res[3])),
                       c(res[4]*(1-res[4]) - res[1]*(1-res[1]), res[4]*(1-res[4])),
                       c(res[5]*(1-res[5]) - res[1]*(1-res[1]), res[5]*(1-res[5])),
                       c(res[6]*(1-res[6]) - res[1]*(1-res[1]), res[6]*(1-res[6])),
                       c(res[7]*(1-res[7]) - res[1]*(1-res[1]), res[7]*(1-res[7])))

v_ic_rdiff <- do.call(rbind,lapply(gradient_rdiff, function(z, model_fit_msm) {
  v <- t(z) %*% cov.mat_ic %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm=model_fit_msm))

v_tmle_rdiff <- do.call(rbind,lapply(gradient_rdiff, function(z,model_fit_msm) {
  v <- t(z) %*% cov.mat_tmle %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm=model_fit_msm))

v_max_rdiff <- do.call(rbind,lapply(gradient_rdiff, function(z,model_fit_msm) {
  v <- t(z) %*% cov.mat_max %*% z
  std.dev <- sqrt(v[1, 1] / dim(model_fit_msm$IC)[1])
},model_fit_msm=model_fit_msm))

rdiff <- c(res[2]-res[1],
           res[3]-res[1],
           res[4]-res[1],
           res[5]-res[1],
           res[6]-res[1],
           res[7]-res[1])

fin_res_rdiff <- matrix(cbind(rdiff,v_ic_rdiff,v_tmle_rdiff,v_max_rdiff),nrow=6)

######################################################################################
# 5. Save output
#-------------------------------------------------------------------------------------

saveRDS(fin_sum,file=paste0(workdir,"Results/ltmle-",data[[args[2]]],"-sum-",desc[[args[3]]],"-",args[4],".rds"))
saveRDS(fin_res_mn,file=paste0(workdir,"Results/ltmle-",data[[args[2]]],"-mn-",desc[[args[3]]],"-",args[4],".rds"))
saveRDS(fin_res_rr,file=paste0(workdir,"Results/ltmle-",data[[args[2]]],"-rr-",desc[[args[3]]],"-",args[4],".rds"))
saveRDS(fin_res_rdiff,file=paste0(workdir,"Results/ltmle-",data[[args[2]]],"-rd-",desc[[args[3]]],"-",args[4],".rds"))

end_time <- Sys.time()
end_time - start_time
