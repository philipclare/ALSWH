######################################################################################
##   
## Effects of physical activity on health-related quality of life
## LTMLE MSM Analysis of dynamic regimes of physical activity on health outcomes
## Date: 20 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

if (Sys.info()["sysname"]=="linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh/"
} else {
  workdir <- "Y:/PRJ-prc_alswh/Physical activity trajectories/"
}

libs <- c("SuperLearner","glmnet","ranger","arm","ltmle","parallel")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

args <- as.numeric(commandArgs(trailingOnly = TRUE))
seeds <- c("395702","663452","941566","907543","237738","561682","266872","162583","248138","984238")
set.seed(seeds[args[1]])

######################################################################################
# 2. Load data 
#-------------------------------------------------------------------------------------

load(paste0(workdir,"Data/primary analysis data - wide form.RData"))
ltmle_data <- imp_primary
rm(imp_primary)

######################################################################################
# 3. Define LTMLE parameters 
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
# 4. Define included variables and functional forms 
#-------------------------------------------------------------------------------------

cnodes <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")
anodes <- c("activity_bin3","activity_bin4","activity_bin5","activity_bin6","activity_bin7","activity_bin8")
lnodes <- c("marital2","age2","ariapgp2","employ2","seifadis2","live_u182","live_o182","cesd102","mnstrs2","whobmigroup2","vegetables2","fruit2","alcliferisk2","alcepisrisk2","smokst2",
            "marital3","age3","ariapgp3","employ3","seifadis3","live_u183","live_o183","cancer_3yr3","arthritis_3yr3","depression_3yr3","anxiety_3yr3","cesd103","mnstrs3","whobmigroup3","vegetables3","fruit3","alcliferisk3","alcepisrisk3","smokst3",
            "marital4","age4","ariapgp4","employ4","seifadis4","live_u184","live_o184","cancer_3yr4","arthritis_3yr4","depression_3yr4","anxiety_3yr4","cesd104","mnstrs4","whobmigroup4","vegetables4","fruit4","alcliferisk4","alcepisrisk4","smokst4",
            "marital5","age5","ariapgp5","employ5","seifadis5","live_u185","live_o185","cancer_3yr5","arthritis_3yr5","depression_3yr5","anxiety_3yr5","cesd105","mnstrs5","whobmigroup5","vegetables5","fruit5","alcliferisk5","alcepisrisk5","smokst5",
            "marital6","age6","ariapgp6","employ6","seifadis6","live_u186","live_o186","cancer_3yr6","arthritis_3yr6","depression_3yr6","anxiety_3yr6","cesd106","mnstrs6","whobmigroup6","vegetables6","fruit6","alcliferisk6","alcepisrisk6","smokst6",
            "marital7","age7","ariapgp7","employ7","seifadis7","live_u187","live_o187","cancer_3yr7","arthritis_3yr7","depression_3yr7","anxiety_3yr7","cesd107","mnstrs7","whobmigroup7","vegetables7","fruit7","alcliferisk7","alcepisrisk7","smokst7")

q_base <- "Q.kplus1 ~ b_wtarea + b_pcsa + b_mcsa + b_gh + b_pf + b_re + b_rp + b_cobcat + b_bp + b_educ + b_mh + b_vt + b_sf + b_cancer_ever + b_depression_ever + b_anxiety_ever"
g_base <- "b_pcsa + b_mcsa + b_gh + b_pf + b_re + b_rp + b_cobcat + b_bp + b_educ + b_mh + b_vt + b_sf + b_cancer_ever + b_depression_ever + b_anxiety_ever"
w2_conf <- "marital2 + age2 + ariapgp2 + employ2 + seifadis2 + live_u182 + live_o182 + cesd102 + mnstrs2 + whobmigroup2 + vegetables2 + fruit2 + alcliferisk2 + alcepisrisk2 + smokst2"
w3_conf <- "marital3 + age3 + ariapgp3 + employ3 + seifadis3 + live_u183 + live_o183 + cancer_3yr3 + arthritis_3yr3 + depression_3yr3 + anxiety_3yr3 + cesd103 + mnstrs3 + whobmigroup3 + vegetables3 + fruit3 + alcliferisk3 + alcepisrisk3 + smokst3"
w4_conf <- "marital4 + age4 + ariapgp4 + employ4 + seifadis4 + live_u184 + live_o184 + cancer_3yr4 + arthritis_3yr4 + depression_3yr4 + anxiety_3yr4 + cesd104 + mnstrs4 + whobmigroup4 + vegetables4 + fruit4 + alcliferisk4 + alcepisrisk4 + smokst4"
w5_conf <- "marital5 + age5 + ariapgp5 + employ5 + seifadis5 + live_u185 + live_o185 + cancer_3yr5 + arthritis_3yr5 + depression_3yr5 + anxiety_3yr5 + cesd105 + mnstrs5 + whobmigroup5 + vegetables5 + fruit5 + alcliferisk5 + alcepisrisk5 + smokst5"
w6_conf <- "marital6 + age6 + ariapgp6 + employ6 + seifadis6 + live_u186 + live_o186 + cancer_3yr6 + arthritis_3yr6 + depression_3yr6 + anxiety_3yr6 + cesd106 + mnstrs6 + whobmigroup6 + vegetables6 + fruit6 + alcliferisk6 + alcepisrisk6 + smokst6"
w7_conf <- "marital7 + age7 + ariapgp7 + employ7 + seifadis7 + live_u187 + live_o187 + cancer_3yr7 + arthritis_3yr7 + depression_3yr7 + anxiety_3yr7 + cesd107 + mnstrs7 + whobmigroup7 + vegetables7 + fruit7 + alcliferisk7 + alcepisrisk7 + smokst7"

q3_form <- paste(q_base,                        w2_conf,sep=" + ")
q4_form <- paste(q_base,w2_conf,"activity_bin3",w3_conf,sep=" + ")
q5_form <- paste(q_base,w3_conf,"activity_bin4",w4_conf,sep=" + ")
q6_form <- paste(q_base,w4_conf,"activity_bin5",w5_conf,sep=" + ")
q7_form <- paste(q_base,w5_conf,"activity_bin6",w6_conf,sep=" + ")
q9_form <- paste(q_base,w2_conf,"activity_bin3",w3_conf,"activity_bin4",
                 w4_conf,"activity_bin5",w5_conf,"activity_bin6",
                 w6_conf,"activity_bin7",w7_conf,"activity_bin8",sep=" + ")

c3_form <- paste("censored3 ~ b_wtarea",g_base,                                        w2_conf,sep=" + ")
g3_form <- paste("activity_bin3 ~ b_wtarea",g_base,                                        w2_conf,sep=" + ")
c4_form <- paste("censored4 ~ b_wtarea",g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + ")
g4_form <- paste("activity_bin4 ~ b_wtarea",g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + ")
c5_form <- paste("censored5 ~ b_wtarea",g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + ")
g5_form <- paste("activity_bin5 ~ b_wtarea",g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + ")
c6_form <- paste("censored6 ~ b_wtarea",g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + ")
g6_form <- paste("activity_bin6 ~ b_wtarea",g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + ")
c7_form <- paste("censored7 ~ b_wtarea",g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + ")
g7_form <- paste("activity_bin7 ~ b_wtarea",g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + ")
c8_form <- paste("censored8 ~ b_wtarea",g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + ")
g8_form <- paste("activity_bin8 ~ b_wtarea",g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + ")
c9_form <- paste("censored9 ~ b_wtarea",g_base,"activity_bin7",w7_conf,"activity_bin8",sep=" + ")

qform <- c(marital3=q3_form,
           marital4=q4_form,
           marital5=q5_form,
           marital6=q6_form,
           marital7=q7_form,
           outcome=q9_form)
gform <- c(censored3=c3_form,activity_bin3=g3_form,
           censored4=c4_form,activity_bin4=g4_form,
           censored5=c5_form,activity_bin5=g5_form,
           censored6=c6_form,activity_bin6=g6_form,
           censored7=c7_form,activity_bin7=g7_form,
           censored8=c8_form,activity_bin8=g8_form,
           censored9=c9_form)

######################################################################################
# 5. Define SuperLearner Libraries
#-------------------------------------------------------------------------------------

ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128))

SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
              g=c("SL.mean","SL.glm","SL.gam"))
SLlib2 <- list(Q=c("SL.mean","SL.glm","SL.gam"),
               g=c("SL.mean","SL.glm","SL.gam",ranger_128$names))

######################################################################################
# 6. Setup parallel processing environment
#-------------------------------------------------------------------------------------

numcores <- 4
cl <- makeCluster(numcores)
parallel::clusterEvalQ(cl, workdir <- "Y:/PRJ-prc_alswh/")
parallel::clusterEvalQ(cl, libs <- c("SuperLearner","glmnet","ranger","arm","ltmle"))
parallel::clusterEvalQ(cl, lapply(libs, library, character.only = TRUE))
parallel::clusterEvalQ(cl, ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128)))
parallel::clusterExport(cl, list("ltmle_data","cnodes","anodes","lnodes","qform","gform",
                                 "theta.set","sum.measures","msm.formula",
                                 "SLlib","SLlib2"))

######################################################################################
# 7. Run LTMLE models 
#-------------------------------------------------------------------------------------

outcomes <- c("pcsa9","mcsa9","pf9","rp9","bp9","gh9","vt9","sf9","re9","mh9")

outcome <- outcomes[args[1]]

fit <- parLapply(cl,ltmle_data[1:8],function (x,outcome=y) {
  
  x <- x[,c(2:140,which(colnames(x)==outcome))]
  
  ynodes <- outcome
  names(qform)[6] <- outcome
  
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
  
  model_fit_msm <- ltmleMSM(x,
                            Cnodes = cnodes,
                            Anodes = anodes,
                            Lnodes = lnodes,
                            Ynodes = ynodes,
                            Qform = qform,
                            gform = gform,
                            regimes = regimes,
                            summary.measures = sum.measures,
                            working.msm = msm.formula,
                            Yrange = c(0,100),
                            SL.library = SLlib2)
  
},
outcome=outcome)

######################################################################################
# 8. Save output
#-------------------------------------------------------------------------------------

save(fit,file=paste0(workdir,"Results/primary-results-",outcome,".RData"))

