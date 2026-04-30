######################################################################################
##   
## Effects of physical activity on incident obesity
## Primary LTMLE MSM Analysis
## Date: 4 June 2025
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://osf.io/fyszg
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
start_time <- Sys.time()

# 1.1. Specify paths to Katana/windows/Mac paths based on system
if (Sys.info()[['sysname']]=="Linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/Obesity/"
} else if (Sys.info()[['sysname']]=="Windows") {
  workdir <- "Y:/PRJ-prc_alswh/Paper 3 - Obesity/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/Paper 3 - Obesity/" # MAC
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("arm","dplyr","fastDummies","gam","ltmle","parallel","ranger","SuperLearner")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

# 1.3. Set argument either for testing or passed by scheduler on HPC
if (Sys.info()[['sysname']]=="Linux") {
  args <- as.numeric(commandArgs(trailingOnly = TRUE))
} else if (Sys.info()[['sysname']]=="Windows" | Sys.info()[['sysname']]=="Darwin") {
  args <- 1 # for testing on local computer
}

# 1.4. Set seed to ensure replicability
set.seed(697924)

nonlinear <- c("Y ~ cum_exp",
               "Y ~ cum_exp + I(cum_exp^2)",
               "Y ~ cum_exp + I(cum_exp^3)",
               "Y ~ cum_exp + I(cum_exp^2) + I(cum_exp^3)")

######################################################################################
# 2. Load data
#-------------------------------------------------------------------------------------

analysis_data <- readRDS(paste0(workdir,"Data/posthoc bmi analysis data - 20260304.rds"))

analysis_data <- analysis_data[[1]]

######################################################################################
# 3. Define LTMLE parameters 
#-------------------------------------------------------------------------------------

regime_set <- expand.grid(rep(list(0:1), 7))
colnames(regime_set) <- c("V1","V2","V3","V4","V5","V6","V7")

n <- 4779

regimes <- array(dim = c(n, 7, 128)) #n x num.Anodes x num.regimes
for (cnt in 1:128) {
  regimes[, , cnt] <- matrix(rep(as.matrix(regime_set[cnt,]),n),nrow=n,byrow=TRUE)
}

regime_sum <- data.frame(rbind(cbind(regime_set$V1,
                                     regime_set$V1+regime_set$V2,
                                     regime_set$V1+regime_set$V2+regime_set$V3,
                                     regime_set$V1+regime_set$V2+regime_set$V3+regime_set$V4,
                                     regime_set$V1+regime_set$V2+regime_set$V3+regime_set$V4+regime_set$V5,
                                     regime_set$V1+regime_set$V2+regime_set$V3+regime_set$V4+regime_set$V5+regime_set$V6,
                                     regime_set$V1+regime_set$V2+regime_set$V3+regime_set$V4+regime_set$V5+regime_set$V6+regime_set$V7)))

sum.measures <- array(dim = c(128, 1, 1)) # num.regimes x num.measures x num.Ynodes
sum.measures[, 1, 1] <- matrix(regime_sum[,7],nrow=128)

colnames(sum.measures) <- c("cum_exp")

######################################################################################
# 4. Define included variables and functional forms 
#-------------------------------------------------------------------------------------

ynodes <- c("bmi10")
cnodes <- c("death3","death4","death5","death6","death7","death8","death9","death10")
anodes <- c("activity_bin3","activity_bin4","activity_bin5","activity_bin6","activity_bin7","activity_bin8","activity_bin9")
lstubs0 <- c("marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ","live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5","cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3","menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","finfinc","pcsa","mcsa","bmi")
lstubs1 <- c("marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ","live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5","heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3","menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","finfinc","pcsa","mcsa","bmi")
lstubs2 <- c("marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ","live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5","heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3","menopause_2","menopause_3","hrt","sleep_prob","finfinc","pcsa","mcsa","bmi")
lnodes <- c(paste0(lstubs1,"3"),
            paste0(lstubs2,"4"),
            paste0(lstubs2,"5"),
            paste0(lstubs2,"6"),
            paste0(lstubs2,"7"),
            paste0(lstubs2,"8"))

q_base <- "Q.kplus1 ~ b_bmi + b_heartdis_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever + b_metmin + b_cobcat + b_pcsa + b_mcsa + b_educ_2 + b_educ_3 + "
g_base <- "b_bmi + b_heartdis_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever + b_metmin + b_cobcat + b_pcsa + b_mcsa + b_educ_2 + b_educ_3 + "
w2_conf <- paste(paste0(lstubs0,"2"),collapse=" + ")
w3_conf <- paste(paste0(lstubs1,"3"),collapse=" + ")
w4_conf <- paste(paste0(lstubs2,"4"),collapse=" + ")
w5_conf <- paste(paste0(lstubs2,"5"),collapse=" + ")
w6_conf <- paste(paste0(lstubs2,"6"),collapse=" + ")
w7_conf <- paste(paste0(lstubs2,"7"),collapse=" + ")
w8_conf <- paste(paste0(lstubs2,"8"),collapse=" + ")

l3_form <- paste(q_base,w2_conf,sep=" + ")
l4_form <- paste(q_base,"activity_bin3",w3_conf,w2_conf,sep=" + ")
l5_form <- paste(q_base,"activity_bin4",w4_conf,"activity_bin3",w3_conf,sep=" + ")
l6_form <- paste(q_base,"activity_bin5",w5_conf,"activity_bin4",w4_conf,sep=" + ")
l7_form <- paste(q_base,"activity_bin6",w6_conf,"activity_bin5",w5_conf,sep=" + ")
l8_form <- paste(q_base,"activity_bin7",w7_conf,"activity_bin6",w6_conf,sep=" + ")

y10_form <- paste(q_base,"activity_bin3",w2_conf,"activity_bin4",w3_conf,"activity_bin5",w4_conf,"activity_bin6",w5_conf,"activity_bin7",w6_conf,"activity_bin8",w7_conf,"activity_bin9",w8_conf,sep=" + ")

c3_form <- paste0("death3 ~ ",       paste(g_base,                                        w2_conf,sep=" + "))
g3_form <- paste0("activity_bin3 ~ ",paste(g_base,                                        w2_conf,sep=" + "))
c4_form <- paste0("death4 ~ ",       paste(g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + "))
g4_form <- paste0("activity_bin4 ~ ",paste(g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + "))
c5_form <- paste0("death5 ~ ",       paste(g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + "))
g5_form <- paste0("activity_bin5 ~ ",paste(g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + "))
c6_form <- paste0("death6 ~ ",       paste(g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + "))
g6_form <- paste0("activity_bin6 ~ ",paste(g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + "))
c7_form <- paste0("death7 ~ ",       paste(g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + "))
g7_form <- paste0("activity_bin7 ~ ",paste(g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + "))
c8_form <- paste0("death8 ~ ",       paste(g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + "))
g8_form <- paste0("activity_bin8 ~ ",paste(g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + "))
c9_form <- paste0("death9 ~ ",       paste(g_base,"activity_bin7",w7_conf,"activity_bin8",w8_conf,sep=" + "))
g9_form <- paste0("activity_bin9 ~ ",paste(g_base,"activity_bin7",w6_conf,"activity_bin8",w8_conf,sep=" + "))
c10_form <- paste0("death10 ~ ",     paste(g_base,"activity_bin8",w7_conf,"activity_bin9",sep=" + "))

qform <- c(marital_23=l3_form,
           marital_24=l4_form,
           marital_25=l5_form,
           marital_26=l6_form,
           marital_27=l7_form,
           marital_28=l8_form,
           bmi10=y10_form)
gform <- c(death3=c3_form,activity_bin3=g3_form,
           death4=c4_form,activity_bin4=g4_form,
           death5=c5_form,activity_bin5=g5_form,
           death6=c6_form,activity_bin6=g6_form,
           death7=c7_form,activity_bin7=g7_form,
           death8=c8_form,activity_bin8=g8_form,
           death9=c9_form,activity_bin9=g9_form,
           death10=c10_form)

######################################################################################
# 5. Define SuperLearner Libraries
#-------------------------------------------------------------------------------------

ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128))

SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
              g=c("SL.mean","SL.glm","SL.gam","SL.ranger_1"))

######################################################################################
# 6. Run LTMLE models 
#-------------------------------------------------------------------------------------

cl <- makeCluster(4)
parallel::clusterEvalQ(cl, library("ltmle"))
parallel::clusterEvalQ(cl, library("ranger"))
parallel::clusterEvalQ(cl, library("SuperLearner"))
parallel::clusterEvalQ(cl, library("gam"))
parallel::clusterEvalQ(cl, ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128)))
parallel::clusterExport(cl, c("analysis_data","cnodes","anodes","lnodes","ynodes","qform","gform","regimes","sum.measures","ranger_128","SLlib"))

linearity <- do.call(rbind,parLapply(cl,nonlinear, function (x) {
  
  msm.formula <- x
  
  nterms <- length(attr(terms(formula(msm.formula)),"term.labels"))+1
  test.formula <- paste0("Y ~ ",paste("S",seq(1,nterms),sep="", collapse=" + "))
  
  model_fit_msm <- ltmleMSM(analysis_data[,-1],
                            Cnodes = cnodes,
                            Anodes = anodes,
                            Lnodes = lnodes,
                            Ynodes = ynodes,
                            final.Ynodes = ynodes,
                            Qform = qform,
                            gform = gform,
                            regimes = regimes,
                            summary.measures = sum.measures,
                            working.msm = msm.formula,
                            survivalOutcome = FALSE,
                            SL.library = SLlib,
                            variance.method = "ic",
                            observation.weights = analysis_data$b_wtarea)
  
  test_data <- data.frame(model_fit_msm$msm$data)
  test_data$weights <- model_fit_msm$msm$prior.weights
  
  test <- glm(data=test_data,
              formula=test.formula,
              family=binomial,
              weights=weights)
  
  res <- c(x,AIC(test),BIC(test))
  
}))

saveRDS(linearity,file=paste0(workdir,"Results/fit/bmi_linearity.rds"))


