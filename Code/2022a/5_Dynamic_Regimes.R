######################################################################################
##   
## Effects of physical activity on health-related quality of life
## LTMLE Analysis of dynamic regimes of physical activity on health outcomes
## Date: 31 August 2022
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/"

libs <- c("SuperLearner","glmnet","ranger","arm","ltmle")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2. Load data 
#-----------------------------------------------------------------------------

load(paste0(workdir,"Physical activity trajectories/Data/analysis data - wide form.RData"))
ltmle_data <- imp[[1]]

##############################################################################
# 4. Define LTMLE parameters 
#-----------------------------------------------------------------------------

theta.set <- c(50,55,60,65)
regimes <- array(dim = c(dim(ltmle_data)[1], 6, 4)) #n x num.Anodes x num.regimes

summary.measures <- array(theta.set, dim = c(4, 3, 1))
summary.measures[, , 1] <- matrix(c(0,1,0,0,
                                        0,0,1,0,
                                        0,0,0,1),
                                      nrow=4)
colnames(summary.measures) <- c("start_age_55","start_age_60","start_age_65")
age_cols <- which( colnames(ltmle_data)=="age3" | colnames(ltmle_data)== "age4" | colnames(ltmle_data)== "age5" | colnames(ltmle_data)== "age6" | colnames(ltmle_data)== "age7" | colnames(ltmle_data)== "age8")
cnt <- 0
for (theta.index in 1:4) {
  cnt <- cnt + 1
  regimes[, 1, cnt] <- ltmle_data$age2 > theta.set[theta.index]
  regimes[, 2, cnt] <- ltmle_data$age3 > theta.set[theta.index]
  regimes[, 3, cnt] <- ltmle_data$age4 > theta.set[theta.index]
  regimes[, 4, cnt] <- ltmle_data$age5 > theta.set[theta.index]
  regimes[, 5, cnt] <- ltmle_data$age6 > theta.set[theta.index]
  regimes[, 6, cnt] <- ltmle_data$age7 > theta.set[theta.index]
}
working.msm <- "Y ~ start_age_55 + start_age_60 + start_age_65"

# ltmle_data$censored3 <- BinaryToCensoring(is.uncensored=ltmle_data$censored3)
# ltmle_data$censored4 <- BinaryToCensoring(is.uncensored=ltmle_data$censored4)
# ltmle_data$censored5 <- BinaryToCensoring(is.uncensored=ltmle_data$censored5)
# ltmle_data$censored6 <- BinaryToCensoring(is.uncensored=ltmle_data$censored6)
# ltmle_data$censored7 <- BinaryToCensoring(is.uncensored=ltmle_data$censored7)
# ltmle_data$censored8 <- BinaryToCensoring(is.uncensored=ltmle_data$censored8)
# ltmle_data$censored9 <- BinaryToCensoring(is.uncensored=ltmle_data$censored9)

# cnodes <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")
anodes <- c("activity_bin3","activity_bin4","activity_bin5","activity_bin6","activity_bin7","activity_bin8")
lnodes <- c("marital2","age2","ariapgp2","employ2","seifadis2","live_u182","live_o182","cancer_3yr2","arthritis_3yr2","depression_3yr2","anxiety_3yr2","cesd102","mnstrs2","whobmigroup2","vegetables2","fruit2","alcliferisk2","alcepisrisk2","smokst2",
            "marital3","age3","ariapgp3","employ3","seifadis3","live_u183","live_o183","cancer_3yr3","arthritis_3yr3","depression_3yr3","anxiety_3yr3","cesd103","mnstrs3","whobmigroup3","vegetables3","fruit3","alcliferisk3","alcepisrisk3","smokst3",
            "marital4","age4","ariapgp4","employ4","seifadis4","live_u184","live_o184","cancer_3yr4","arthritis_3yr4","depression_3yr4","anxiety_3yr4","cesd104","mnstrs4","whobmigroup4","vegetables4","fruit4","alcliferisk4","alcepisrisk4","smokst4",
            "marital5","age5","ariapgp5","employ5","seifadis5","live_u185","live_o185","cancer_3yr5","arthritis_3yr5","depression_3yr5","anxiety_3yr5","cesd105","mnstrs5","whobmigroup5","vegetables5","fruit5","alcliferisk5","alcepisrisk5","smokst5",
            "marital6","age6","ariapgp6","employ6","seifadis6","live_u186","live_o186","cancer_3yr6","arthritis_3yr6","depression_3yr6","anxiety_3yr6","cesd106","mnstrs6","whobmigroup6","vegetables6","fruit6","alcliferisk6","alcepisrisk6","smokst6",
            "marital7","age7","ariapgp7","employ7","seifadis7","live_u187","live_o187","cancer_3yr7","arthritis_3yr7","depression_3yr7","anxiety_3yr7","cesd107","mnstrs7","whobmigroup7","vegetables7","fruit7","alcliferisk7","alcepisrisk7","smokst7")
ynodes <- c("pcsa9")

ltmle_data[,c("marital2","marital3","marital4","marital5","marital6","marital7")] <- lapply(ltmle_data[,c("marital2","marital3","marital4","marital5","marital6","marital7")], factor)
ltmle_data[,c("ariapgp2","ariapgp3","ariapgp4","ariapgp5","ariapgp6","ariapgp7")] <- lapply(ltmle_data[,c("ariapgp2","ariapgp3","ariapgp4","ariapgp5","ariapgp6","ariapgp7")], factor)
ltmle_data[,c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")] <- lapply(ltmle_data[,c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")], factor)
ltmle_data[,c("whobmigroup2","whobmigroup3","whobmigroup4","whobmigroup5","whobmigroup6","whobmigroup7")] <- lapply(ltmle_data[,c("whobmigroup2","whobmigroup3","whobmigroup4","whobmigroup5","whobmigroup6","whobmigroup7")], factor)
ltmle_data[,c("smokst2","smokst3","smokst4","smokst5","smokst6","smokst7")] <- lapply(ltmle_data[,c("smokst2","smokst3","smokst4","smokst5","smokst6","smokst7")], factor)

treat <- matrix(1, nrow = 11477, ncol = 6)
dyn_trt_50 <- cbind(ltmle_data$age2 > 50, ltmle_data$age3 > 50, ltmle_data$age4 > 50,
                    ltmle_data$age5 > 50, ltmle_data$age6 > 50, ltmle_data$age7 > 50)
dyn_trt_55 <- cbind(ltmle_data$age2 > 55, ltmle_data$age3 > 55, ltmle_data$age4 > 55,
                    ltmle_data$age5 > 55, ltmle_data$age6 > 55, ltmle_data$age7 > 55)
dyn_trt_60 <- cbind(ltmle_data$age2 > 60, ltmle_data$age3 > 60, ltmle_data$age4 > 60,
                    ltmle_data$age5 > 60, ltmle_data$age6 > 60, ltmle_data$age7 > 60)
dyn_trt_65 <- cbind(ltmle_data$age2 > 65, ltmle_data$age3 > 65, ltmle_data$age4 > 65,
                    ltmle_data$age5 > 65, ltmle_data$age6 > 65, ltmle_data$age7 > 65)
control <- matrix(0, nrow = 11477, ncol = 6)

##############################################################################
# 5. Define SuperLearner Libraries
#-----------------------------------------------------------------------------

ranger_128 <- create.Learner("SL.ranger", tune=list(mtry = c(2,3,4)), params = list(num.trees = 128))
ranger_256 <- create.Learner("SL.ranger", tune=list(mtry = c(2,3,4)), params = list(num.trees = 256))

# SLlib <- list(Q=c("SL.mean","SL.glm","SL.glm.interaction","SL.gam"),
#               g=c("SL.mean","SL.glm","SL.glm.interaction","SL.gam",ranger_128$names))
SLlib <- list(Q=c("SL.mean","SL.glm","SL.glm.interaction"),
              g=c("SL.mean","SL.glm","SL.glm.interaction"))

##############################################################################
# 6. Run LTMLE models 
#-----------------------------------------------------------------------------

model_fit_msm <- ltmleMSM(ltmle_data[,c(2:135)],
                      Anodes=anodes, Lnodes=lnodes, Ynodes=ynodes,
                      regimes=regimes,
                      summary.measures=summary.measures,
                      working.msm=working.msm,
                      survivalOutcome = FALSE)
summary(model_fit_msm)

model_fit <- ltmle(ltmle_data[,c(1:129)],
                          Cnodes=cnodes, Anodes=anodes, Lnodes=lnodes, Ynodes=ynodes,
                          abar=list(c(1,1,1,1,1,1),c(0,0,0,0,0,0)))
summary(model_fit)

model_fit_dyn <- ltmle(ltmle_data[,c(1:129)],
                   Cnodes=cnodes, Anodes=anodes, Lnodes=lnodes, Ynodes=ynodes,
                   abar=list(treat,control))
summary(model_fit_dyn)

model_fit_all <- ltmle(ltmle_data[,c(2:135)],
                       Anodes = anodes, Lnodes = lnodes, Ynodes = ynodes, 
                       abar = treat,
                       Yrange=c(0,100))
model_fit_50 <- ltmle(ltmle_data[,c(2:135)],
                      Anodes = anodes, Lnodes = lnodes, Ynodes = ynodes,  
                      abar=dyn_trt_50,
                      Yrange=c(0,100))
model_fit_55 <- ltmle(ltmle_data[,c(2:135)],
                      Anodes = anodes, Lnodes = lnodes, Ynodes = ynodes, 
                      abar=dyn_trt_55,
                      Yrange=c(0,100))
model_fit_60 <- ltmle(ltmle_data[,c(2:135)],
                      Anodes = anodes, Lnodes = lnodes, Ynodes = ynodes,  
                      abar=dyn_trt_60,
                      Yrange=c(0,100))
model_fit_65 <- ltmle(ltmle_data[,c(2:135)],
                      Anodes = anodes, Lnodes = lnodes, Ynodes = ynodes,  
                      abar=dyn_trt_65,
                      Yrange=c(0,100))
model_fit_none <- ltmle(ltmle_data[,c(2:135)],
                        Anodes = anodes, Lnodes = lnodes, Ynodes = ynodes,
                        abar=control,
                        Yrange=c(0,100))

summary(model_fit_all)
summary(model_fit_50)
summary(model_fit_55)
summary(model_fit_60)
summary(model_fit_65)
summary(model_fit_none)