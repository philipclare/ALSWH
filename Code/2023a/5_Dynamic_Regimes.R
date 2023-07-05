######################################################################################
##   
## Effects of physical activity on health-related quality of life
## LTMLE MSM Analysis of dynamic regimes of physical activity on health outcomes
## Date: 30 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

.libPaths("/home/z3312911/RPackages")
workdir <- "/home/z3312911/alswh/"

libs <- c("SuperLearner","glmnet","ranger","arm","ltmle","parallel")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

args <- as.numeric(commandArgs(trailingOnly = TRUE))
args <- c(args[1],
          ceiling(args[1]/4),
          ceiling(args[1]-((ceiling(args[1]/4)-1)*4)),
          ceiling(args[1]-((ceiling(args[1]/4)-1)*4))*10-9,
          ceiling(args[1]-((ceiling(args[1]/4)-1)*4))*10)

set.seed(395702)
seeds <- sample.int(100000, 40)
set.seed(seeds[args[1]])

######################################################################################
# 2. Define functions 
#-------------------------------------------------------------------------------------

fit_ltmle <- function (x,outcome,cnodes,anodes,lnodes,qform,gform,sum.measures,msm.formula,SLlib) {

  x <- x[,c(2:245,which(colnames(x)==outcome))]
  
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
                            SL.library = SLlib)
  
}

extract_means <- function (x) {
  
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
  
}

extract_differences <- function (x) {
  
  res <- c(c(plogis(summary(x)$cmat[2,c(1,2)]),
             plogis(summary(x)$cmat[3,c(1,2)]),
             plogis(summary(x)$cmat[4,c(1,2)]),
             plogis(summary(x)$cmat[5,c(1,2)]),
             plogis(summary(x)$cmat[6,c(1,2)]),
             plogis(summary(x)$cmat[7,c(1,2)]),
             plogis(summary(x)$cmat[8,c(1,2)]),
             plogis(summary(x)$cmat[9,c(1,2)]),
             plogis(summary(x)$cmat[10,c(1,2)])))
  
  cov.mat <- var(x$IC)
  
  gradient <- list(c(res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, 0, 0, 0),
                   c(res[2]*(1-res[2]) - res[1]*(1-res[1]), res[2]*(1-res[2]), 0, 0, 0, 0, 0, 0, 0, 0),
                   c(res[3]*(1-res[3]) - res[1]*(1-res[1]), 0, res[3]*(1-res[3]), 0, 0, 0, 0, 0, 0, 0),
                   c(res[4]*(1-res[4]) - res[1]*(1-res[1]), 0, 0, res[4]*(1-res[4]), 0, 0, 0, 0, 0, 0),
                   c(res[5]*(1-res[5]) - res[1]*(1-res[1]), 0, 0, 0, res[5]*(1-res[5]), 0, 0, 0, 0, 0),
                   c(res[6]*(1-res[6]) - res[1]*(1-res[1]), 0, 0, 0, 0, res[6]*(1-res[6]), 0, 0, 0, 0),
                   c(res[7]*(1-res[7]) - res[1]*(1-res[1]), 0, 0, 0, 0, 0, res[7]*(1-res[7]), 0, 0, 0),
                   c(res[8]*(1-res[8]) - res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, res[8]*(1-res[8]), 0, 0),
                   c(res[9]*(1-res[9]) - res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, 0, res[9]*(1-res[9]), 0),
                   c(res[10]*(1-res[10]) - res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, 0, 0, res[10]*(1-res[10])))
  
  v <- do.call(rbind,lapply(gradient, function(z,x) {
    v <- t(z) %*% cov.mat %*% z
    std.dev <- sqrt(v[1, 1] / dim(x$IC)[1])*100
  },x=x))
  
  coef <- c((plogis(summary(x)$cmat[1,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1])*100)-(plogis(summary(x)$cmat[1,1])*100),
            (plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])*100)-(plogis(summary(x)$cmat[1,1])*100))
  
  fin_res <- matrix(cbind(coef,v),nrow=10)
  fin_res <- fin_res[c(2:10),]
  
}

######################################################################################
# 3. Load and transform data 
#-------------------------------------------------------------------------------------

load(paste0(workdir,"Data/primary analysis data - wide form 20230605.RData"))
ltmle_data <- imp_primary
rm(imp_primary)

######################################################################################
# 4. Define LTMLE parameters 
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
# 5. Define included variables and functional forms 
#-------------------------------------------------------------------------------------

cnodes <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")
anodes <- c("activity_bin3","activity_bin4","activity_bin5","activity_bin6","activity_bin7","activity_bin8")
colnums <- match(c('marital_22','mh2','marital_23','mh3','marital_24','mh4','marital_25','mh5','marital_26','mh6','marital_27','mh7') ,names(ltmle_data[[1]]))
lnodes <- colnames(ltmle_data[[1]][,c((colnums[1]):(colnums[2]),(colnums[3]):(colnums[4]),(colnums[5]):(colnums[6]),(colnums[7]):(colnums[8]),(colnums[9]):(colnums[10]),(colnums[11]):(colnums[12]))])

q_base <- "Q.kplus1 ~ b_wtarea + b_cobcat + b_educ_2 + b_educ_3 + b_heartdis_ever + b_hypert_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever"
g_base <- "b_cobcat + b_educ_2 + b_educ_3 + b_heartdis_ever + b_hypert_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever"
w2_conf <- "marital_22 + marital_32 + age2 + ariapgp_22 + ariapgp_32 + employ2 + seifadis_22 + seifadis_32 + live_u182 + live_o182 + cesd102 + mnstrs2 + whobmigroup_22 + whobmigroup_32 + whobmigroup_42 + vegetables2 + fruit2 + alcliferisk2 + alcepisrisk2 + smokst_22 + smokst_32 + pcsa2 + mcsa2 + gh2 + pf2 + re2 + rp2 + bp2 + mh2 + vt2 + sf2"
w3_conf <- "marital_23 + marital_33 + age3 + ariapgp_23 + ariapgp_33 + employ3 + seifadis_23 + seifadis_33 + live_u183 + live_o183 + heartdis_3yr3 + hypert_3yr3 + stroke_3yr3 + cancer_3yr3 + arthritis_3yr3 + depression_3yr3 + anxiety_3yr3 + cesd103 + mnstrs3 + whobmigroup_23 + whobmigroup_33 + whobmigroup_43 + vegetables3 + fruit3 + alcliferisk3 + alcepisrisk3 + smokst_23 + smokst_33 + pcsa3 + mcsa3 + gh3 + pf3 + re3 + rp3 + bp3 + mh3 + vt3 + sf3"
w4_conf <- "marital_24 + marital_34 + age4 + ariapgp_24 + ariapgp_34 + employ4 + seifadis_24 + seifadis_34 + live_u184 + live_o184 + heartdis_3yr4 + hypert_3yr4 + stroke_3yr4 + cancer_3yr4 + arthritis_3yr4 + depression_3yr4 + anxiety_3yr4 + cesd104 + mnstrs4 + whobmigroup_24 + whobmigroup_34 + whobmigroup_44 + vegetables4 + fruit4 + alcliferisk4 + alcepisrisk4 + smokst_24 + smokst_34 + pcsa4 + mcsa4 + gh4 + pf4 + re4 + rp4 + bp4 + mh4 + vt4 + sf4"
w5_conf <- "marital_25 + marital_35 + age5 + ariapgp_25 + ariapgp_35 + employ5 + seifadis_25 + seifadis_35 + live_u185 + live_o185 + heartdis_3yr5 + hypert_3yr5 + stroke_3yr5 + cancer_3yr5 + arthritis_3yr5 + depression_3yr5 + anxiety_3yr5 + cesd105 + mnstrs5 + whobmigroup_25 + whobmigroup_35 + whobmigroup_45 + vegetables5 + fruit5 + alcliferisk5 + alcepisrisk5 + smokst_25 + smokst_35 + pcsa5 + mcsa5 + gh5 + pf5 + re5 + rp5 + bp5 + mh5 + vt5 + sf5"
w6_conf <- "marital_26 + marital_36 + age6 + ariapgp_26 + ariapgp_36 + employ6 + seifadis_26 + seifadis_36 + live_u186 + live_o186 + heartdis_3yr6 + hypert_3yr6 + stroke_3yr6 + cancer_3yr6 + arthritis_3yr6 + depression_3yr6 + anxiety_3yr6 + cesd106 + mnstrs6 + whobmigroup_26 + whobmigroup_36 + whobmigroup_46 + vegetables6 + fruit6 + alcliferisk6 + alcepisrisk6 + smokst_26 + smokst_36 + pcsa6 + mcsa6 + gh6 + pf6 + re6 + rp6 + bp6 + mh6 + vt6 + sf6"
w7_conf <- "marital_27 + marital_37 + age7 + ariapgp_27 + ariapgp_37 + employ7 + seifadis_27 + seifadis_37 + live_u187 + live_o187 + heartdis_3yr7 + hypert_3yr7 + stroke_3yr7 + cancer_3yr7 + arthritis_3yr7 + depression_3yr7 + anxiety_3yr7 + cesd107 + mnstrs7 + whobmigroup_27 + whobmigroup_37 + whobmigroup_47 + vegetables7 + fruit7 + alcliferisk7 + alcepisrisk7 + smokst_27 + smokst_37 + pcsa7 + mcsa7 + gh7 + pf7 + re7 + rp7 + bp7 + mh7 + vt7 + sf7"

q3_form <- paste(q_base,                        w2_conf,sep=" + ")
q4_form <- paste(q_base,w2_conf,"activity_bin3",w3_conf,sep=" + ")
q5_form <- paste(q_base,w3_conf,"activity_bin4",w4_conf,sep=" + ")
q6_form <- paste(q_base,w4_conf,"activity_bin5",w5_conf,sep=" + ")
q7_form <- paste(q_base,w5_conf,"activity_bin6",w6_conf,sep=" + ")
q9_form <- paste(q_base,w2_conf,"activity_bin3",w3_conf,"activity_bin4",
                 w4_conf,"activity_bin5",w5_conf,"activity_bin6",
                 w6_conf,"activity_bin7",w7_conf,"activity_bin8",sep=" + ")

c3_form <- paste("censored3 ~ b_wtarea",    g_base,                                        w2_conf,sep=" + ")
g3_form <- paste("activity_bin3 ~ b_wtarea",g_base,                                        w2_conf,sep=" + ")
c4_form <- paste("censored4 ~ b_wtarea",    g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + ")
g4_form <- paste("activity_bin4 ~ b_wtarea",g_base,                w2_conf,"activity_bin3",w3_conf,sep=" + ")
c5_form <- paste("censored5 ~ b_wtarea",    g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + ")
g5_form <- paste("activity_bin5 ~ b_wtarea",g_base,"activity_bin3",w3_conf,"activity_bin4",w4_conf,sep=" + ")
c6_form <- paste("censored6 ~ b_wtarea",    g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + ")
g6_form <- paste("activity_bin6 ~ b_wtarea",g_base,"activity_bin4",w4_conf,"activity_bin5",w5_conf,sep=" + ")
c7_form <- paste("censored7 ~ b_wtarea",    g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + ")
g7_form <- paste("activity_bin7 ~ b_wtarea",g_base,"activity_bin5",w5_conf,"activity_bin6",w6_conf,sep=" + ")
c8_form <- paste("censored8 ~ b_wtarea",    g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + ")
g8_form <- paste("activity_bin8 ~ b_wtarea",g_base,"activity_bin6",w6_conf,"activity_bin7",w7_conf,sep=" + ")
c9_form <- paste("censored9 ~ b_wtarea",    g_base,"activity_bin7",w7_conf,"activity_bin8",sep=" + ")

qform <- c(marital_23=q3_form,
           marital_24=q4_form,
           marital_25=q5_form,
           marital_26=q6_form,
           marital_27=q7_form,
           outcome=q9_form)
gform <- c(censored3=c3_form,activity_bin3=g3_form,
           censored4=c4_form,activity_bin4=g4_form,
           censored5=c5_form,activity_bin5=g5_form,
           censored6=c6_form,activity_bin6=g6_form,
           censored7=c7_form,activity_bin7=g7_form,
           censored8=c8_form,activity_bin8=g8_form,
           censored9=c9_form)

######################################################################################
# 6. Define SuperLearner Libraries
#-------------------------------------------------------------------------------------

ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128))

SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
              g=c("SL.mean","SL.glm","SL.gam"))
SLlib2 <- list(Q=c("SL.mean","SL.glm","SL.gam"),
               g=c("SL.mean","SL.glm","SL.gam",ranger_128$names))

######################################################################################
# 7. Run LTMLE models 
#-------------------------------------------------------------------------------------

outcomes <- c("pcsa9","mcsa9","pf9","rp9","bp9","gh9","vt9","sf9","re9","mh9")
outcome <- outcomes[args[2]]

fit <- lapply(ltmle_data[args[4]:args[5]],
              fit_ltmle,
              outcome = outcome,
              cnodes = cnodes,
              anodes = anodes,
              lnodes = lnodes,
              qform = qform,
              gform = gform,
              sum.measures = sum.measures,
              msm.formula = msm.formula,
              SLlib = SLlib2)

######################################################################################
# 8. Extract coefficients and standard errors
#-------------------------------------------------------------------------------------

mean <- lapply(fit,extract_means)

diff <- lapply(fit,extract_differences)

######################################################################################
# 9. Save output
#-------------------------------------------------------------------------------------

saveRDS(mean,file=paste0(workdir,"Results/primary-means-",outcome,"-",args[3],".rds"))
saveRDS(diff,file=paste0(workdir,"Results/primary-differences-",outcome,"-",args[3],".rds"))
