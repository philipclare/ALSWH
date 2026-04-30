######################################################################################
##   
## Effects of physical activity on incident obesity
## Additional 'natural' regime analysis
## Date: 26 Feb 2026
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
libs <- c("Amelia","arm","dplyr","fastDummies","gam","ltmle","parallel","ranger","SuperLearner")
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

args <- c(ceiling(args[1]/25),
          ceiling(args[1]-((ceiling(args[1]/25)-1)*25)))

# 1.4. Set seed to ensure replicability
set.seed(697924)
seeds <- sample.int(100000, 25)
set.seed(seeds[args[1]])

######################################################################################
# 2. Load data
#-------------------------------------------------------------------------------------

analysis_data <- list(readRDS(paste0(workdir,"Data/primary analysis data - 20240827.rds")), #working
                      readRDS(paste0(workdir,"Data/categorical analysis data - 20240827.rds")),
                      readRDS(paste0(workdir,"Data/severe obesity analysis data - 20240827.rds")), #working
                      readRDS(paste0(workdir,"Data/weight gain a analysis data - 20240827.rds")),
                      readRDS(paste0(workdir,"Data/weight gain b analysis data - 20240827.rds")), #working
                      lapply(readRDS(paste0(workdir,"Data/stratified analysis data - 20250606.rds")), function (x) {x[[1]]}),
                      lapply(readRDS(paste0(workdir,"Data/stratified analysis data - 20250606.rds")), function (x) {x[[1]]}))

analysis_name <- c("pr","cat","sev","5pct","10pct","low_educ","high_educ")[[args[1]]]

analysis_data <- analysis_data[[args[1]]] #[[args[2]]]

analysis <- function (analysis_data) {
  analysis_data <- analysis_data %>%
    mutate(obesity5 = ifelse(obesity4 == 1, 1, obesity5),
           obesity6 = ifelse(obesity5 == 1, 1, obesity6),
           obesity7 = ifelse(obesity6 == 1, 1, obesity7),
           obesity8 = ifelse(obesity7 == 1, 1, obesity8),
           obesity9 = ifelse(obesity8 == 1, 1, obesity9),
           obesity10 = ifelse(obesity9 == 1, 1, obesity10))
  
  ######################################################################################
  # 3. Define LTMLE parameters 
  #-------------------------------------------------------------------------------------
  if (args[[1]]==1 | args[[1]]==3 | args[[1]]==4 | args[[1]]==5 | args[[1]]==6 | args[[1]]==7) {
    rule_natural <- list(function (x) {
      c(1,1,1,1,1,1,1)
    },
    function (x) {
      c(x$activity_bin3,x$activity_bin4,x$activity_bin5,x$activity_bin6,x$activity_bin7,x$activity_bin8,x$activity_bin9)
    })
  } else if (args[[1]]==2) {
    rule_natural <- list(function (x) {
      c(1,0,1,0,1,0,1,0,1,0,1,0,1,0)
    },
    function (x) {
      c(x$activity_cat_13,x$activity_cat_23,x$activity_cat_14,x$activity_cat_24,x$activity_cat_15,x$activity_cat_25,x$activity_cat_16,x$activity_cat_26,
        x$activity_cat_17,x$activity_cat_27,x$activity_cat_18,x$activity_cat_28,x$activity_cat_19,x$activity_cat_29)
    })
  }
  
  ######################################################################################
  # 4. Define included variables and functional forms 
  #-------------------------------------------------------------------------------------
  if (args[[1]]==1 | args[[1]]==3 | args[[1]]==4 | args[[1]]==5 | args[[1]]==6 | args[[1]]==7) {
    
    ynodes <- c("obesity4","obesity5","obesity6","obesity7","obesity8","obesity9","obesity10")
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
    
    
    if (args[[1]]==6 | args[[1]]==7) {
      q_base <- "Q.kplus1 ~ b_bmi + b_heartdis_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever + b_metmin + b_cobcat + b_pcsa + b_mcsa + "
      g_base <- "b_bmi + b_heartdis_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever + b_metmin + b_cobcat + b_pcsa + b_mcsa + "
    }
    else {
      q_base <- "Q.kplus1 ~ b_bmi + b_heartdis_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever + b_metmin + b_cobcat + b_pcsa + b_mcsa + b_educ_2 + b_educ_3 + "
      g_base <- "b_bmi + b_heartdis_ever + b_stroke_ever + b_cancer_ever + b_depression_ever + b_anxiety_ever + b_metmin + b_cobcat + b_pcsa + b_mcsa + b_educ_2 + b_educ_3 + "
    }
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
    
    y4_form <- paste(q_base,"activity_bin3",w2_conf,sep=" + ")
    y5_form <- paste(q_base,"activity_bin3",w2_conf,"activity_bin4",w3_conf,sep=" + ")
    y6_form <- paste(q_base,"activity_bin3",w2_conf,"activity_bin4",w3_conf,"activity_bin5",w4_conf,sep=" + ")
    y7_form <- paste(q_base,"activity_bin3",w2_conf,"activity_bin4",w3_conf,"activity_bin5",w4_conf,"activity_bin6",w5_conf,sep=" + ")
    y8_form <- paste(q_base,"activity_bin3",w2_conf,"activity_bin4",w3_conf,"activity_bin5",w4_conf,"activity_bin6",w5_conf,"activity_bin7",w6_conf,sep=" + ")
    y9_form <- paste(q_base,"activity_bin3",w2_conf,"activity_bin4",w3_conf,"activity_bin5",w4_conf,"activity_bin6",w5_conf,"activity_bin7",w6_conf,"activity_bin8",w7_conf,sep=" + ")
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
               obesity4=y4_form,marital_24=l4_form,
               obesity5=y5_form,marital_25=l5_form,
               obesity6=y6_form,marital_26=l6_form,
               obesity7=y7_form,marital_27=l7_form,
               obesity8=y8_form,marital_28=l8_form,
               obesity9=y9_form,
               obesity10=y10_form)
    gform <- c(death3=c3_form,activity_bin3=g3_form,
               death4=c4_form,activity_bin4=g4_form,
               death5=c5_form,activity_bin5=g5_form,
               death6=c6_form,activity_bin6=g6_form,
               death7=c7_form,activity_bin7=g7_form,
               death8=c8_form,activity_bin8=g8_form,
               death9=c9_form,activity_bin9=g9_form,
               death10=c10_form)
  } else if (args[[1]]==2) {
    ynodes <- c("obesity4","obesity5","obesity6","obesity7","obesity8","obesity9","obesity10")
    cnodes <- c("death3","death4","death5","death6","death7","death8","death9","death10")
    anodes <- c("activity_cat_13","activity_cat_23","activity_cat_14","activity_cat_24","activity_cat_15","activity_cat_25","activity_cat_16","activity_cat_26",
                "activity_cat_17","activity_cat_27","activity_cat_18","activity_cat_28","activity_cat_19","activity_cat_29")
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
    l4_form <- paste(q_base,"activity_cat_13","activity_cat_23",w3_conf,w2_conf,sep=" + ")
    l5_form <- paste(q_base,"activity_cat_14","activity_cat_24",w4_conf,"activity_cat_13","activity_cat_23",w3_conf,sep=" + ")
    l6_form <- paste(q_base,"activity_cat_15","activity_cat_25",w5_conf,"activity_cat_14","activity_cat_24",w4_conf,sep=" + ")
    l7_form <- paste(q_base,"activity_cat_16","activity_cat_26",w6_conf,"activity_cat_15","activity_cat_25",w5_conf,sep=" + ")
    l8_form <- paste(q_base,"activity_cat_17","activity_cat_27",w7_conf,"activity_cat_16","activity_cat_26",w6_conf,sep=" + ")
    
    y4_form  <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,sep=" + ")
    y4_form  <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,sep=" + ")
    y5_form  <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,"activity_cat_14","activity_cat_24",w3_conf,sep=" + ")
    y6_form  <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,"activity_cat_14","activity_cat_24",w3_conf,"activity_cat_15","activity_cat_25",w4_conf,sep=" + ")
    y7_form  <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,"activity_cat_14","activity_cat_24",w3_conf,"activity_cat_15","activity_cat_25",w4_conf,"activity_cat_16","activity_cat_26",w5_conf,sep=" + ")
    y8_form  <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,"activity_cat_14","activity_cat_24",w3_conf,"activity_cat_15","activity_cat_25",w4_conf,"activity_cat_16","activity_cat_26",w5_conf,"activity_cat_17","activity_cat_27",w6_conf,sep=" + ")
    y9_form  <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,"activity_cat_14","activity_cat_24",w3_conf,"activity_cat_15","activity_cat_25",w4_conf,"activity_cat_16","activity_cat_26",w5_conf,"activity_cat_17","activity_cat_27",w6_conf,"activity_cat_18","activity_cat_28",w7_conf,sep=" + ")
    y10_form <- paste(q_base,"activity_cat_13","activity_cat_23",w2_conf,"activity_cat_14","activity_cat_24",w3_conf,"activity_cat_15","activity_cat_25",w4_conf,"activity_cat_16","activity_cat_26",w5_conf,"activity_cat_17","activity_cat_27",w6_conf,"activity_cat_18","activity_cat_28",w7_conf,"activity_cat_19","activity_cat_29",w8_conf,sep=" + ")
    
    c3_form   <- paste0("death3 ~ ",         paste(g_base,                                                                                w2_conf,sep=" + "))
    g3_1_form <- paste0("activity_cat_13 ~ ",paste(g_base,                                                                                w2_conf,sep=" + "))
    g3_2_form <- paste0("activity_cat_23 ~ ",paste(g_base,                                                                                w2_conf,sep=" + "))
    c4_form   <- paste0("death4 ~ ",         paste(g_base,                                    w2_conf,"activity_cat_13","activity_cat_23",w3_conf,sep=" + "))
    g4_1_form <- paste0("activity_cat_14 ~ ",paste(g_base,                                    w2_conf,"activity_cat_13","activity_cat_23",w3_conf,sep=" + "))
    g4_2_form <- paste0("activity_cat_24 ~ ",paste(g_base,                                    w2_conf,"activity_cat_13","activity_cat_23",w3_conf,sep=" + "))
    c5_form   <- paste0("death5 ~ ",         paste(g_base,"activity_cat_13","activity_cat_23",w3_conf,"activity_cat_14","activity_cat_24",w4_conf,sep=" + "))
    g5_1_form <- paste0("activity_cat_15 ~ ",paste(g_base,"activity_cat_13","activity_cat_23",w3_conf,"activity_cat_14","activity_cat_24",w4_conf,sep=" + "))
    g5_2_form <- paste0("activity_cat_25 ~ ",paste(g_base,"activity_cat_13","activity_cat_23",w3_conf,"activity_cat_14","activity_cat_24",w4_conf,sep=" + "))
    c6_form   <- paste0("death6 ~ ",         paste(g_base,"activity_cat_14","activity_cat_24",w4_conf,"activity_cat_15","activity_cat_25",w5_conf,sep=" + "))
    g6_1_form <- paste0("activity_cat_16 ~ ",paste(g_base,"activity_cat_14","activity_cat_24",w4_conf,"activity_cat_15","activity_cat_25",w5_conf,sep=" + "))
    g6_2_form <- paste0("activity_cat_26 ~ ",paste(g_base,"activity_cat_14","activity_cat_24",w4_conf,"activity_cat_15","activity_cat_25",w5_conf,sep=" + "))
    c7_form   <- paste0("death7 ~ ",         paste(g_base,"activity_cat_15","activity_cat_25",w5_conf,"activity_cat_16","activity_cat_26",w6_conf,sep=" + "))
    g7_1_form <- paste0("activity_cat_17 ~ ",paste(g_base,"activity_cat_15","activity_cat_25",w5_conf,"activity_cat_16","activity_cat_26",w6_conf,sep=" + "))
    g7_2_form <- paste0("activity_cat_27 ~ ",paste(g_base,"activity_cat_15","activity_cat_25",w5_conf,"activity_cat_16","activity_cat_26",w6_conf,sep=" + "))
    c8_form   <- paste0("death8 ~ ",         paste(g_base,"activity_cat_16","activity_cat_26",w6_conf,"activity_cat_17","activity_cat_27",w7_conf,sep=" + "))
    g8_1_form <- paste0("activity_cat_18 ~ ",paste(g_base,"activity_cat_16","activity_cat_26",w6_conf,"activity_cat_17","activity_cat_27",w7_conf,sep=" + "))
    g8_2_form <- paste0("activity_cat_28 ~ ",paste(g_base,"activity_cat_16","activity_cat_26",w6_conf,"activity_cat_17","activity_cat_27",w7_conf,sep=" + "))
    c9_form   <- paste0("death9 ~ ",         paste(g_base,"activity_cat_17","activity_cat_27",w7_conf,"activity_cat_18","activity_cat_28",w8_conf,sep=" + "))
    g9_1_form <- paste0("activity_cat_19 ~ ",paste(g_base,"activity_cat_17","activity_cat_27",w6_conf,"activity_cat_18","activity_cat_28",w8_conf,sep=" + "))
    g9_2_form <- paste0("activity_cat_29 ~ ",paste(g_base,"activity_cat_17","activity_cat_27",w6_conf,"activity_cat_18","activity_cat_28",w8_conf,sep=" + "))
    c10_form  <- paste0("death10 ~ ",        paste(g_base,"activity_cat_18","activity_cat_28",w7_conf,"activity_cat_19","activity_cat_29",sep=" + "))
    
    qform <- c(marital_23=l3_form,
               obesity4=y4_form,marital_24=l4_form,
               obesity5=y5_form,marital_25=l5_form,
               obesity6=y6_form,marital_26=l6_form,
               obesity7=y7_form,marital_27=l7_form,
               obesity8=y8_form,marital_28=l8_form,
               obesity9=y9_form,
               obesity10=y10_form)
    gform <- c(death3=c3_form,activity_cat_13=g3_1_form,activity_cat_23=g3_2_form,
               death4=c4_form,activity_cat_14=g4_1_form,activity_cat_24=g4_2_form,
               death5=c5_form,activity_cat_15=g5_1_form,activity_cat_25=g5_2_form,
               death6=c6_form,activity_cat_16=g6_1_form,activity_cat_26=g6_2_form,
               death7=c7_form,activity_cat_17=g7_1_form,activity_cat_27=g7_2_form,
               death8=c8_form,activity_cat_18=g8_1_form,activity_cat_28=g8_2_form,
               death9=c9_form,activity_cat_19=g9_1_form,activity_cat_29=g9_2_form,
               death10=c10_form)
  }
  
  ######################################################################################
  # 5. Define SuperLearner Libraries
  #-------------------------------------------------------------------------------------
  
  ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128))
  
  SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
                g=c("SL.mean","SL.glm","SL.gam",ranger_128$names))
  
  ######################################################################################
  # 6. Run LTMLE models 
  #-------------------------------------------------------------------------------------
  
  if (args[[1]]==1 | args[[1]]==2 | args[[1]]==3 | args[[1]]==6 | args[[1]]==7) {
    
    model_fit_cont <- ltmle(analysis_data[,-1],
                            Cnodes = cnodes,
                            Anodes = anodes,
                            Lnodes = lnodes,
                            Ynodes = ynodes,
                            Qform = qform,
                            gform = gform,
                            rule = rule_natural,
                            survivalOutcome = TRUE,
                            SL.library = SLlib,
                            variance.method = "ic",
                            observation.weights = analysis_data$b_wtarea)
    
  } else if (args[[1]]==4 | args[[1]]==5) {
    model_fit_cont <- ltmle(analysis_data[,-1],
                            Cnodes = cnodes,
                            Anodes = anodes,
                            Lnodes = lnodes,
                            Ynodes = ynodes,
                            Qform = qform,
                            gform = gform,
                            rule = rule_natural,
                            survivalOutcome = FALSE,
                            SL.library = SLlib,
                            variance.method = "ic",
                            observation.weights = analysis_data$b_wtarea)
  }
  model_fit_cont
}
ranger_128 <- create.Learner("SL.ranger", params = list(num.trees = 128))
res <- lapply(analysis_data,analysis)

sum <- lapply(res,summary)

est <- do.call(rbind,lapply(sum, function (x) {
  c(x$effect.measures$treatment$estimate,
    x$effect.measures$control$estimate,
    x$effect.measures$ATE$estimate,
    x$effect.measures$RR$estimate)
}))

se <- do.call(rbind,lapply(sum, function (x) {
  c(x$effect.measures$treatment$std.dev,
    x$effect.measures$control$std.dev,
    x$effect.measures$ATE$std.dev,
    x$effect.measures$RR$std.dev)
}))

mi.meld(q=est,se=se)

output <- data.frame(est=as.vector(mi.meld(q=est,se=se)$q.mi),
                     se=as.vector(mi.meld(q=est,se=se)$se.mi))


######################################################################################
# 5. Save output
#-------------------------------------------------------------------------------------

# saveRDS(res,file=paste0(workdir,"Results/natural-summary-",analysis_name,"-",args[2],".rds"))

end_time <- Sys.time()
end_time - start_time
