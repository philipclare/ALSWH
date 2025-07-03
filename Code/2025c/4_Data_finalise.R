######################################################################################
##   
## Effects of physical activity on incident obesity
## Finalise data after imputation and format for analysis
## Date: 17 June 2025
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://osf.io/fyszg
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Specify paths to Katana/windows/Mac paths based on system
if (Sys.info()[['sysname']]=="Linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh_lonely/"
} else if (Sys.info()[['sysname']]=="Windows") {
  workdir <- "Y:/PRJ-prc_alswh/Paper 3 - Obesity/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/Paper 3 - Obesity/" # MAC
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("dplyr","fastDummies","gtools","ltmle")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

cens_list <- c("death3","death4","death5","death6","death7","death8","death9","death10")
seifa_list <- c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7","seifadis8")

######################################################################################
# 2. Define functions
#-------------------------------------------------------------------------------------

derive_variables <- function (x) {
  
  x$alcliferisk <- ifelse(x$alcfq>10,1,0)
  x$alcepisrisk <- ifelse(x$alcbng>1,1,0)
  x$activity_bin <- ifelse(x$weighted_activity_time>=150,1,0)
  x$activity_cat <- ifelse(x$weighted_activity_time>=300,2,x$activity_bin)
  
  x$bmi <- x$wtkg/((x$htcm/100)^2)
  x$obesity <- ifelse(x$bmi>=30,1,0)
  x$obesity_sev <- ifelse(x$bmi>=35,1,0)
  
  x <- subset(x, select = -c(alcfq,alcbng,weighted_activity_time,inarea,b_hypert_ever,hypert_3yr,wtkg,htcm,whobmigroup))
  x
  
}

dummy_code <- function (x) {
  
  
  x[,c("b_cobcat","employ","live_u18","live_o18","heartdis_3yr","stroke_3yr","hrt",
       "cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","vegetables","fruit")] <- lapply(x[,c("b_cobcat","employ","live_u18","live_o18","heartdis_3yr","stroke_3yr","hrt",
                                                                                                         "cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","vegetables","fruit")], function (x) {as.numeric(x)-1})
  
  x <- dummy_cols(x, select_columns=c('b_educ','marital','ariapgp','smokst','menopause','sleep_cat'),remove_most_frequent_dummy=TRUE,remove_selected_columns=TRUE)
  
  x <- x %>% 
    dplyr::rename(
      b_educ_2 = 'b_educ_Trade/apprentice/certificate/diploma',
      b_educ_3 = 'b_educ_University',
      marital_2 = 'marital_Separated/divorced/never married',
      marital_3 = 'marital_Widowed',
      ariapgp_2 = 'ariapgp_Major city',
      ariapgp_3 = 'ariapgp_Remote',
      smokst_2 = 'smokst_Current smoker',
      smokst_3 = 'smokst_Ex smoker',
      menopause_2 = 'menopause_Pre',
      menopause_3 = 'menopause_Surgical')
  
  x <- dummy_cols(x, select_columns='activity_cat',remove_first_dummy=TRUE,remove_selected_columns=TRUE)
  
  x <- x %>%
    mutate(
      seifadis = case_match(seifadis, c(1,2) ~ 1, c(3,4) ~ 2, c(5,6) ~ 3, c(7,8) ~ 4, c(9,10) ~ 5)
    )
  
  x <- dummy_cols(x, select_columns='seifadis',ignore_na=TRUE,remove_first_dummy=TRUE,remove_selected_columns=TRUE)
  
  x
  
}

######################################################################################
# 2. Load imputed data
#-------------------------------------------------------------------------------------

imp <- readRDS(file=paste0(workdir,"Data/imputed data - long form.rds"))
imp <- imp[1:25]
imp_sens <- readRDS(file=paste0(workdir,"Data/imputed data - long form for sensitivity.rds"))
imp_sens <- imp_sens[1:25]

######################################################################################
# 3. Create computed/derived variables
#-------------------------------------------------------------------------------------

imp <- lapply(imp,derive_variables)

imp_sens <- lapply(imp_sens,derive_variables)

######################################################################################
# 4. Dummy code factors
#-------------------------------------------------------------------------------------

imp <- lapply(imp,dummy_code)

imp_sens <- lapply(imp_sens,dummy_code)

imp_sens <- lapply(imp_sens,function (x) {
  
  x[,c("dentist","vitamins")] <- lapply(x[,c("dentist","vitamins")], function (x) {as.numeric(x)-1})
  
  x <- dummy_cols(x, select_columns=c('pap','mam'),remove_most_frequent_dummy=TRUE,remove_selected_columns=TRUE)
  
  x <- x %>% 
    dplyr::rename(
      pap_2 = 'pap_Longer ago',
      pap_3 = 'pap_Never',
      mam_2 = 'mam_Longer ago',
      mam_3 = 'mam_Never')
  
  x
  
})

######################################################################################
# 5. Reshape to wide, drop unnecessary variables and structure for LTMLE
#-------------------------------------------------------------------------------------

imp <- lapply(imp,function (x) {
  
  x <- x[order(x$wave),]
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c(
                 "death","obesity","obesity_sev","activity_bin","activity_cat_1","activity_cat_2",
                 "marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ",
                 "live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5",
                 "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                 "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3",
                 "menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","sleep_prob",
                 "finfinc","pcsa","mcsa","bmi"),
               sep = "",
               dir="wide")
  
  x$b_bmi <- x$bmi2
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c(
                 "death","obesity","obesity_sev",
                 "activity_bin","activity_cat_1","activity_cat_2",
                 "marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ",
                 "live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5",
                 "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                 "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3",
                 "menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","sleep_prob",
                 "finfinc","pcsa","mcsa","bmi"),
               sep = "",
               dir="long")
  
  x$weight_gain_a <- ifelse((x$bmi/x$b_bmi)>1.05,1,0)
  x$weight_gain_b <- ifelse((x$bmi/x$b_bmi)>1.1,1,0)
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c(
                 "death","obesity","obesity_sev","weight_gain_a","weight_gain_b",
                 "activity_bin","activity_cat_1","activity_cat_2",
                 "marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ",
                 "live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5",
                 "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                 "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3",
                 "menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","sleep_prob",
                 "finfinc","pcsa","mcsa","bmi"),
               sep = "",
               dir="wide")
  
  x <- subset(x, select=-death2)
  
  x[,cens_list] <- lapply(x[,cens_list], function (y) {
    y <- BinaryToCensoring(is.censored=y)
  })
  
  x <- subset(x, select = -c(heartdis_3yr2,stroke_3yr2,cancer_3yr2,arthritis_3yr2,depression_3yr2,anxiety_3yr2))
  
  x <- subset(x, select = -c(b_htcm,b_wtkg,b_pf,obesity2,obesity3,obesity_sev2,obesity_sev3,weight_gain_a2,weight_gain_a3,weight_gain_b2,weight_gain_b3,
                             sleep_cat_14,sleep_cat_15,sleep_cat_16,sleep_cat_17,sleep_cat_18,sleep_cat_19,sleep_cat_110,
                             sleep_cat_24,sleep_cat_25,sleep_cat_26,sleep_cat_27,sleep_cat_28,sleep_cat_29,sleep_cat_210,
                             sleep_cat_44,sleep_cat_45,sleep_cat_46,sleep_cat_47,sleep_cat_48,sleep_cat_49,sleep_cat_410,
                             sleep_prob2,sleep_prob3,
                             activity_bin2,activity_cat_12,activity_cat_22,
                             marital_29,marital_39,age9,ariapgp_29,ariapgp_39,employ9,seifadis_29,seifadis_39,seifadis_49,seifadis_59,live_u189,live_o189,
                             heartdis_3yr9,stroke_3yr9,cancer_3yr9,arthritis_3yr9,depression_3yr9,anxiety_3yr9,
                             cesd109,mnstrs9,vegetables9,fruit9,alcliferisk9,alcepisrisk9,smokst_29,smokst_39,
                             menopause_29,menopause_39,hrt9,pcsa9,mcsa9,sleep_prob9,finfinc9,bmi9,
                             marital_210,marital_310,age10,ariapgp_210,ariapgp_310,employ10,seifadis_210,seifadis_310,seifadis_410,seifadis_510,live_u1810,live_o1810,
                             heartdis_3yr10,stroke_3yr10,cancer_3yr10,arthritis_3yr10,depression_3yr10,anxiety_3yr10,
                             cesd1010,mnstrs10,vegetables10,fruit10,alcliferisk10,alcepisrisk10,smokst_210,smokst_310,
                             menopause_210,menopause_310,hrt10,pcsa10,mcsa10,sleep_prob10,finfinc10,bmi10,
                             activity_bin10,activity_cat_110,activity_cat_210))
  x
  
})

imp_sens <- lapply(imp_sens,function (x) {
  
  x <- x[order(x$wave),]
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("dentist","vitamins","pap_2","pap_3","mam_2","mam_3",
                         "death","obesity","obesity_sev",
                         "activity_bin","activity_cat_1","activity_cat_2",
                         "marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ",
                         "live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5",
                         "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                         "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3",
                         "menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","sleep_prob",
                         "finfinc","pcsa","mcsa","bmi"),
               sep = "",
               dir="wide")
  
  x$b_bmi <- x$bmi2
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("dentist","vitamins","pap_2","pap_3","mam_2","mam_3",
                         "death","obesity","obesity_sev",
                         "activity_bin","activity_cat_1","activity_cat_2",
                         "marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ",
                         "live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5",
                         "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                         "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3",
                         "menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","sleep_prob",
                         "finfinc","pcsa","mcsa","bmi"),
               sep = "",
               dir="long")
  
  x$weight_gain_a <- ifelse((x$bmi/x$b_bmi)>1.05,1,0)
  x$weight_gain_b <- ifelse((x$bmi/x$b_bmi)>1.1,1,0)
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("dentist","vitamins","pap_2","pap_3","mam_2","mam_3",
                         "death","obesity","obesity_sev","weight_gain_a","weight_gain_b",
                         "activity_bin","activity_cat_1","activity_cat_2",
                         "marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ",
                         "live_u18","live_o18","seifadis_2","seifadis_3","seifadis_4","seifadis_5",
                         "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                         "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst_2","smokst_3",
                         "menopause_2","menopause_3","hrt","sleep_cat_1","sleep_cat_2","sleep_cat_4","sleep_prob",
                         "finfinc","pcsa","mcsa","bmi"),
               sep = "",
               dir="wide")
  
  x <- subset(x, select=-death2)
  
  x[,cens_list] <- lapply(x[,cens_list], function (y) {
    y <- BinaryToCensoring(is.censored=y)
  })
  
  x <- subset(x, select = -c(heartdis_3yr2,stroke_3yr2,cancer_3yr2,arthritis_3yr2,depression_3yr2,anxiety_3yr2))
  
  
  x <- subset(x, select = -c(b_htcm,b_wtkg,b_pf,obesity2,obesity3,obesity_sev2,obesity_sev3,weight_gain_a2,weight_gain_a3,weight_gain_b2,weight_gain_b3,
                             dentist2,vitamins2,pap_22,pap_32,mam_22,mam_32,dentist3,vitamins3,pap_23,pap_33,mam_23,mam_33,
                             sleep_cat_14,sleep_cat_15,sleep_cat_16,sleep_cat_17,sleep_cat_18,sleep_cat_19,sleep_cat_110,
                             sleep_cat_24,sleep_cat_25,sleep_cat_26,sleep_cat_27,sleep_cat_28,sleep_cat_29,sleep_cat_210,
                             sleep_cat_44,sleep_cat_45,sleep_cat_46,sleep_cat_47,sleep_cat_48,sleep_cat_49,sleep_cat_410,
                             sleep_prob2,sleep_prob3,
                             activity_bin2,activity_cat_12,activity_cat_22,
                             marital_29,marital_39,age9,ariapgp_29,ariapgp_39,employ9,seifadis_29,seifadis_39,seifadis_49,seifadis_59,live_u189,live_o189,
                             heartdis_3yr9,stroke_3yr9,cancer_3yr9,arthritis_3yr9,depression_3yr9,anxiety_3yr9,
                             cesd109,mnstrs9,vegetables9,fruit9,alcliferisk9,alcepisrisk9,smokst_29,smokst_39,
                             menopause_29,menopause_39,hrt9,pcsa9,mcsa9,sleep_prob9,finfinc9,bmi9,
                             marital_210,marital_310,age10,ariapgp_210,ariapgp_310,employ10,seifadis_210,seifadis_310,seifadis_410,seifadis_510,live_u1810,live_o1810,
                             heartdis_3yr10,stroke_3yr10,cancer_3yr10,arthritis_3yr10,depression_3yr10,anxiety_3yr10,
                             cesd1010,mnstrs10,vegetables10,fruit10,alcliferisk10,alcepisrisk10,smokst_210,smokst_310,
                             menopause_210,menopause_310,hrt10,pcsa10,mcsa10,sleep_prob10,finfinc10,bmi10,
                             activity_bin10,activity_cat_110,activity_cat_210))
  x
  
})

######################################################################################
# 6. Create analysis-specific data for primary, s1, s2
#-------------------------------------------------------------------------------------

imp_primary <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_cat_13,activity_cat_14,activity_cat_15,activity_cat_16,activity_cat_17,activity_cat_18,activity_cat_19,
                           activity_cat_23,activity_cat_24,activity_cat_25,activity_cat_26,activity_cat_27,activity_cat_28,activity_cat_29,
                           obesity_sev4,obesity_sev5,obesity_sev6,obesity_sev7,obesity_sev8,obesity_sev9,obesity_sev10,
                           weight_gain_a4,weight_gain_a5,weight_gain_a6,weight_gain_a7,weight_gain_a8,weight_gain_a9,weight_gain_a10,
                           weight_gain_b4,weight_gain_b5,weight_gain_b6,weight_gain_b7,weight_gain_b8,weight_gain_b9,weight_gain_b10))
  x
  
})

imp_cat <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,activity_bin9,
                           obesity_sev4,obesity_sev5,obesity_sev6,obesity_sev7,obesity_sev8,obesity_sev9,obesity_sev10,
                           weight_gain_a4,weight_gain_a5,weight_gain_a6,weight_gain_a7,weight_gain_a8,weight_gain_a9,weight_gain_a10,
                           weight_gain_b4,weight_gain_b5,weight_gain_b6,weight_gain_b7,weight_gain_b8,weight_gain_b9,weight_gain_b10))
  x
  
})

imp_sevob <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_cat_13,activity_cat_14,activity_cat_15,activity_cat_16,activity_cat_17,activity_cat_18,activity_cat_19,
                           activity_cat_23,activity_cat_24,activity_cat_25,activity_cat_26,activity_cat_27,activity_cat_28,activity_cat_29,
                           obesity4,obesity5,obesity6,obesity7,obesity8,obesity9,obesity10,
                           weight_gain_a4,weight_gain_a5,weight_gain_a6,weight_gain_a7,weight_gain_a8,weight_gain_a9,weight_gain_a10,
                           weight_gain_b4,weight_gain_b5,weight_gain_b6,weight_gain_b7,weight_gain_b8,weight_gain_b9,weight_gain_b10))
  
  x <- x %>%
    dplyr::rename(
      obesity4 = obesity_sev4,
      obesity5 = obesity_sev5,
      obesity6 = obesity_sev6,
      obesity7 = obesity_sev7,
      obesity8 = obesity_sev8,
      obesity9 = obesity_sev9,
      obesity10 = obesity_sev10
    )
  
  x
  
})

imp_gaina <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_cat_13,activity_cat_14,activity_cat_15,activity_cat_16,activity_cat_17,activity_cat_18,activity_cat_19,
                           activity_cat_23,activity_cat_24,activity_cat_25,activity_cat_26,activity_cat_27,activity_cat_28,activity_cat_29,
                           obesity4,obesity5,obesity6,obesity7,obesity8,obesity9,obesity10,
                           obesity_sev4,obesity_sev5,obesity_sev6,obesity_sev7,obesity_sev8,obesity_sev9,obesity_sev10,
                           weight_gain_b4,weight_gain_b5,weight_gain_b6,weight_gain_b7,weight_gain_b8,weight_gain_b9,weight_gain_b10))
  
  x <- x %>%
    dplyr::rename(
      obesity4 = weight_gain_a4,
      obesity5 = weight_gain_a5,
      obesity6 = weight_gain_a6,
      obesity7 = weight_gain_a7,
      obesity8 = weight_gain_a8,
      obesity9 = weight_gain_a9,
      obesity10 = weight_gain_a10
    )
  
  x
  
})

imp_gainb <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_cat_13,activity_cat_14,activity_cat_15,activity_cat_16,activity_cat_17,activity_cat_18,activity_cat_19,
                           activity_cat_23,activity_cat_24,activity_cat_25,activity_cat_26,activity_cat_27,activity_cat_28,activity_cat_29,
                           obesity4,obesity5,obesity6,obesity7,obesity8,obesity9,obesity10,
                           obesity_sev4,obesity_sev5,obesity_sev6,obesity_sev7,obesity_sev8,obesity_sev9,obesity_sev10,
                           weight_gain_a4,weight_gain_a5,weight_gain_a6,weight_gain_a7,weight_gain_a8,weight_gain_a9,weight_gain_a10))
  
  x <- x %>%
    dplyr::rename(
      obesity4 = weight_gain_b4,
      obesity5 = weight_gain_b5,
      obesity6 = weight_gain_b6,
      obesity7 = weight_gain_b7,
      obesity8 = weight_gain_b8,
      obesity9 = weight_gain_b9,
      obesity10 = weight_gain_b10
    )
  
  x
  
})

imp_sens_adj <- lapply(imp_sens, function (x) {
  
  x <- subset(x, select=-c(activity_cat_13,activity_cat_14,activity_cat_15,activity_cat_16,activity_cat_17,activity_cat_18,activity_cat_19,
                           activity_cat_23,activity_cat_24,activity_cat_25,activity_cat_26,activity_cat_27,activity_cat_28,activity_cat_29,
                           obesity_sev4,obesity_sev5,obesity_sev6,obesity_sev7,obesity_sev8,obesity_sev9,obesity_sev10,
                           weight_gain_a4,weight_gain_a5,weight_gain_a6,weight_gain_a7,weight_gain_a8,weight_gain_a9,weight_gain_a10,
                           weight_gain_b4,weight_gain_b5,weight_gain_b6,weight_gain_b7,weight_gain_b8,weight_gain_b9,weight_gain_b10))
  x
  
})

imp_strat <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_cat_13,activity_cat_14,activity_cat_15,activity_cat_16,activity_cat_17,activity_cat_18,activity_cat_19,
                           activity_cat_23,activity_cat_24,activity_cat_25,activity_cat_26,activity_cat_27,activity_cat_28,activity_cat_29,
                           obesity_sev4,obesity_sev5,obesity_sev6,obesity_sev7,obesity_sev8,obesity_sev9,obesity_sev10,
                           weight_gain_a4,weight_gain_a5,weight_gain_a6,weight_gain_a7,weight_gain_a8,weight_gain_a9,weight_gain_a10,
                           weight_gain_b4,weight_gain_b5,weight_gain_b6,weight_gain_b7,weight_gain_b8,weight_gain_b9,weight_gain_b10))
  
  x1 <- x[which(x$b_educ_2==0 & x$b_educ_3==0),]
  x1 <- subset(x1, select=-c(b_educ_2,b_educ_3))
  x2 <- x[which(x$b_educ_2==1 | x$b_educ_3==1),]
  x2 <- subset(x2, select=-c(b_educ_2))
  
  list(x1,x2)
  
})


######################################################################################
# 7. Save final, analysis-ready dataset
#-------------------------------------------------------------------------------------

saveRDS(imp_primary,file=paste0(workdir,"Data/primary analysis data - 20240827.rds"))
saveRDS(imp_cat,file=paste0(workdir,"Data/categorical analysis data - 20240827.rds"))
saveRDS(imp_sevob,file=paste0(workdir,"Data/severe obesity analysis data - 20240827.rds"))
saveRDS(imp_gaina,file=paste0(workdir,"Data/weight gain a analysis data - 20240827.rds"))
saveRDS(imp_gainb,file=paste0(workdir,"Data/weight gain b analysis data - 20240827.rds"))
saveRDS(imp_strat,file=paste0(workdir,"Data/stratified analysis data - 20250606.rds"))
saveRDS(imp_sens_adj,file=paste0(workdir,"Data/sensitivity analysis data - 20250606.rds"))
