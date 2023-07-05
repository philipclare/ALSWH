######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Finalise data after imputation and format for analysis
## Date: 30 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"

libs <- c("dplyr","fastDummies","gtools","ltmle")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

seifa_list <- c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")
cens_list <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")

######################################################################################
# 2. Load imputed data
#-------------------------------------------------------------------------------------

load(file=paste0(workdir,"Data/imputed data - long form.RData"))

######################################################################################
# 3. Create computed/derived variables
#-------------------------------------------------------------------------------------

imp <- lapply(imp,function (x) {
  
  x$alcliferisk <- ifelse(x$alcfq>10,1,0)
  x$alcepisrisk <- ifelse(x$alcbng>1,1,0)
  x$activity_bin <- ifelse(x$weighted_activity_time>=150,1,0)
  x$activity_bin_sens1 <- ifelse(x$weighted_activity_time>=75,1,0)
  x$activity_bin_sens2 <- ifelse(x$weighted_activity_time>=300,1,0)
  
  x <- subset(x, select = -c(alcfq,alcbng,weighted_activity_time,inarea))
  x
  
})

######################################################################################
# 4. Dummy code factors
#-------------------------------------------------------------------------------------

imp <- lapply(imp,function (x) {
  
  x[,c("b_cobcat","employ","live_u18","live_o18","heartdis_3yr","hypert_3yr","stroke_3yr",
       "cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","vegetables","fruit")] <- lapply(x[,c("b_cobcat","employ","live_u18","live_o18","heartdis_3yr","hypert_3yr","stroke_3yr",
                                                                                                         "cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr","vegetables","fruit")], function (x) {as.numeric(x)-1})
  
  x <- dummy_cols(x, select_columns=c('b_educ','marital','ariapgp','whobmigroup','smokst'),remove_most_frequent_dummy=TRUE)
  colnums <- match(c('b_educ','marital','ariapgp','whobmigroup','smokst') ,names(x))
  x <- x[,c(1:(colnums[1]-1),58:59,(colnums[1]+1):(colnums[2]-1),60:61,(colnums[2]+1):(colnums[3]-1),62:63,(colnums[3]+1):(colnums[4]-1),64:66,(colnums[4]+1):(colnums[5]-1),67:68,(colnums[5]+1):57)]
  
  x <- x %>% 
    dplyr::rename(
      b_educ_2 = 'b_educ_Trade/apprentice/certificate/diploma',
      b_educ_3 = 'b_educ_University',
      marital_2 = 'marital_Separated/divorced/never married',
      marital_3 = 'marital_Widowed',
      ariapgp_2 = 'ariapgp_Major city',
      ariapgp_3 = 'ariapgp_Remote',
      whobmigroup_2 = 'whobmigroup_Obese',
      whobmigroup_3 = 'whobmigroup_Overweight',
      whobmigroup_4 = 'whobmigroup_Underweight',
      smokst_2 = 'smokst_Current smoker',
      smokst_3 = 'smokst_Ex smoker')
  
  x
  
})

######################################################################################
# 5. Reshape to wide, drop unnecessary variables and structure for LTMLE
#-------------------------------------------------------------------------------------

imp <- lapply(imp,function (x) {
  
  x$censored <- 1
  
  x <- x[order(x$wave),]
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c(
                         "censored","activity_bin","activity_bin_sens1","activity_bin_sens2","marital_2","marital_3","age","ariapgp_2","ariapgp_3","employ","seifadis","live_u18","live_o18",
                         "heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                         "cesd10","mnstrs","whobmigroup_2","whobmigroup_3","whobmigroup_4","vegetables","fruit",
                         "alcliferisk","alcepisrisk","smokst_2","smokst_3",
                         "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"),
               sep = "",
               dir="wide")
  
  x <- subset(x, select=-censored2)
  
  x[,cens_list] <- lapply(x[,cens_list], function (y) {
    y <- ifelse(is.na(y),0,y)
    y <- BinaryToCensoring(is.uncensored=y)
    })
  
  x <- subset(x, select = -c(heartdis_3yr2,hypert_3yr2,stroke_3yr2,cancer_3yr2,arthritis_3yr2,depression_3yr2,anxiety_3yr2))
  x[,seifa_list] <- lapply(x[,seifa_list], quantcut, q=3)
  x[,seifa_list] <- lapply(x[,seifa_list], function (y) {
    levels(y) <- c("lowest","middle","highest")
    y
  })

  x <- dummy_cols(x, select_columns=seifa_list,ignore_na=TRUE,remove_first_dummy=TRUE)
  colnums <- match(seifa_list ,names(x))
  x <- x[,c(1:(colnums[1]-1),342:343,(colnums[1]+1):(colnums[2]-1),344:345,(colnums[2]+1):(colnums[3]-1),346:347,(colnums[3]+1):(colnums[4]-1),348:349,(colnums[4]+1):(colnums[5]-1),350:351,(colnums[5]+1):(colnums[6]-1),352:353,(colnums[6]+1):341)]

  x <- x %>% 
    dplyr::rename(
      seifadis_22 = 'seifadis2_middle',
      seifadis_32 = 'seifadis2_highest',
      seifadis_23 = 'seifadis3_middle',
      seifadis_33 = 'seifadis3_highest',
      seifadis_24 = 'seifadis4_middle',
      seifadis_34 = 'seifadis4_highest',
      seifadis_25 = 'seifadis5_middle',
      seifadis_35 = 'seifadis5_highest',
      seifadis_26 = 'seifadis6_middle',
      seifadis_36 = 'seifadis6_highest',
      seifadis_27 = 'seifadis7_middle',
      seifadis_37 = 'seifadis7_highest')
  
  x <- subset(x, select = -c(b_pcsa,b_mcsa,b_gh,b_pf,b_re,b_rp,b_bp,b_mh,b_vt,b_sf,
                             activity_bin2,activity_bin_sens12,activity_bin_sens22,
                             marital_28,marital_38,age8,ariapgp_28,ariapgp_38,employ8,seifadis8,live_u188,live_o188,
                             heartdis_3yr8,hypert_3yr8,stroke_3yr8,cancer_3yr8,arthritis_3yr8,depression_3yr8,anxiety_3yr8,
                             cesd108,mnstrs8,whobmigroup_28,whobmigroup_38,whobmigroup_48,vegetables8,fruit8,alcliferisk8,alcepisrisk8,smokst_28,smokst_38,
                             pcsa8,mcsa8,pf8,rp8,bp8,gh8,vt8,sf8,re8,mh8,
                             marital_29,marital_39,age9,ariapgp_29,ariapgp_39,employ9,seifadis9,live_u189,live_o189,
                             heartdis_3yr9,hypert_3yr9,stroke_3yr9,cancer_3yr9,arthritis_3yr9,depression_3yr9,anxiety_3yr9,
                             cesd109,mnstrs9,whobmigroup_29,whobmigroup_39,whobmigroup_49,vegetables9,fruit9,alcliferisk9,alcepisrisk9,smokst_29,smokst_39,
                             activity_bin9,activity_bin_sens19,activity_bin_sens29))
   x

})

######################################################################################
# 6. Create analysis-specific data for primary, s1, s2
#-------------------------------------------------------------------------------------

imp_primary <- lapply(imp, function (x) {
  
  subset(x, select=-c(activity_bin_sens13,activity_bin_sens14,activity_bin_sens15,activity_bin_sens16,activity_bin_sens17,activity_bin_sens18,
                      activity_bin_sens23,activity_bin_sens24,activity_bin_sens25,activity_bin_sens26,activity_bin_sens27,activity_bin_sens28))
  
})

imp_sensitivity_1 <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens23,activity_bin_sens24,activity_bin_sens25,activity_bin_sens26,activity_bin_sens27,activity_bin_sens28))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens13,
           activity_bin4 = activity_bin_sens14,
           activity_bin5 = activity_bin_sens15,
           activity_bin6 = activity_bin_sens16,
           activity_bin7 = activity_bin_sens17,
           activity_bin8 = activity_bin_sens18)
  
})

imp_sensitivity_2 <- lapply(imp, function (x) {
  
  x <- subset(x, select=-c(activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens13,activity_bin_sens14,activity_bin_sens15,activity_bin_sens16,activity_bin_sens17,activity_bin_sens18))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens23,
           activity_bin4 = activity_bin_sens24,
           activity_bin5 = activity_bin_sens25,
           activity_bin6 = activity_bin_sens26,
           activity_bin7 = activity_bin_sens27,
           activity_bin8 = activity_bin_sens28)
  
})

raw_data <- imp_primary[[41]]
imp_primary <- imp_primary[1:40]
imp_sensitivity_1 <- imp_sensitivity_1[1:40]
imp_sensitivity_2 <- imp_sensitivity_2[1:40]

######################################################################################
# 7. Save final, analysis-ready dataset
#-------------------------------------------------------------------------------------

save(raw_data,file=paste0(workdir,"Data/raw data for table 1 - wide form 20230605.RData"))
save(imp_primary,file=paste0(workdir,"Data/primary analysis data - wide form 20230605.RData"))
save(imp_sensitivity_1,file=paste0(workdir,"Data/sensitivity analysis 1 data - wide form 20230605.RData"))
save(imp_sensitivity_2,file=paste0(workdir,"Data/sensitivity analysis 2 data - wide form 20230605.RData"))
