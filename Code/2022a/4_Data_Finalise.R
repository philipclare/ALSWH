######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Finalise data after imputation and format for analysis
## Date: 20 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/Physical activity trajectories/"

libs <- c("plyr","dplyr","ltmle","gtools")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2. Load imputed data
#-----------------------------------------------------------------------------

load(file=paste0(workdir,"Data/imputed data - long form.RData"))

##############################################################################
# 3. Create computed/derived variables
#-----------------------------------------------------------------------------

imp <- lapply(imp,function (x) {
  x$alcliferisk <- ifelse(x$alcfq>10,1,0)
  x$alcepisrisk <- ifelse(x$alcbng>1,1,0)
  x$activity_bin <- ifelse(x$weighted_activity_time>=150,1,0)
  x$activity_bin_sens <- ifelse(x$weighted_activity_time>=75,1,0)
  
  x[,c("alcliferisk","alcepisrisk")] <- lapply(x[,c("alcliferisk","alcepisrisk")], factor, labels=c("No","Yes"))
  x <- subset(x, select = -c(alcfq,alcbng,weighted_activity_time,inarea))
  x
})

##############################################################################
# 3. Reshape to wide, drop unnecessary variables and structure for LTMLE
#-----------------------------------------------------------------------------

seifa_list <- c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")
cens_list <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")

imp <- lapply(imp,function (x) {
  x$censored <- 1
  
  x <- x[order(x$wave),]
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("b_pcsa","b_mcsa","b_gh","b_pf","b_re","b_rp","b_cobcat","b_bp","b_educ","b_mh","b_vt","b_sf",
                         "b_cancer_ever","b_depression_ever","b_anxiety_ever",
                         "censored","activity_bin","activity_bin_sens","marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
                         "cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                         "cesd10","mnstrs","whobmigroup","vegetables","fruit",
                         "alcliferisk","alcepisrisk","smokst",
                         "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"),
               sep = "",
               dir="wide")
  
  x <- subset(x, select=-censored2)
  
  x[,cens_list] <- lapply(x[,cens_list], function (y) {
    y <- ifelse(is.na(y),0,y)
    y <- BinaryToCensoring(is.uncensored=y)
    })
  
  x <- subset(x, select = -c(b_pcsa3,b_mcsa3,b_gh3,b_pf3,b_re3,b_rp3,b_cobcat3,b_bp3,b_educ3,b_mh3,b_vt3,b_sf3,b_cancer_ever3,b_depression_ever3,b_anxiety_ever3,
                             b_pcsa4,b_mcsa4,b_gh4,b_pf4,b_re4,b_rp4,b_cobcat4,b_bp4,b_educ4,b_mh4,b_vt4,b_sf4,b_cancer_ever4,b_depression_ever4,b_anxiety_ever4,
                             b_pcsa5,b_mcsa5,b_gh5,b_pf5,b_re5,b_rp5,b_cobcat5,b_bp5,b_educ5,b_mh5,b_vt5,b_sf5,b_cancer_ever5,b_depression_ever5,b_anxiety_ever5,
                             b_pcsa6,b_mcsa6,b_gh6,b_pf6,b_re6,b_rp6,b_cobcat6,b_bp6,b_educ6,b_mh6,b_vt6,b_sf6,b_cancer_ever6,b_depression_ever6,b_anxiety_ever6,
                             b_pcsa7,b_mcsa7,b_gh7,b_pf7,b_re7,b_rp7,b_cobcat7,b_bp7,b_educ7,b_mh7,b_vt7,b_sf7,b_cancer_ever7,b_depression_ever7,b_anxiety_ever7,
                             b_pcsa8,b_mcsa8,b_gh8,b_pf8,b_re8,b_rp8,b_cobcat8,b_bp8,b_educ8,b_mh8,b_vt8,b_sf8,b_cancer_ever8,b_depression_ever8,b_anxiety_ever8,
                             b_pcsa9,b_mcsa9,b_gh9,b_pf9,b_re9,b_rp9,b_cobcat9,b_bp9,b_educ9,b_mh9,b_vt9,b_sf9,b_cancer_ever9,b_depression_ever9,b_anxiety_ever9))

  x <- x %>%
    rename(b_pcsa = b_pcsa2,
           b_mcsa = b_mcsa2,
           b_gh = b_gh2,
           b_pf = b_pf2,
           b_re = b_re2,
           b_rp = b_rp2,
           b_cobcat = b_cobcat2,
           b_bp = b_bp2,
           b_educ = b_educ2,
           b_mh = b_mh2,
           b_vt = b_vt2,
           b_sf = b_sf2,
           b_cancer_ever = b_cancer_ever2,
           b_depression_ever = b_depression_ever2,
           b_anxiety_ever = b_anxiety_ever2)

  x[,seifa_list] <- lapply(x[,seifa_list], quantcut, q=3)

  x <- subset(x, select = -c(activity_bin2,activity_bin_sens2,
                             pcsa2,mcsa2,pf2,rp2,bp2,gh2,vt2,sf2,re2,mh2,
                             pcsa3,mcsa3,pf3,rp3,bp3,gh3,vt3,sf3,re3,mh3,
                             pcsa4,mcsa4,pf4,rp4,bp4,gh4,vt4,sf4,re4,mh4,
                             pcsa5,mcsa5,pf5,rp5,bp5,gh5,vt5,sf5,re5,mh5,
                             pcsa6,mcsa6,pf6,rp6,bp6,gh6,vt6,sf6,re6,mh6,
                             pcsa7,mcsa7,pf7,rp7,bp7,gh7,vt7,sf7,re7,mh7,
                             marital8,age8,ariapgp8,employ8,seifadis8,live_u188,live_o188,
                             cancer_3yr8,arthritis_3yr8,depression_3yr8,anxiety_3yr8,
                             cesd108,mnstrs8,whobmigroup8,vegetables8,fruit8,alcliferisk8,alcepisrisk8,smokst8,
                             pcsa8,mcsa8,pf8,rp8,bp8,gh8,vt8,sf8,re8,mh8,
                             marital9,age9,ariapgp9,employ9,seifadis9,live_u189,live_o189,
                             cancer_3yr9,arthritis_3yr9,depression_3yr9,anxiety_3yr9,
                             cesd109,mnstrs9,whobmigroup9,vegetables9,fruit9,alcliferisk9,alcepisrisk9,smokst9,activity_bin9))
  
  x[,c("marital2","marital3","marital4","marital5","marital6","marital7")] <- lapply(x[,c("marital2","marital3","marital4","marital5","marital6","marital7")], factor)
  x[,c("ariapgp2","ariapgp3","ariapgp4","ariapgp5","ariapgp6","ariapgp7")] <- lapply(x[,c("ariapgp2","ariapgp3","ariapgp4","ariapgp5","ariapgp6","ariapgp7")], factor)
  x[,c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")] <- lapply(x[,c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")], factor)
  x[,c("whobmigroup2","whobmigroup3","whobmigroup4","whobmigroup5","whobmigroup6","whobmigroup7")] <- lapply(x[,c("whobmigroup2","whobmigroup3","whobmigroup4","whobmigroup5","whobmigroup6","whobmigroup7")], factor)
  x[,c("smokst2","smokst3","smokst4","smokst5","smokst6","smokst7")] <- lapply(x[,c("smokst2","smokst3","smokst4","smokst5","smokst6","smokst7")], factor)

  x
})

imp_primary <- lapply(imp, function (x) {
  subset(x, select=-c(activity_bin_sens3,activity_bin_sens4,activity_bin_sens5,activity_bin_sens6,activity_bin_sens7,activity_bin_sens8))
})

imp_sensitivity <- lapply(imp, function (x) {
  x <- subset(x, select=-c(activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens3,
           activity_bin4 = activity_bin_sens4,
           activity_bin5 = activity_bin_sens5,
           activity_bin6 = activity_bin_sens6,
           activity_bin7 = activity_bin_sens7,
           activity_bin8 = activity_bin_sens8)
})

##############################################################################
# 4. Save final, analysis-ready dataset
#-----------------------------------------------------------------------------

save(imp_primary,file=paste0(workdir,"Data/primary analysis data - wide form.RData"))
save(imp_sensitivity,file=paste0(workdir,"Data/sensitivity analysis data - wide form.RData"))

