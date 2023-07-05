######################################################################################
##   
## Effects of physical activity on mortality
## Finalise data after imputation and format for analysis
## Date: 19 December 2022
## OSF Registration: https://osf.io/pytzx
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list=ls()) # clean global environment
workdir <- "//surefsn025/ProfileR025$/philipclare/Documents/ALSWH/"

libs <- c("plyr","dplyr","ltmle","gtools")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

restructure_censor <- function (x,cens_list,out_list) {
  
  x.names <- colnames(x)
  x[,cens_list] <- lapply(x[,cens_list], function (y) {
    y <- as.numeric(y)-1
    y <- ifelse(is.na(y),0,y)
  })
  
  x[,out_list] <- lapply(x[,out_list], function (y) {
    y <- 1-y
    y <- ifelse(is.na(y),0,y)
  })
  
  ynodes <- x[,out_list]
  
  types <- do.call(cbind,lapply(x,class))
  for (i in rev(sort(c(grep("censored", colnames(x)),grep("death", colnames(x)))))) {
    x <- as.data.frame(t(apply(x, 1, function (z,i) {
      if (z[i]=="0") {
        z <- c(z[1:i],rep(NA,length(z)-i))
      } else {
        z
      }
    },i=i)))
  }
  colnames(x) <- x.names
  
  x$death5 <- ifelse(is.na(x$death5) & (is.na(x$censored5) | x$censored5==1) & (is.na(x$censored4) | x$censored4==1) & (is.na(x$censored3) | x$censored3==1),ynodes$death5,x$death5)
  x$death6 <- ifelse(is.na(x$death6) & (is.na(x$censored6) | x$censored6==1) & (is.na(x$censored5) | x$censored5==1) & (is.na(x$censored4) | x$censored4==1) & (is.na(x$censored3) | x$censored3==1),ynodes$death6,x$death6)
  x$death7 <- ifelse(is.na(x$death7) & (is.na(x$censored7) | x$censored7==1) & (is.na(x$censored6) | x$censored6==1) & (is.na(x$censored5) | x$censored5==1) & (is.na(x$censored4) | x$censored4==1) & (is.na(x$censored3) | x$censored3==1),ynodes$death7,x$death7)
  x$death8 <- ifelse(is.na(x$death8) & (is.na(x$censored8) | x$censored8==1) & (is.na(x$censored7) | x$censored7==1) & (is.na(x$censored6) | x$censored6==1) & (is.na(x$censored5) | x$censored5==1) & (is.na(x$censored4) | x$censored4==1) & (is.na(x$censored3) | x$censored3==1),ynodes$death8,x$death8)
  x$death9 <- ifelse(is.na(x$death9) & (is.na(x$censored9) | x$censored9==1) & (is.na(x$censored8) | x$censored8==1) & (is.na(x$censored7) | x$censored7==1) & (is.na(x$censored6) | x$censored6==1) & (is.na(x$censored5) | x$censored5==1) & (is.na(x$censored4) | x$censored4==1) & (is.na(x$censored3) | x$censored3==1),ynodes$death9,x$death9)
  
  x[,out_list] <- lapply(x[,out_list], function (y) {
    ifelse(y==0,1,0)
  })
  
  for (a in 1:length(types)) {
    if (types[a]=="numeric") {
      x[,a] <- as.numeric(x[,a])
    }
    if (types[a]=="factor") {
      x[,a] <- factor(x[,a])
    }
  }
  
  x[,cens_list] <- lapply(x[,cens_list], function (y) {
    y <- BinaryToCensoring(is.uncensored=y)
  })
  x
  
}

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
  x$activity_bin_sens_1 <- ifelse(x$weighted_activity_time>=75,1,0)
  x$activity_bin_sens_2 <- ifelse(x$weighted_activity_time>=300,1,0)
  
  x[,c("alcliferisk","alcepisrisk")] <- lapply(x[,c("alcliferisk","alcepisrisk")], factor, labels=c("No","Yes"))
  x <- subset(x, select = -c(alcfq,alcbng,weighted_activity_time,inarea))
  x <- x[which(x$wave>1),]
  x
})

######################################################################################
# 4. Reshape to wide, drop unnecessary variables and structure for LTMLE
#-------------------------------------------------------------------------------------

bnodes <- c("death4","death5","death6","death7","death8","death9",
            "CVD_death4","CVD_death5","CVD_death6","CVD_death7","CVD_death8","CVD_death9",
            "cancer_death4","cancer_death5","cancer_death6","cancer_death7","cancer_death8","cancer_death9")
seifa_list <- c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")
cens_list <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")
out_list <- c("death4","death5","death6","death7","death8","death9")

imp <- lapply(imp,function (x) {
  x <- x[order(x$wave),]
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("b_pcsa","b_mcsa","b_gh","b_pf","b_re","b_rp","b_cobcat","b_bp","b_educ","b_mh","b_vt","b_sf",
                         "b_cancer_ever","b_depression_ever","b_anxiety_ever",
                         "censored","death","CVD_censor","CVD_death","cancer_censor","cancer_death",
                         "activity_bin","activity_bin_sens_1","activity_bin_sens_2",
                         "age","marital","ariapgp","employ","seifadis","live_u18","live_o18",
                         "cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                         "cesd10","mnstrs","whobmigroup","vegetables","fruit",
                         "alcliferisk","alcepisrisk","smokst",
                         "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"),
               sep = "",
               dir="wide")
  
  x[,bnodes] <- lapply(x[,bnodes], function (y) {
    y <- as.numeric(y)-1
    
  })
  
  x <- subset(x, select = -c(cancer_3yr2,arthritis_3yr2,depression_3yr2,anxiety_3yr2,
                             b_pcsa3,b_mcsa3,b_gh3,b_pf3,b_re3,b_rp3,b_cobcat3,b_bp3,b_educ3,b_mh3,b_vt3,b_sf3,b_cancer_ever3,b_depression_ever3,b_anxiety_ever3,
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

  x <- subset(x, select = -c(activity_bin2,activity_bin_sens_12,activity_bin_sens_22,
                             censored2,death2,CVD_censor2,CVD_death2,cancer_censor2,cancer_death2,
                             pf3,rp3,bp3,gh3,vt3,sf3,re3,mh3,death3,CVD_death3,cancer_death3,
                             pf4,rp4,bp4,gh4,vt4,sf4,re4,mh4,
                             pf5,rp5,bp5,gh5,vt5,sf5,re5,mh5,
                             pf6,rp6,bp6,gh6,vt6,sf6,re6,mh6,
                             pf7,rp7,bp7,gh7,vt7,sf7,re7,mh7,
                             marital8,age8,ariapgp8,employ8,seifadis8,live_u188,live_o188,
                             cancer_3yr8,arthritis_3yr8,depression_3yr8,anxiety_3yr8,
                             cesd108,mnstrs8,whobmigroup8,vegetables8,fruit8,alcliferisk8,alcepisrisk8,smokst8,
                             pcsa8,mcsa8,pf8,rp8,bp8,gh8,vt8,sf8,re8,mh8,
                             marital9,age9,ariapgp9,employ9,seifadis9,live_u189,live_o189,
                             cancer_3yr9,arthritis_3yr9,depression_3yr9,anxiety_3yr9,
                             cesd109,mnstrs9,whobmigroup9,vegetables9,fruit9,alcliferisk9,alcepisrisk9,smokst9,activity_bin9,activity_bin_sens_19,activity_bin_sens_29,
                             pcsa9,mcsa9,pf9,rp9,bp9,gh9,vt9,sf9,re9,mh9))
  
  x[,c("marital2","marital3","marital4","marital5","marital6","marital7")] <- lapply(x[,c("marital2","marital3","marital4","marital5","marital6","marital7")], factor)
  x[,c("ariapgp2","ariapgp3","ariapgp4","ariapgp5","ariapgp6","ariapgp7")] <- lapply(x[,c("ariapgp2","ariapgp3","ariapgp4","ariapgp5","ariapgp6","ariapgp7")], factor)
  x[,c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")] <- lapply(x[,c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")], factor)
  x[,c("whobmigroup2","whobmigroup3","whobmigroup4","whobmigroup5","whobmigroup6","whobmigroup7")] <- lapply(x[,c("whobmigroup2","whobmigroup3","whobmigroup4","whobmigroup5","whobmigroup6","whobmigroup7")], factor)
  x[,c("smokst2","smokst3","smokst4","smokst5","smokst6","smokst7")] <- lapply(x[,c("smokst2","smokst3","smokst4","smokst5","smokst6","smokst7")], factor)

  x
})

ac_p <- lapply(imp, function (x) {
  x <- subset(x, select=-c(CVD_censor3,CVD_censor4,CVD_censor5,CVD_censor6,CVD_censor7,CVD_censor8,CVD_censor9,
                           CVD_death4,CVD_death5,CVD_death6,CVD_death7,CVD_death8,CVD_death9,
                           cancer_censor3,cancer_censor4,cancer_censor5,cancer_censor6,cancer_censor7,cancer_censor8,cancer_censor9,
                           cancer_death4,cancer_death5,cancer_death6,cancer_death7,cancer_death8,cancer_death9,
                           activity_bin_sens_13,activity_bin_sens_14,activity_bin_sens_15,activity_bin_sens_16,activity_bin_sens_17,activity_bin_sens_18,
                           activity_bin_sens_23,activity_bin_sens_24,activity_bin_sens_25,activity_bin_sens_26,activity_bin_sens_27,activity_bin_sens_28))
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

ac_s1 <- lapply(imp, function (x) {
  x <- subset(x, select=-c(CVD_censor3,CVD_censor4,CVD_censor5,CVD_censor6,CVD_censor7,CVD_censor8,CVD_censor9,
                           CVD_death4,CVD_death5,CVD_death6,CVD_death7,CVD_death8,CVD_death9,
                           cancer_censor3,cancer_censor4,cancer_censor5,cancer_censor6,cancer_censor7,cancer_censor8,cancer_censor9,
                           cancer_death4,cancer_death5,cancer_death6,cancer_death7,cancer_death8,cancer_death9,
                           activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens_23,activity_bin_sens_24,activity_bin_sens_25,activity_bin_sens_26,activity_bin_sens_27,activity_bin_sens_28))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens_13,
           activity_bin4 = activity_bin_sens_14,
           activity_bin5 = activity_bin_sens_15,
           activity_bin6 = activity_bin_sens_16,
           activity_bin7 = activity_bin_sens_17,
           activity_bin8 = activity_bin_sens_18)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

ac_s2 <- lapply(imp, function (x) {
  x <- subset(x, select=-c(CVD_censor3,CVD_censor4,CVD_censor5,CVD_censor6,CVD_censor7,CVD_censor8,CVD_censor9,
                           CVD_death4,CVD_death5,CVD_death6,CVD_death7,CVD_death8,CVD_death9,
                           cancer_censor3,cancer_censor4,cancer_censor5,cancer_censor6,cancer_censor7,cancer_censor8,cancer_censor9,
                           cancer_death4,cancer_death5,cancer_death6,cancer_death7,cancer_death8,cancer_death9,
                           activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens_13,activity_bin_sens_14,activity_bin_sens_15,activity_bin_sens_16,activity_bin_sens_17,activity_bin_sens_18))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens_23,
           activity_bin4 = activity_bin_sens_24,
           activity_bin5 = activity_bin_sens_25,
           activity_bin6 = activity_bin_sens_26,
           activity_bin7 = activity_bin_sens_27,
           activity_bin8 = activity_bin_sens_28)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

cvd_p <- lapply(imp, function (x) {
  x <- subset(x, select=-c(censored3,censored4,censored5,censored6,censored7,censored8,censored9,
                      death4,death5,death6,death7,death8,death9,
                      cancer_censor3,cancer_censor4,cancer_censor5,cancer_censor6,cancer_censor7,cancer_censor8,cancer_censor9,
                      cancer_death4,cancer_death5,cancer_death6,cancer_death7,cancer_death8,cancer_death9,
                      activity_bin_sens_13,activity_bin_sens_14,activity_bin_sens_15,activity_bin_sens_16,activity_bin_sens_17,activity_bin_sens_18,
                      activity_bin_sens_23,activity_bin_sens_24,activity_bin_sens_25,activity_bin_sens_26,activity_bin_sens_27,activity_bin_sens_28))

  x <- x %>%
    rename(death4 = CVD_death4,
           death5 = CVD_death5,
           death6 = CVD_death6,
           death7 = CVD_death7,
           death8 = CVD_death8,
           death9 = CVD_death9,
           censored3 = CVD_censor3,
           censored4 = CVD_censor4,
           censored5 = CVD_censor5,
           censored6 = CVD_censor6,
           censored7 = CVD_censor7,
           censored8 = CVD_censor8,
           censored9 = CVD_censor9)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

cvd_s1 <- lapply(imp, function (x) {
  x <- subset(x, select=-c(censored3,censored4,censored5,censored6,censored7,censored8,censored9,
                           death4,death5,death6,death7,death8,death9,
                           cancer_censor3,cancer_censor4,cancer_censor5,cancer_censor6,cancer_censor7,cancer_censor8,cancer_censor9,
                           cancer_death4,cancer_death5,cancer_death6,cancer_death7,cancer_death8,cancer_death9,
                           activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens_23,activity_bin_sens_24,activity_bin_sens_25,activity_bin_sens_26,activity_bin_sens_27,activity_bin_sens_28))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens_13,
           activity_bin4 = activity_bin_sens_14,
           activity_bin5 = activity_bin_sens_15,
           activity_bin6 = activity_bin_sens_16,
           activity_bin7 = activity_bin_sens_17,
           activity_bin8 = activity_bin_sens_18,
           death4 = CVD_death4,
           death5 = CVD_death5,
           death6 = CVD_death6,
           death7 = CVD_death7,
           death8 = CVD_death8,
           death9 = CVD_death9,
           censored3 = CVD_censor3,
           censored4 = CVD_censor4,
           censored5 = CVD_censor5,
           censored6 = CVD_censor6,
           censored7 = CVD_censor7,
           censored8 = CVD_censor8,
           censored9 = CVD_censor9)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

cvd_s2 <- lapply(imp, function (x) {
  x <- subset(x, select=-c(censored3,censored4,censored5,censored6,censored7,censored8,censored9,
                           death4,death5,death6,death7,death8,death9,
                           cancer_censor3,cancer_censor4,cancer_censor5,cancer_censor6,cancer_censor7,cancer_censor8,cancer_censor9,
                           cancer_death4,cancer_death5,cancer_death6,cancer_death7,cancer_death8,cancer_death9,
                           activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens_13,activity_bin_sens_14,activity_bin_sens_15,activity_bin_sens_16,activity_bin_sens_17,activity_bin_sens_18))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens_23,
           activity_bin4 = activity_bin_sens_24,
           activity_bin5 = activity_bin_sens_25,
           activity_bin6 = activity_bin_sens_26,
           activity_bin7 = activity_bin_sens_27,
           activity_bin8 = activity_bin_sens_28,
           death4 = CVD_death4,
           death5 = CVD_death5,
           death6 = CVD_death6,
           death7 = CVD_death7,
           death8 = CVD_death8,
           death9 = CVD_death9,
           censored3 = CVD_censor3,
           censored4 = CVD_censor4,
           censored5 = CVD_censor5,
           censored6 = CVD_censor6,
           censored7 = CVD_censor7,
           censored8 = CVD_censor8,
           censored9 = CVD_censor9)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

cancer_p <- lapply(imp, function (x) {
  x <- subset(x, select=-c(CVD_censor3,CVD_censor4,CVD_censor5,CVD_censor6,CVD_censor7,CVD_censor8,CVD_censor9,
                      CVD_death4,CVD_death5,CVD_death6,CVD_death7,CVD_death8,CVD_death9,
                      censored3,censored4,censored5,censored6,censored7,censored8,censored9,
                      death4,death5,death6,death7,death8,death9,
                      activity_bin_sens_13,activity_bin_sens_14,activity_bin_sens_15,activity_bin_sens_16,activity_bin_sens_17,activity_bin_sens_18,
                      activity_bin_sens_23,activity_bin_sens_24,activity_bin_sens_25,activity_bin_sens_26,activity_bin_sens_27,activity_bin_sens_28))
  
  x <- x %>% 
    rename(death4 = cancer_death4,
           death5 = cancer_death5,
           death6 = cancer_death6,
           death7 = cancer_death7,
           death8 = cancer_death8,
           death9 = cancer_death9,
           censored3 = cancer_censor3,
           censored4 = cancer_censor4,
           censored5 = cancer_censor5,
           censored6 = cancer_censor6,
           censored7 = cancer_censor7,
           censored8 = cancer_censor8,
           censored9 = cancer_censor9)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

cancer_s1 <- lapply(imp, function (x) {
  x <- subset(x, select=-c(CVD_censor3,CVD_censor4,CVD_censor5,CVD_censor6,CVD_censor7,CVD_censor8,CVD_censor9,
                           CVD_death4,CVD_death5,CVD_death6,CVD_death7,CVD_death8,CVD_death9,
                           censored3,censored4,censored5,censored6,censored7,censored8,censored9,
                           death4,death5,death6,death7,death8,death9,
                           activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens_23,activity_bin_sens_24,activity_bin_sens_25,activity_bin_sens_26,activity_bin_sens_27,activity_bin_sens_28))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens_13,
           activity_bin4 = activity_bin_sens_14,
           activity_bin5 = activity_bin_sens_15,
           activity_bin6 = activity_bin_sens_16,
           activity_bin7 = activity_bin_sens_17,
           activity_bin8 = activity_bin_sens_18,
           death4 = cancer_death4,
           death5 = cancer_death5,
           death6 = cancer_death6,
           death7 = cancer_death7,
           death8 = cancer_death8,
           death9 = cancer_death9,
           censored3 = cancer_censor3,
           censored4 = cancer_censor4,
           censored5 = cancer_censor5,
           censored6 = cancer_censor6,
           censored7 = cancer_censor7,
           censored8 = cancer_censor8,
           censored9 = cancer_censor9)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

cancer_s2 <- lapply(imp, function (x) {
  x <- subset(x, select=-c(CVD_censor3,CVD_censor4,CVD_censor5,CVD_censor6,CVD_censor7,CVD_censor8,CVD_censor9,
                           CVD_death4,CVD_death5,CVD_death6,CVD_death7,CVD_death8,CVD_death9,
                           censored3,censored4,censored5,censored6,censored7,censored8,censored9,
                           death4,death5,death6,death7,death8,death9,
                           activity_bin3,activity_bin4,activity_bin5,activity_bin6,activity_bin7,activity_bin8,
                           activity_bin_sens_13,activity_bin_sens_14,activity_bin_sens_15,activity_bin_sens_16,activity_bin_sens_17,activity_bin_sens_18))
  x <- x %>% 
    rename(activity_bin3 = activity_bin_sens_23,
           activity_bin4 = activity_bin_sens_24,
           activity_bin5 = activity_bin_sens_25,
           activity_bin6 = activity_bin_sens_26,
           activity_bin7 = activity_bin_sens_27,
           activity_bin8 = activity_bin_sens_28,
           death4 = cancer_death4,
           death5 = cancer_death5,
           death6 = cancer_death6,
           death7 = cancer_death7,
           death8 = cancer_death8,
           death9 = cancer_death9,
           censored3 = cancer_censor3,
           censored4 = cancer_censor4,
           censored5 = cancer_censor5,
           censored6 = cancer_censor6,
           censored7 = cancer_censor7,
           censored8 = cancer_censor8,
           censored9 = cancer_censor9)
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

######################################################################################
# 5. Save final, analysis-ready dataset
#-------------------------------------------------------------------------------------

save(ac_p,file=paste0(workdir,"Data/all cause analysis - primary.RData"))
save(ac_s1,file=paste0(workdir,"Data/all cause analysis - s1.RData"))
save(ac_s2,file=paste0(workdir,"Data/all cause analysis - s2.RData"))

save(cvd_p,file=paste0(workdir,"Data/CVD analysis - primary.RData"))
save(cvd_s1,file=paste0(workdir,"Data/CVD analysis - s1.RData"))
save(cvd_s2,file=paste0(workdir,"Data/CVD analysis - s2.RData"))

save(cancer_p,file=paste0(workdir,"Data/cancer analysis - primary.RData"))
save(cancer_s1,file=paste0(workdir,"Data/cancer analysis - s1.RData"))
save(cancer_s2,file=paste0(workdir,"Data/cancer analysis - s2.RData"))
