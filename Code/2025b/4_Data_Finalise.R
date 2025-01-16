######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Finalise data after imputation and format for analysis
## Date: 24 July 2023
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------
rm(list=ls()) # clean global environment
# workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"
workdir <- "/Volumes/research-data/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"

libs <- c("dplyr","fastDummies","gtools","ltmle","plyr")
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
    if (types[a]=="numeric" | types[a]=="integer") {
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

imp <- lapply(seq(1,30), function (x) {
  data.frame(do.call(rbind,readRDS(paste0(workdir,"Data/HPC Imputations/Post-hoc/imputed data-",x,".rds"))))
})
load(paste0(workdir,"Data/imputation data.RData"))
imp[[31]] <- data.frame(imp_data)
rm(imp_data)

######################################################################################
# 3. Create computed/derived variables
#-------------------------------------------------------------------------------------

imp <- lapply(imp,function (x) {
  x$alcliferisk <- ifelse(x$alcfq>10,1,0)
  x$alcepisrisk <- ifelse(x$alcbng>1,1,0)
  
  x[,c("alcliferisk","alcepisrisk")] <- lapply(x[,c("alcliferisk","alcepisrisk")], factor, labels=c("No","Yes"))
  x <- subset(x, select = -c(alcfq,alcbng,b_inarea))
  x <- x[which(x$wave>1),]
  x
})

######################################################################################
# 4. Reshape to wide, drop unnecessary variables and structure for LTMLE
#-------------------------------------------------------------------------------------

seifa_list <- c("seifadis2","seifadis3","seifadis4","seifadis5","seifadis6","seifadis7")
cens_list <- c("censored3","censored4","censored5","censored6","censored7","censored8","censored9")
out_list <- c("death4","death5","death6","death7","death8","death9")
exp_list <- c("lonely_binary3","lonely_binary4","lonely_binary5","lonely_binary6","lonely_binary7","lonely_binary8")
l_list <- c("employ","live_alone","alcliferisk","alcepisrisk","depression_3yr","anxiety_3yr")

imp_p <- lapply(imp,function (x) {
  x <- x[order(x$wave),]
  
  x$lonely_binary <- ifelse(as.numeric(x$lonely_category) >= 3, 1,0)
  
  x <- subset(x, select = -c(lonely_category,mos_short))
  
  levels(x$mstat) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='mstat',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$ariapgp) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='ariapgp',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$whobmigroup) <- c("1","2","3","4")
  x <- dummy_cols(x, select_columns='whobmigroup',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$smokst) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='smokst',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)

  x[,l_list] <- lapply(x[,l_list], function (y) {
    y <- as.numeric(y)-1
  })

  # Drop cases with  with history of major illness
  x <- x[!(x$b_heartdis_ever=="Yes"),]
  x <- x[!(x$b_stroke_ever=="Yes"),]
  x <- x[!(x$b_cancer_ever=="Yes"),]
  
  # removing unnecessary variables 
  x <- subset(x, select = -c(b_heartdis_ever, b_stroke_ever, b_cancer_ever))
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("b_educ","b_wtarea","b_language","b_country","b_depression_ever","b_anxiety_ever",
                         "censored","death",
                         "lonely_binary",
                         "age","mstat_2","mstat_3","ariapgp_1","ariapgp_3","employ","seifadis","mnstrs","whobmigroup_1","whobmigroup_3","whobmigroup_4",
                         "alcliferisk","alcepisrisk","smokst_2","smokst_3","depression_3yr","anxiety_3yr",
                         "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh",
                         "mos_long","live_alone"),
               sep = "",
               dir="wide")
  
  x[,out_list] <- lapply(x[,out_list], function (y) {
    y <- as.numeric(y)-1
  })

  x <- subset(x, select = -c(depression_3yr2,anxiety_3yr2,
                             b_depression_ever3,b_anxiety_ever3,
                             b_depression_ever4,b_anxiety_ever4,
                             b_depression_ever5,b_anxiety_ever5,
                             b_depression_ever6,b_anxiety_ever6,
                             b_depression_ever7,b_anxiety_ever7, 
                             b_depression_ever8,b_anxiety_ever8, 
                             b_depression_ever9,b_anxiety_ever9))
   
  x <- x %>%
    dplyr::rename(wtarea = b_wtarea2,
           b_country = b_country2,
           b_educ = b_educ2,
           b_language = b_language2,
           b_depression_ever = b_depression_ever2,
           b_anxiety_ever = b_anxiety_ever2)
  
  x[,seifa_list] <- lapply(x[,seifa_list], function (y) {
    y <- quantcut(y, q=3)
    levels(y) <- c("1","2","3")
    y
  })
  x <- dummy_cols(x, select_columns='seifadis2',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis3',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis4',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis5',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis6',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis7',remove_first_dummy = TRUE)
  x <- x %>%
    dplyr::rename(seifadis_22 = seifadis2_2,
           seifadis_32 = seifadis2_3,
           seifadis_23 = seifadis3_2,
           seifadis_33 = seifadis3_3,
           seifadis_24 = seifadis4_2,
           seifadis_34 = seifadis4_3,
           seifadis_25 = seifadis5_2,
           seifadis_35 = seifadis5_3,
           seifadis_26 = seifadis6_2,
           seifadis_36 = seifadis6_3,
           seifadis_27 = seifadis7_2,
           seifadis_37 = seifadis7_3)
  
  x <- subset(x, select = c(idproj,wtarea,
                            b_country,b_educ,b_language,b_depression_ever,b_anxiety_ever,
                                                            mos_long2,age2,mstat_22,mstat_32,ariapgp_12,ariapgp_32,employ2,seifadis_22,seifadis_32,mnstrs2,whobmigroup_12,whobmigroup_32,whobmigroup_42,live_alone2,alcliferisk2,alcepisrisk2,smokst_22,smokst_32,pcsa2,mcsa2,gh2,pf2,rp2,bp2,vt2,re2,mh2,sf2,
                            censored3,       lonely_binary3,mos_long3,age3,mstat_23,mstat_33,ariapgp_13,ariapgp_33,employ3,seifadis_23,seifadis_33,mnstrs3,whobmigroup_13,whobmigroup_33,whobmigroup_43,live_alone3,alcliferisk3,alcepisrisk3,smokst_23,smokst_33,pcsa3,mcsa3,gh3,pf3,rp3,bp3,vt3,re3,mh3,sf3,depression_3yr3,anxiety_3yr3,
                            censored4,death4,lonely_binary4,mos_long4,age4,mstat_24,mstat_34,ariapgp_14,ariapgp_34,employ4,seifadis_24,seifadis_34,mnstrs4,whobmigroup_14,whobmigroup_34,whobmigroup_44,live_alone4,alcliferisk4,alcepisrisk4,smokst_24,smokst_34,pcsa4,mcsa4,gh4,pf4,rp4,bp4,vt4,re4,mh4,sf4,depression_3yr4,anxiety_3yr4,
                            censored5,death5,lonely_binary5,mos_long5,age5,mstat_25,mstat_35,ariapgp_15,ariapgp_35,employ5,seifadis_25,seifadis_35,mnstrs5,whobmigroup_15,whobmigroup_35,whobmigroup_45,live_alone5,alcliferisk5,alcepisrisk5,smokst_25,smokst_35,pcsa5,mcsa5,gh5,pf5,rp5,bp5,vt5,re5,mh5,sf5,depression_3yr5,anxiety_3yr5,
                            censored6,death6,lonely_binary6,mos_long6,age6,mstat_26,mstat_36,ariapgp_16,ariapgp_36,employ6,seifadis_26,seifadis_36,mnstrs6,whobmigroup_16,whobmigroup_36,whobmigroup_46,live_alone6,alcliferisk6,alcepisrisk6,smokst_26,smokst_36,pcsa6,mcsa6,gh6,pf6,rp6,bp6,vt6,re6,mh6,sf6,depression_3yr6,anxiety_3yr6,
                            censored7,death7,lonely_binary7,mos_long7,age7,mstat_27,mstat_37,ariapgp_17,ariapgp_37,employ7,seifadis_27,seifadis_37,mnstrs7,whobmigroup_17,whobmigroup_37,whobmigroup_47,live_alone7,alcliferisk7,alcepisrisk7,smokst_27,smokst_37,pcsa7,mcsa7,gh7,pf7,rp7,bp7,vt7,re7,mh7,sf7,depression_3yr7,anxiety_3yr7,
                            censored8,death8,lonely_binary8,
                            censored9,death9))
  
  x
}) 

ac_p <- lapply(imp_p, function (x) {
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

imp_s1 <- lapply(imp,function (x) {
  x <- x[order(x$wave),]
  
  x$lonely_binary <- ifelse(as.numeric(x$lonely_category) >= 2, 1,0)
  
  x <- subset(x, select = -c(lonely_category,mos_short))

  levels(x$mstat) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='mstat',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$ariapgp) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='ariapgp',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$whobmigroup) <- c("1","2","3","4")
  x <- dummy_cols(x, select_columns='whobmigroup',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$smokst) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='smokst',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  
  x[,l_list] <- lapply(x[,l_list], function (y) {
    y <- as.numeric(y)-1
  })
  
  # Drop cases with  with history of major illness
  x <- x[!(x$b_heartdis_ever=="Yes"),]
  x <- x[!(x$b_stroke_ever=="Yes"),]
  x <- x[!(x$b_cancer_ever=="Yes"),]
  
  # removing unnecessary variables 
  x <- subset(x, select = -c(b_heartdis_ever, b_stroke_ever, b_cancer_ever))
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("b_educ","b_wtarea","b_language","b_country","b_depression_ever","b_anxiety_ever",
                         "censored","death",
                         "lonely_binary",
                         "age","mstat_2","mstat_3","ariapgp_1","ariapgp_3","employ","seifadis","mnstrs","whobmigroup_1","whobmigroup_3","whobmigroup_4",
                         "alcliferisk","alcepisrisk","smokst_2","smokst_3","depression_3yr","anxiety_3yr",
                         "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh",
                         "mos_long","live_alone"),
               sep = "",
               dir="wide")
  
  x[,out_list] <- lapply(x[,out_list], function (y) {
    y <- as.numeric(y)-1
  })

  x <- subset(x, select = -c(depression_3yr2,anxiety_3yr2,
                             b_depression_ever3,b_anxiety_ever3,
                             b_depression_ever4,b_anxiety_ever4, 
                             b_depression_ever5,b_anxiety_ever5, 
                             b_depression_ever6,b_anxiety_ever6, 
                             b_depression_ever7,b_anxiety_ever7, 
                             b_depression_ever8,b_anxiety_ever8, 
                             b_depression_ever9,b_anxiety_ever9))
  
  x <- x %>%
    dplyr::rename(wtarea = b_wtarea2,
           b_country = b_country2,
           b_educ = b_educ2,
           b_language = b_language2,
           b_depression_ever = b_depression_ever2,
           b_anxiety_ever = b_anxiety_ever2)
  
  x[,seifa_list] <- lapply(x[,seifa_list], function (y) {
    y <- quantcut(y, q=3)
    levels(y) <- c("1","2","3")
    y
  })
  x <- dummy_cols(x, select_columns='seifadis2',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis3',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis4',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis5',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis6',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis7',remove_first_dummy = TRUE)
  x <- x %>%
    dplyr::rename(seifadis_22 = seifadis2_2,
           seifadis_32 = seifadis2_3,
           seifadis_23 = seifadis3_2,
           seifadis_33 = seifadis3_3,
           seifadis_24 = seifadis4_2,
           seifadis_34 = seifadis4_3,
           seifadis_25 = seifadis5_2,
           seifadis_35 = seifadis5_3,
           seifadis_26 = seifadis6_2,
           seifadis_36 = seifadis6_3,
           seifadis_27 = seifadis7_2,
           seifadis_37 = seifadis7_3)
  
  x <- subset(x, select = c(idproj,wtarea,
                            b_country,b_educ,b_language,b_depression_ever,b_anxiety_ever,
                            mos_long2,age2,mstat_22,mstat_32,ariapgp_12,ariapgp_32,employ2,seifadis_22,seifadis_32,mnstrs2,whobmigroup_12,whobmigroup_32,whobmigroup_42,live_alone2,alcliferisk2,alcepisrisk2,smokst_22,smokst_32,pcsa2,mcsa2,gh2,pf2,rp2,bp2,vt2,re2,mh2,sf2,
                            censored3,       lonely_binary3,mos_long3,age3,mstat_23,mstat_33,ariapgp_13,ariapgp_33,employ3,seifadis_23,seifadis_33,mnstrs3,whobmigroup_13,whobmigroup_33,whobmigroup_43,live_alone3,alcliferisk3,alcepisrisk3,smokst_23,smokst_33,pcsa3,mcsa3,gh3,pf3,rp3,bp3,vt3,re3,mh3,sf3,depression_3yr3,anxiety_3yr3,
                            censored4,death4,lonely_binary4,mos_long4,age4,mstat_24,mstat_34,ariapgp_14,ariapgp_34,employ4,seifadis_24,seifadis_34,mnstrs4,whobmigroup_14,whobmigroup_34,whobmigroup_44,live_alone4,alcliferisk4,alcepisrisk4,smokst_24,smokst_34,pcsa4,mcsa4,gh4,pf4,rp4,bp4,vt4,re4,mh4,sf4,depression_3yr4,anxiety_3yr4,
                            censored5,death5,lonely_binary5,mos_long5,age5,mstat_25,mstat_35,ariapgp_15,ariapgp_35,employ5,seifadis_25,seifadis_35,mnstrs5,whobmigroup_15,whobmigroup_35,whobmigroup_45,live_alone5,alcliferisk5,alcepisrisk5,smokst_25,smokst_35,pcsa5,mcsa5,gh5,pf5,rp5,bp5,vt5,re5,mh5,sf5,depression_3yr5,anxiety_3yr5,
                            censored6,death6,lonely_binary6,mos_long6,age6,mstat_26,mstat_36,ariapgp_16,ariapgp_36,employ6,seifadis_26,seifadis_36,mnstrs6,whobmigroup_16,whobmigroup_36,whobmigroup_46,live_alone6,alcliferisk6,alcepisrisk6,smokst_26,smokst_36,pcsa6,mcsa6,gh6,pf6,rp6,bp6,vt6,re6,mh6,sf6,depression_3yr6,anxiety_3yr6,
                            censored7,death7,lonely_binary7,mos_long7,age7,mstat_27,mstat_37,ariapgp_17,ariapgp_37,employ7,seifadis_27,seifadis_37,mnstrs7,whobmigroup_17,whobmigroup_37,whobmigroup_47,live_alone7,alcliferisk7,alcepisrisk7,smokst_27,smokst_37,pcsa7,mcsa7,gh7,pf7,rp7,bp7,vt7,re7,mh7,sf7,depression_3yr7,anxiety_3yr7,
                            censored8,death8,lonely_binary8,
                            censored9,death9))
  
  x
}) 

ac_s1 <- lapply(imp_s1, function (x) {
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

imp_s2 <- lapply(imp,function (x) {
  x <- x[order(x$wave),]
  
  x$lonely_binary <- ifelse(as.numeric(x$lonely_category) >= 4, 1,0)
  
  x <- subset(x, select = -c(lonely_category,mos_short))
  
  levels(x$mstat) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='mstat',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$ariapgp) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='ariapgp',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$whobmigroup) <- c("1","2","3","4")
  x <- dummy_cols(x, select_columns='whobmigroup',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$smokst) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='smokst',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  
  x[,l_list] <- lapply(x[,l_list], function (y) {
    y <- as.numeric(y)-1
  })
  
  # Drop cases with  with history of major illness
  x <- x[!(x$b_heartdis_ever=="Yes"),]
  x <- x[!(x$b_stroke_ever=="Yes"),]
  x <- x[!(x$b_cancer_ever=="Yes"),]
  
  # removing unnecessary variables 
  x <- subset(x, select = -c(b_heartdis_ever, b_stroke_ever, b_cancer_ever))
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("b_educ","b_wtarea","b_language","b_country","b_depression_ever","b_anxiety_ever",
                         "censored","death",
                         "lonely_binary",
                         "age","mstat_2","mstat_3","ariapgp_1","ariapgp_3","employ","seifadis","mnstrs","whobmigroup_1","whobmigroup_3","whobmigroup_4",
                         "alcliferisk","alcepisrisk","smokst_2","smokst_3","depression_3yr","anxiety_3yr",
                         "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh",
                         "mos_long","live_alone"),
               sep = "",
               dir="wide")
  
  x[,out_list] <- lapply(x[,out_list], function (y) {
    y <- as.numeric(y)-1
  })

  x <- subset(x, select = -c(depression_3yr2,anxiety_3yr2,
                             b_depression_ever3,b_anxiety_ever3, 
                             b_depression_ever4,b_anxiety_ever4, 
                             b_depression_ever5,b_anxiety_ever5, 
                             b_depression_ever6,b_anxiety_ever6, 
                             b_depression_ever7,b_anxiety_ever7, 
                             b_depression_ever8,b_anxiety_ever8, 
                             b_depression_ever9,b_anxiety_ever9))
  
  x <- x %>%
    dplyr::rename(wtarea = b_wtarea2,
           b_country = b_country2,
           b_educ = b_educ2,
           b_language = b_language2,
           b_depression_ever = b_depression_ever2,
           b_anxiety_ever = b_anxiety_ever2)
  
  x[,seifa_list] <- lapply(x[,seifa_list], function (y) {
    y <- quantcut(y, q=3)
    levels(y) <- c("1","2","3")
    y
  })
  x <- dummy_cols(x, select_columns='seifadis2',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis3',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis4',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis5',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis6',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis7',remove_first_dummy = TRUE)
  x <- x %>%
    dplyr::rename(seifadis_22 = seifadis2_2,
           seifadis_32 = seifadis2_3,
           seifadis_23 = seifadis3_2,
           seifadis_33 = seifadis3_3,
           seifadis_24 = seifadis4_2,
           seifadis_34 = seifadis4_3,
           seifadis_25 = seifadis5_2,
           seifadis_35 = seifadis5_3,
           seifadis_26 = seifadis6_2,
           seifadis_36 = seifadis6_3,
           seifadis_27 = seifadis7_2,
           seifadis_37 = seifadis7_3)
  
  x <- subset(x, select = c(idproj,wtarea,
                            b_country,b_educ,b_language,b_depression_ever,b_anxiety_ever,
                            mos_long2,age2,mstat_22,mstat_32,ariapgp_12,ariapgp_32,employ2,seifadis_22,seifadis_32,mnstrs2,whobmigroup_12,whobmigroup_32,whobmigroup_42,live_alone2,alcliferisk2,alcepisrisk2,smokst_22,smokst_32,pcsa2,mcsa2,gh2,pf2,rp2,bp2,vt2,re2,mh2,sf2,
                            censored3,       lonely_binary3,mos_long3,age3,mstat_23,mstat_33,ariapgp_13,ariapgp_33,employ3,seifadis_23,seifadis_33,mnstrs3,whobmigroup_13,whobmigroup_33,whobmigroup_43,live_alone3,alcliferisk3,alcepisrisk3,smokst_23,smokst_33,pcsa3,mcsa3,gh3,pf3,rp3,bp3,vt3,re3,mh3,sf3,depression_3yr3,anxiety_3yr3,
                            censored4,death4,lonely_binary4,mos_long4,age4,mstat_24,mstat_34,ariapgp_14,ariapgp_34,employ4,seifadis_24,seifadis_34,mnstrs4,whobmigroup_14,whobmigroup_34,whobmigroup_44,live_alone4,alcliferisk4,alcepisrisk4,smokst_24,smokst_34,pcsa4,mcsa4,gh4,pf4,rp4,bp4,vt4,re4,mh4,sf4,depression_3yr4,anxiety_3yr4,
                            censored5,death5,lonely_binary5,mos_long5,age5,mstat_25,mstat_35,ariapgp_15,ariapgp_35,employ5,seifadis_25,seifadis_35,mnstrs5,whobmigroup_15,whobmigroup_35,whobmigroup_45,live_alone5,alcliferisk5,alcepisrisk5,smokst_25,smokst_35,pcsa5,mcsa5,gh5,pf5,rp5,bp5,vt5,re5,mh5,sf5,depression_3yr5,anxiety_3yr5,
                            censored6,death6,lonely_binary6,mos_long6,age6,mstat_26,mstat_36,ariapgp_16,ariapgp_36,employ6,seifadis_26,seifadis_36,mnstrs6,whobmigroup_16,whobmigroup_36,whobmigroup_46,live_alone6,alcliferisk6,alcepisrisk6,smokst_26,smokst_36,pcsa6,mcsa6,gh6,pf6,rp6,bp6,vt6,re6,mh6,sf6,depression_3yr6,anxiety_3yr6,
                            censored7,death7,lonely_binary7,mos_long7,age7,mstat_27,mstat_37,ariapgp_17,ariapgp_37,employ7,seifadis_27,seifadis_37,mnstrs7,whobmigroup_17,whobmigroup_37,whobmigroup_47,live_alone7,alcliferisk7,alcepisrisk7,smokst_27,smokst_37,pcsa7,mcsa7,gh7,pf7,rp7,bp7,vt7,re7,mh7,sf7,depression_3yr7,anxiety_3yr7,
                            censored8,death8,lonely_binary8,
                            censored9,death9))
  
  x
}) 

ac_s2 <- lapply(imp_s2, function (x) {
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

imp_s3 <- lapply(imp,function (x) {
  x <- x[order(x$wave),]
  
  x$lonely_binary <- ifelse(as.numeric(x$lonely_category) >= 3, 1,0)
  
  x <- subset(x, select = -c(lonely_category,mos_short))
  
  levels(x$mstat) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='mstat',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$ariapgp) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='ariapgp',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$whobmigroup) <- c("1","2","3","4")
  x <- dummy_cols(x, select_columns='whobmigroup',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  levels(x$smokst) <- c("1","2","3")
  x <- dummy_cols(x, select_columns='smokst',remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  
  x[,l_list] <- lapply(x[,l_list], function (y) {
    y <- as.numeric(y)-1
  })
  
  x <- reshape(x,
               timevar=c("wave"), 
               idvar=c("idproj"),
               v.names=c("b_educ","b_wtarea","b_language","b_country","b_depression_ever","b_anxiety_ever","b_heartdis_ever", "b_stroke_ever", "b_cancer_ever", 
                         "censored","death",
                         "lonely_binary",
                         "age","mstat_2","mstat_3","ariapgp_1","ariapgp_3","employ","seifadis","mnstrs","whobmigroup_1","whobmigroup_3","whobmigroup_4",
                         "alcliferisk","alcepisrisk","smokst_2","smokst_3","depression_3yr","anxiety_3yr",
                         "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh",
                         "mos_long","live_alone"),
               sep = "",
               dir="wide")
  
  x[,out_list] <- lapply(x[,out_list], function (y) {
    y <- as.numeric(y)-1
  })
  
  x <- subset(x, select = -c(depression_3yr2,anxiety_3yr2,
                             b_depression_ever3,b_anxiety_ever3,b_heartdis_ever3,b_stroke_ever3,b_cancer_ever3, 
                             b_depression_ever4,b_anxiety_ever4,b_heartdis_ever4,b_stroke_ever4,b_cancer_ever4, 
                             b_depression_ever5,b_anxiety_ever5,b_heartdis_ever5,b_stroke_ever5,b_cancer_ever5, 
                             b_depression_ever6,b_anxiety_ever6,b_heartdis_ever6,b_stroke_ever6,b_cancer_ever6, 
                             b_depression_ever7,b_anxiety_ever7,b_heartdis_ever7,b_stroke_ever7,b_cancer_ever7, 
                             b_depression_ever8,b_anxiety_ever8,b_heartdis_ever8,b_stroke_ever8,b_cancer_ever8, 
                             b_depression_ever9,b_anxiety_ever9,b_heartdis_ever9,b_stroke_ever9,b_cancer_ever9))
  
  x <- x %>%
    dplyr::rename(wtarea = b_wtarea2,
                  b_country = b_country2,
                  b_educ = b_educ2,
                  b_language = b_language2,
                  b_depression_ever = b_depression_ever2,
                  b_anxiety_ever = b_anxiety_ever2,
                  b_heartdis_ever = b_heartdis_ever2,
                  b_stroke_ever = b_stroke_ever2,
                  b_cancer_ever = b_cancer_ever2)
  
  x[,seifa_list] <- lapply(x[,seifa_list], function (y) {
    y <- quantcut(y, q=3)
    levels(y) <- c("1","2","3")
    y
  })
  x <- dummy_cols(x, select_columns='seifadis2',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis3',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis4',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis5',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis6',remove_first_dummy = TRUE)
  x <- dummy_cols(x, select_columns='seifadis7',remove_first_dummy = TRUE)
  x <- x %>%
    dplyr::rename(seifadis_22 = seifadis2_2,
                  seifadis_32 = seifadis2_3,
                  seifadis_23 = seifadis3_2,
                  seifadis_33 = seifadis3_3,
                  seifadis_24 = seifadis4_2,
                  seifadis_34 = seifadis4_3,
                  seifadis_25 = seifadis5_2,
                  seifadis_35 = seifadis5_3,
                  seifadis_26 = seifadis6_2,
                  seifadis_36 = seifadis6_3,
                  seifadis_27 = seifadis7_2,
                  seifadis_37 = seifadis7_3)
  
  x <- subset(x, select = c(idproj,wtarea,
                            b_country,b_educ,b_language,b_depression_ever,b_anxiety_ever,b_heartdis_ever,b_stroke_ever,b_cancer_ever,
                            mos_long2,age2,mstat_22,mstat_32,ariapgp_12,ariapgp_32,employ2,seifadis_22,seifadis_32,mnstrs2,whobmigroup_12,whobmigroup_32,whobmigroup_42,live_alone2,alcliferisk2,alcepisrisk2,smokst_22,smokst_32,pcsa2,mcsa2,gh2,pf2,rp2,bp2,vt2,re2,mh2,sf2,
                            censored3,       lonely_binary3,mos_long3,age3,mstat_23,mstat_33,ariapgp_13,ariapgp_33,employ3,seifadis_23,seifadis_33,mnstrs3,whobmigroup_13,whobmigroup_33,whobmigroup_43,live_alone3,alcliferisk3,alcepisrisk3,smokst_23,smokst_33,pcsa3,mcsa3,gh3,pf3,rp3,bp3,vt3,re3,mh3,sf3,depression_3yr3,anxiety_3yr3,
                            censored4,death4,lonely_binary4,mos_long4,age4,mstat_24,mstat_34,ariapgp_14,ariapgp_34,employ4,seifadis_24,seifadis_34,mnstrs4,whobmigroup_14,whobmigroup_34,whobmigroup_44,live_alone4,alcliferisk4,alcepisrisk4,smokst_24,smokst_34,pcsa4,mcsa4,gh4,pf4,rp4,bp4,vt4,re4,mh4,sf4,depression_3yr4,anxiety_3yr4,
                            censored5,death5,lonely_binary5,mos_long5,age5,mstat_25,mstat_35,ariapgp_15,ariapgp_35,employ5,seifadis_25,seifadis_35,mnstrs5,whobmigroup_15,whobmigroup_35,whobmigroup_45,live_alone5,alcliferisk5,alcepisrisk5,smokst_25,smokst_35,pcsa5,mcsa5,gh5,pf5,rp5,bp5,vt5,re5,mh5,sf5,depression_3yr5,anxiety_3yr5,
                            censored6,death6,lonely_binary6,mos_long6,age6,mstat_26,mstat_36,ariapgp_16,ariapgp_36,employ6,seifadis_26,seifadis_36,mnstrs6,whobmigroup_16,whobmigroup_36,whobmigroup_46,live_alone6,alcliferisk6,alcepisrisk6,smokst_26,smokst_36,pcsa6,mcsa6,gh6,pf6,rp6,bp6,vt6,re6,mh6,sf6,depression_3yr6,anxiety_3yr6,
                            censored7,death7,lonely_binary7,mos_long7,age7,mstat_27,mstat_37,ariapgp_17,ariapgp_37,employ7,seifadis_27,seifadis_37,mnstrs7,whobmigroup_17,whobmigroup_37,whobmigroup_47,live_alone7,alcliferisk7,alcepisrisk7,smokst_27,smokst_37,pcsa7,mcsa7,gh7,pf7,rp7,bp7,vt7,re7,mh7,sf7,depression_3yr7,anxiety_3yr7,
                            censored8,death8,lonely_binary8,
                            censored9,death9))
  
  x
}) 

ac_s3 <- lapply(imp_s3, function (x) {
  
  x <- restructure_censor(x=x,cens_list=cens_list,out_list=out_list)
  
})

######################################################################################
# 5. Save final, analysis-ready datasets
#-------------------------------------------------------------------------------------

saveRDS(ac_p,file=paste0(workdir,"Data/all cause analysis - pr.rds"))
saveRDS(ac_s1,file=paste0(workdir,"Data/all cause analysis - s1.rds"))
saveRDS(ac_s2,file=paste0(workdir,"Data/all cause analysis - s2.rds"))
saveRDS(ac_s3,file=paste0(workdir,"Data/all cause analysis - s3.rds"))
