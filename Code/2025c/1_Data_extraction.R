######################################################################################
##   
## Effects of physical activity on incident obesity
## Extract key variables from separate wave datasets
## Date: 6 June 2025
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
  workdir <- "Y:/PRJ-prc_alswh/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/" # MAC
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("haven","dplyr","tidyr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Wave 1 Data
#-------------------------------------------------------------------------------------

w1data <- read_dta(paste0(workdir,"Data/w1Mbf.dta"))
w1data <- zap_label(w1data)
w1data <- zap_labels(w1data)
w1data <- zap_formats(w1data)

w1data <- w1data %>% 
  rename_all(~stringr::str_replace(.,"^m1","b_"))
w1data <- subset(w1data, select = c(idproj,inarea,b_wtarea,b_htcm,b_wtkg,
                                    b_cobcat,b_q90,b_pcsa,b_mcsa,b_pf))

w1data <- w1data %>% 
  rename(b_educ = b_q90)

w1data <- w1data %>% mutate(b_educ = case_match(b_educ, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1,
                                            5 ~ 1,
                                            6 ~ 2,
                                            7 ~ 2))
w1data$b_educ <- factor(w1data$b_educ,labels=c("Less than high school","Trade/apprentice/certificate/diploma","University"))
w1data <- w1data %>% mutate(b_cobcat = case_match(b_cobcat, 
                                            1 ~ 0,
                                            2 ~ 1,
                                            3 ~ 1,
                                            4 ~ 1,
                                            5 ~ 1))
w1data$b_cobcat <- factor(w1data$b_cobcat,labels=c("Australia","Other"))

saveRDS(w1data,file=paste0(workdir,"Paper 3 - Obesity/Data/w1 bmi.rds"))

######################################################################################
# 3. Wave 2 Data
#-------------------------------------------------------------------------------------

w2data <- read_dta(paste0(workdir,"Data/w2Mbf.dta"))
w2data <- zap_label(w2data)
w2data <- zap_labels(w2data)
w2data <- zap_formats(w2data)

names(w2data) <- tolower(names(w2data))
w2data$wave <- 2
w2data <- w2data %>% 
  rename_all(~stringr::str_replace(.,"^m2",""))

w2data <- subset(w2data, select = c(idproj,wave,htcm,wtkg,
                                    age,q54mn,q54sc,ariapgp,idis96_deciles,marital,whobmigroup,bmi,q72,q74b,q74c,
                                    q74d,smokst,q39,q40,q41,mnstrs,cesd10,menstatgp,q29ay,
                                    q20c,q20d,q20e,q20k,q20l,q20m,q20n,q20o,q20p,q20q,pcsa,mcsa,metmin))

w2data <- w2data %>% 
  rename(seifadis = idis96_deciles,
         finfinc = q72,
         b_metmin = metmin,
         sleep_cat = q29ay)

w2data$b_heartdis_ever <- ifelse(w2data$q20c>=1,
                            1,0)
w2data$b_hypert_ever <- ifelse(w2data$q20d>=1,
                            1,0)
w2data$b_stroke_ever <- ifelse(w2data$q20e>=1,
                            1,0)
w2data$b_cancer_ever <- ifelse(w2data$q20k>=1 | w2data$q20l>=1 | w2data$q20m>=1 | w2data$q20n>=1 | w2data$q20o>=1,
                            1,0)
w2data$b_depression_ever <- ifelse(w2data$q20p>=1,
                               1,0)
w2data$b_anxiety_ever <- ifelse(w2data$q20q>=1,
                               1,0)

w2data$q54sc <- ifelse(is.na(w2data$q54sc),0,w2data$q54sc)
w2data$employ <- ifelse(((w2data$q54mn<1 | w2data$q54mn>3) & (w2data$q54sc<1 | w2data$q54sc>3)),
              0,1)
w2data$employ <- factor(w2data$employ,labels=c("Not employed","Employed"))

w2data$live_u18 <- ifelse(w2data$q74b>1 | w2data$q74c>1,
                          1,0)
w2data$live_o18 <- ifelse(w2data$q74d>1,
                          1,0)

w2data <- w2data %>% 
  rename(alcfre = q39,
         alcqnt = q40,
         alcbng = q41)
w2data <- w2data %>% mutate(alcfre = case_match(alcfre, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1.5,
                                            5 ~ 3.5,
                                            6 ~ 5.5,
                                            7 ~ 7))

w2data <- w2data %>% mutate(alcqnt = case_match(alcqnt, 
                                            1 ~ 1.5,
                                            2 ~ 3.5,
                                            3 ~ 6.5,
                                            4 ~ 9))

w2data <- subset(w2data, select = -c(q20c,q20d,q20e,q20k,q20l,q20m,q20n,q20o,q20p,q20q,q54mn,q54sc,q74b,q74c,q74d))

w2datab <- subset(w2data, select = c(idproj,b_heartdis_ever,b_hypert_ever,b_stroke_ever,b_cancer_ever,b_depression_ever,b_anxiety_ever,b_metmin))
w2datat <- subset(w2data, select = -c(b_heartdis_ever,b_hypert_ever,b_stroke_ever,b_cancer_ever,b_depression_ever,b_anxiety_ever,b_metmin))

saveRDS(w2datab,file=paste0(workdir,"Paper 3 - Obesity/Data/w2b bmi.rds"))
saveRDS(w2datat,file=paste0(workdir,"Paper 3 - Obesity/Data/w2t bmi.rds"))

######################################################################################
# 4. Wave 3 Data
#-------------------------------------------------------------------------------------

w3data <- read_dta(paste0(workdir,"Data/w3Mbf.dta"))
w3data <- zap_label(w3data)
w3data <- zap_labels(w3data)
w3data <- zap_formats(w3data)

names(w3data) <- tolower(names(w3data))
w3data$wave <- 3
w3data <- w3data %>% 
  rename_all(~stringr::str_replace(.,"^m3",""))

w3data <- subset(w3data, select = c(idproj,wave,htcm,wtkg,
                                    age,q71a,q71b,q71c,q71e,ariapgp,idis96_deciles,marital,whobmigroup,bmi,q90c,q90d,
                                    q90e,smokst,mnstrs,cesd10,q69ah,q69am,q69bh,q69bm,q69ch,q69cm,q69dh,q69dm,
                                    pcsa,mcsa,q89,menstatgp,q37aw,
                                    q35a,q35e,q35f,q35g,q35m,q35n,q35o,q35p,q35q,q35r,q35y))
w3data$wave <- 3

w3data <- w3data %>% 
  rename(seifadis = idis96_deciles,
         finfinc = q89,
         sleep_cat = q37aw)

w3data$heartdis_3yr <- ifelse(w3data$q35e>=1,
                                 1,0)
w3data$hypert_3yr <- ifelse(w3data$q35f>=1,
                               1,0)
w3data$stroke_3yr <- ifelse(w3data$q35g>=1,
                               1,0)  
w3data$cancer_3yr <- ifelse(w3data$q35m==1 | w3data$q35n==1 | w3data$q35o==1 | w3data$q35p==1,
                            1,0)
w3data <- w3data %>% 
  rename(depression_3yr = q35q,
         anxiety_3yr = q35r,
         arthritis_3yr = q35a)

w3data$employ <- ifelse(w3data$q71a>1 | w3data$q71b>1 | w3data$q71c>1 | w3data$q71e>1,
                        1,0)
w3data$employ <- factor(w3data$employ,labels=c("Not employed","Employed"))

w3data$live_u18 <- ifelse(w3data$q90c>1 | w3data$q90d>1,
                          1,0)
w3data$live_o18 <- ifelse(w3data$q90e>1,
                          1,0)

w3data$walking_time <- w3data$q69ah*60 + w3data$q69am
w3data$moderate_time <- w3data$q69bh*60 + w3data$q69bm
w3data$vig_leis_time <- w3data$q69ch*60 + w3data$q69cm
w3data$vig_hous_time <- w3data$q69dh*60 + w3data$q69dm

w3data <- subset(w3data, select = -c(q35e,q35f,q35g,q35m,q35n,q35o,q35p,q35y,
                                     q69ah,q69am,q69bh,q69bm,q69ch,q69cm,q69dh,q69dm,
                                     q71a,q71b,q71c,q71e,
                                     q90c,q90d,q90e))

saveRDS(w3data,file=paste0(workdir,"Paper 3 - Obesity/Data/w3 bmi.rds"))

######################################################################################
# 5. Wave 4 Data
#-------------------------------------------------------------------------------------

w4data <- read_dta(paste0(workdir,"Data/w4Mbf.dta"))
w4data <- zap_label(w4data)
w4data <- zap_labels(w4data)
w4data <- zap_formats(w4data)

names(w4data) <- tolower(names(w4data))
w4data$wave <- 4
w4data <- w4data %>% 
  rename_all(~stringr::str_replace(.,"^m4",""))

w4data <- subset(w4data, select = c(idproj,wave,htcm,wtkg,
                                    age,q68a,q68c,ariapgp,sd01_deciles,marital,whobmigroup,bmi,q89c,q89d,q89e,q63,q62,
                                    smokst,q59,q60,q61,mnstrs,cesd10,q67ah,q67am,q67bh,q67bm,q67ch,q67cm,q67dh,q67dm,
                                    pcsa,mcsa,q79,menstatgp,q33a,q33b,q33c,q33d,q33e,
                                    q32a,q32d,q32e,q32f,q32l,q32m,q32n,q32o,q32p,q32q,q32r,q32w,
                                    q13a,q20a,q20b))

w4data$heartdis_3yr <- ifelse(w4data$q32d>=1,
                              1,0)
w4data$hypert_3yr <- ifelse(w4data$q32e>=1,
                            1,0)
w4data$stroke_3yr <- ifelse(w4data$q32f>=1,
                            1,0)  
w4data$cancer_3yr <- ifelse(w4data$q32l==1 | w4data$q32m==1 | w4data$q32n==1 | w4data$q32o==1 | w4data$q32p==1,
                            1,0)
w4data <- w4data %>% 
  rename(depression_3yr = q32q,
         anxiety_3yr = q32r,
         arthritis_3yr = q32a,
         finfinc = q79,
         seifadis = sd01_deciles,
         dentist = q13a,
         pap = q20a,
         mam = q20b)

w4data <- w4data %>% mutate(pap = case_match(pap, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w4data <- w4data %>% mutate(mam = case_match(mam, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w4data$sleep_prob <- w4data$q33a + w4data$q33b + w4data$q33c + w4data$q33d + w4data$q33e

w4data$employ <- ifelse(w4data$q68a>1 | w4data$q68c>1,
                        1,0)
w4data$employ <- factor(w4data$employ,labels=c("Not employed","Employed"))

w4data$live_u18 <- ifelse(w4data$q89c>1 | w4data$q89d>1,
                          1,0)
w4data$live_o18 <- ifelse(w4data$q89e>1,
                          1,0)

w4data <- w4data %>% 
  rename(alcfre = q59,
         alcqnt = q60,
         alcbng = q61)

w4data <- w4data %>% mutate(alcfre = case_match(alcfre, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1.5,
                                            5 ~ 3.5,
                                            6 ~ 5.5,
                                            7 ~ 7))

w4data <- w4data %>% mutate(alcqnt = case_match(alcqnt, 
                                            1 ~ 1.5,
                                            2 ~ 3.5,
                                            3 ~ 6.5,
                                            4 ~ 9))

w4data <- w4data %>% 
  rename(fruit = q63,
         vegetables = q62)

w4data <- w4data %>% mutate(fruit = case_match(fruit, 
                                           0 ~ 0,
                                           1 ~ 0,
                                           2 ~ 1,
                                           3 ~ 1,
                                           4 ~ 1))

w4data <- w4data %>% mutate(vegetables = case_match(vegetables, 
                                                0 ~ 0,
                                                1 ~ 0,
                                                2 ~ 0,
                                                3 ~ 1,
                                                4 ~ 1))

w4data$walking_time <- w4data$q67ah*60 + w4data$q67am
w4data$moderate_time <- w4data$q67bh*60 + w4data$q67bm
w4data$vig_leis_time <- w4data$q67ch*60 + w4data$q67cm
w4data$vig_hous_time <- w4data$q67dh*60 + w4data$q67dm

w4data <- subset(w4data, select = -c(q32d,q32e,q32f,q32l,q32m,q32n,q32o,q32p,q32w,
                                     q33a,q33b,q33c,q33d,q33e,
                                     q67ah,q67am,q67bh,q67bm,q67ch,q67cm,q67dh,q67dm,
                                     q68a,q68c,
                                     q89c,q89d,q89e))

saveRDS(w4data,file=paste0(workdir,"Paper 3 - Obesity/Data/w4 bmi.rds"))

######################################################################################
# 6. Wave 5 Data
#-------------------------------------------------------------------------------------

w5data <- read_dta(paste0(workdir,"Data/w5Mbf.dta"))
w5data <- zap_label(w5data)
w5data <- zap_labels(w5data)
w5data <- zap_formats(w5data)

names(w5data) <- tolower(names(w5data))
w5data$wave <- 5
w5data <- w5data %>% 
  rename_all(~stringr::str_replace(.,"^m5",""))

w5data <- subset(w5data, select = c(idproj,wave,htcm,wtkg,
                                    age,q85a,q85b,q85c,q85e,ariapgp,sd06_deciles,marital,whobmigroup,bmi,q107c,q107d,q107e,q64,q65,
                                    smokst,q59,q60,q61,mnstrs,cesd10,q82ah,q82am,q82bh,q82bm,q82ch,q82cm,q82dh,q82dm,
                                    pcsa,mcsa,q95,menstatgp,q41a,q41b,q41c,q41d,q41e,
                                    q38c,q38d,q38e,q38f,q38g,q38h,q38m,q38n,q38o,q38p,q38q,q38r,q38w,
                                    q31,q14a,q20a,q20b))

w5data <- w5data %>% 
  rename(finfinc = q95,
         seifadis = sd06_deciles,
         dentist = q31,
         vitamins = q14a,
         pap = q20a,
         mam = q20b)

w5data$dentist <- ifelse(w5data$dentist==6,
                         1,0)

w5data <- w5data %>% mutate(pap = case_match(pap, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w5data <- w5data %>% mutate(mam = case_match(mam, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w5data$sleep_prob <- w5data$q41a + w5data$q41b + w5data$q41c + w5data$q41d + w5data$q41e

w5data$heartdis_3yr <- ifelse(w5data$q38f>=1,
                              1,0)
w5data$hypert_3yr <- ifelse(w5data$q38g>=1,
                            1,0)
w5data$stroke_3yr <- ifelse(w5data$q38h>=1,
                            1,0)  
w5data$cancer_3yr <- ifelse(w5data$q38m==1 | w5data$q38n==1 | w5data$q38o==1 | w5data$q38p==1,
                            1,0)
w5data <- w5data %>% 
  rename(depression_3yr = q38q,
         anxiety_3yr = q38r)
w5data$arthritis_3yr <- ifelse(w5data$q38c==1 | w5data$q38d==1 | w5data$q38e==1,
                            1,0)

w5data$employ <- ifelse(w5data$q85a>1 | w5data$q85b>1 | w5data$q85c>1 | w5data$q85e>1,
                        1,0)
w5data$employ <- factor(w5data$employ,labels=c("Not employed","Employed"))

w5data$live_u18 <- ifelse(w5data$q107c>1 | w5data$q107d>1,
                          1,0)
w5data$live_o18 <- ifelse(w5data$q107e>1,
                          1,0)

w5data <- w5data %>% 
  rename(alcfre = q59,
         alcqnt = q60,
         alcbng = q61)

w5data <- w5data %>% mutate(alcfre = case_match(alcfre, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1.5,
                                            5 ~ 3.5,
                                            6 ~ 5.5,
                                            7 ~ 7))

w5data <- w5data %>% mutate(alcqnt = case_match(alcqnt, 
                                            1 ~ 1.5,
                                            2 ~ 3.5,
                                            3 ~ 6.5,
                                            4 ~ 9))

w5data <- w5data %>% 
  rename(fruit = q64,
         vegetables = q65)
w5data <- w5data %>% mutate(fruit = case_match(fruit, 
                                           0 ~ 0,
                                           1 ~ 0,
                                           2 ~ 0,
                                           3 ~ 1,
                                           4 ~ 1,
                                           5 ~ 1,
                                           6 ~ 1))

w5data <- w5data %>% mutate(vegetables = case_match(vegetables, 
                                                0 ~ 0,
                                                1 ~ 0,
                                                2 ~ 0,
                                                3 ~ 0,
                                                4 ~ 0,
                                                5 ~ 1,
                                                6 ~ 1))

w5data$walking_time <- w5data$q82ah*60 + w5data$q82am
w5data$moderate_time <- w5data$q82bh*60 + w5data$q82bm
w5data$vig_leis_time <- w5data$q82ch*60 + w5data$q82cm
w5data$vig_hous_time <- w5data$q82dh*60 + w5data$q82dm

w5data <- subset(w5data, select = -c(q38c,q38d,q38e,q38f,q38g,q38h,q38m,q38n,q38o,q38p,q38w,
                                     q41a,q41b,q41c,q41d,q41e,
                                     q82ah,q82am,q82bh,q82bm,q82ch,q82cm,q82dh,q82dm,
                                     q85a,q85b,q85c,q85e,
                                     q107c,q107d,q107e))

saveRDS(w5data,file=paste0(workdir,"Paper 3 - Obesity/Data/w5 bmi.rds"))

######################################################################################
# 7. Wave 6 Data
#-------------------------------------------------------------------------------------

w6data <- read_dta(paste0(workdir,"Data/w6Mbf.dta"))
w6data <- zap_label(w6data)
w6data <- zap_labels(w6data)
w6data <- zap_formats(w6data)

names(w6data) <- tolower(names(w6data))
w6data$wave <- 6
w6data <- w6data %>% 
  rename_all(~stringr::str_replace(.,"^m6",""))

w6data <- subset(w6data, select = c(idproj,wave,htcm,wtkg,
                                    age,q84a,q84b,q84c,q84e,ariapgp,sd06_deciles,marital,whobmigroup,bmi,q111c,q111d,q111e,q64,q65,
                                    smokst,q60,q61,q62,mnstrs,cesd10,q82atotmin,q82btotmin,q82ctotmin,q82dtotmin,q81a,q81b,q81c,q81d,
                                    pcsa,mcsa,q99,menstatgp,q41a,q41b,q41c,q41d,q41e,
                                    q38c,q38d,q38e,q38f,q38g,q38h,q38m,q38n,q38o,q38p,q38q,q38r,q38x,
                                    q31,q14a,q20a,q20b))

w6data <- w6data %>% 
  rename(finfinc = q99,
         seifadis = sd06_deciles,
         dentist = q31,
         vitamins = q14a,
         pap = q20a,
         mam = q20b)

w6data$dentist <- ifelse(w6data$dentist==6,
                         1,0)

w6data <- w6data %>% mutate(pap = case_match(pap, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w6data <- w6data %>% mutate(mam = case_match(mam, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w6data$sleep_prob <- w6data$q41a + w6data$q41b + w6data$q41c + w6data$q41d + w6data$q41e

w6data$heartdis_3yr <- ifelse(w6data$q38f>=1,
                              1,0)
w6data$hypert_3yr <- ifelse(w6data$q38g>=1,
                            1,0)
w6data$stroke_3yr <- ifelse(w6data$q38h>=1,
                            1,0)  
w6data$cancer_3yr <- ifelse(w6data$q38m==1 | w6data$q38n==1 | w6data$q38o==1 | w6data$q38p==1,
                            1,0)
w6data <- w6data %>% 
  rename(depression_3yr = q38q,
         anxiety_3yr = q38r)
w6data$arthritis_3yr <- ifelse(w6data$q38c==1 | w6data$q38d==1 | w6data$q38e==1,
                               1,0)

w6data$employ <- ifelse(w6data$q84a>1 | w6data$q84b>1 | w6data$q84c>1 | w6data$q84e>1,
                        1,0)
w6data$employ <- factor(w6data$employ,labels=c("Not employed","Employed"))

w6data$live_u18 <- ifelse(w6data$q111c>1 | w6data$q111d>1,
                          1,0)
w6data$live_o18 <- ifelse(w6data$q111e>1,
                          1,0)

w6data <- w6data %>% 
  rename(alcfre = q60,
         alcqnt = q61,
         alcbng = q62)

w6data <- w6data %>% mutate(alcfre = case_match(alcfre, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1.5,
                                            5 ~ 3.5,
                                            6 ~ 5.5,
                                            7 ~ 7))

w6data <- w6data %>% mutate(alcqnt = case_match(alcqnt, 
                                            1 ~ 1.5,
                                            2 ~ 3.5,
                                            3 ~ 6.5,
                                            4 ~ 9))

w6data <- w6data %>% 
  rename(fruit = q64,
         vegetables = q65)
w6data <- w6data %>% mutate(fruit = case_match(fruit, 
                                           0 ~ 0,
                                           1 ~ 0,
                                           2 ~ 0,
                                           3 ~ 1,
                                           4 ~ 1,
                                           5 ~ 1,
                                           6 ~ 1))

w6data <- w6data %>% mutate(vegetables = case_match(vegetables, 
                                                0 ~ 0,
                                                1 ~ 0,
                                                2 ~ 0,
                                                3 ~ 0,
                                                4 ~ 0,
                                                5 ~ 1,
                                                6 ~ 1))

w6data <- w6data %>% 
  rename(walking_time = q82atotmin,
         moderate_time = q82btotmin,
         vig_leis_time = q82ctotmin,
         vig_hous_time = q82dtotmin)

w6data$walking_time <- ifelse(w6data$q81a==0,0,w6data$walking_time)
w6data$moderate_time <- ifelse(w6data$q81b==0,0,w6data$moderate_time)
w6data$vig_leis_time <- ifelse(w6data$q81c==0,0,w6data$vig_leis_time)
w6data$vig_hous_time <- ifelse(w6data$q81d==0,0,w6data$vig_hous_time)

w6data <- subset(w6data, select = -c(q38c,q38d,q38e,q38f,q38g,q38h,q38m,q38n,q38o,q38p,q38x,
                                     q41a,q41b,q41c,q41d,q41e,
                                     q81a,q81b,q81c,q81d,
                                     q84a,q84b,q84c,q84e,
                                     q111c,q111d,q111e))

saveRDS(w6data,file=paste0(workdir,"Paper 3 - Obesity/Data/w6 bmi.rds"))

######################################################################################
# 8. Wave 7 Data
#-------------------------------------------------------------------------------------

w7data <- read_dta(paste0(workdir,"Data/w7Mbf.dta"))
w7data <- zap_label(w7data)
w7data <- zap_labels(w7data)
w7data <- zap_formats(w7data)

names(w7data) <- tolower(names(w7data))
w7data$wave <- 7
w7data <- w7data %>% 
  rename_all(~stringr::str_replace(.,"^m7",""))

w7data <- subset(w7data, select = c(idproj,wave,htcm,wtkg,
                                    age,q83a,q83b,q83c,q83e,ariapgp,sd11_deciles,marital,whobmigroup,bmi,q101c,q101d,
                                    smokst,q51,q52,q53,mnstrs,cesd10,q47ahrs,q47amins,q47bhrs,q47bmins,q47chrs,q47cmins,q47dhrs,q47dmins,
                                    pcsa,mcsa,q95,q24,q31a,q31b,q31c,q31d,q31e,
                                    q32d,q32e,q32f,q32h,q32i,q32k,q32l,q32v,q32w,q32x,q32y,q32z,q32aa,q32cc,q32dd,q32bb,q32gg,
                                    q13h,q14a,q20a,q20b))

w7data <- w7data %>% 
  rename(finfinc = q95,
         hrt = q24,
         seifadis = sd11_deciles,
         dentist = q13h,
         vitamins = q14a,
         pap = q20a,
         mam = q20b)

w7data <- w7data %>% mutate(dentist = case_match(dentist, 2 ~ 0))

w7data <- w7data %>% mutate(pap = case_match(pap, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w7data <- w7data %>% mutate(mam = case_match(mam, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w7data$sleep_prob <- w7data$q31a + w7data$q31b + w7data$q31c + w7data$q31d + w7data$q31e

w7data$heartdis_3yr <- ifelse(w7data$q32i>=1,
                              1,0)
w7data$hypert_3yr <- ifelse(w7data$q32k>=1,
                            1,0)
w7data$stroke_3yr <- ifelse(w7data$q32l>=1,
                            1,0)  
w7data$cancer_3yr <- ifelse(w7data$q32v==1 | w7data$q32v==1 | w7data$q32v==1 | w7data$q32v==1 | w7data$q32z==1 | w7data$q32aa==1,
                            1,0)
w7data <- w7data %>% 
  rename(depression_3yr = q32cc,
         anxiety_3yr = q32dd)
w7data$arthritis_3yr <- ifelse(w7data$q32d==1 | w7data$q32e==1 | w7data$q32f==1,
                               1,0)

w7data$employ <- ifelse(w7data$q83a>1 | w7data$q83b>1 | w7data$q83c>1 | w7data$q83e>1,
                        1,0)
w7data$employ <- factor(w7data$employ,labels=c("Not employed","Employed"))

w7data$live_u18 <- ifelse(w7data$q101c>1,
                          1,0)
w7data$live_o18 <- ifelse(w7data$q101d>1,
                          1,0)

w7data <- w7data %>% 
  rename(alcfre = q51,
         alcqnt = q52,
         alcbng = q53)

w7data <- w7data %>% mutate(alcfre = case_match(alcfre, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1.5,
                                            5 ~ 3.5,
                                            6 ~ 5.5,
                                            7 ~ 7))

w7data <- w7data %>% mutate(alcqnt = case_match(alcqnt, 
                                            1 ~ 1.5,
                                            2 ~ 3.5,
                                            3 ~ 6.5,
                                            4 ~ 9))

w7data$walking_time <- w7data$q47ahrs*60 + w7data$q47amins
w7data$moderate_time <- w7data$q47bhrs*60 + w7data$q47bmins
w7data$vig_leis_time <- w7data$q47chrs*60 + w7data$q47cmins
w7data$vig_hous_time <- w7data$q47dhrs*60 + w7data$q47dmins

w7data <- subset(w7data, select = -c(q31a,q31b,q31c,q31d,q31e,
                                     q32d,q32e,q32f,q32h,q32i,q32k,q32l,q32v,q32w,q32x,q32y,q32z,q32aa,q32bb,q32gg,
                                     q47ahrs,q47amins,q47bhrs,q47bmins,q47chrs,q47cmins,q47dhrs,q47dmins,
                                     q83a,q83b,q83c,q83e,
                                     q101c,q101d))

saveRDS(w7data,file=paste0(workdir,"Paper 3 - Obesity/Data/w7 bmi.rds"))

######################################################################################
# 9. Wave 8 Data
#-------------------------------------------------------------------------------------

w8data <- read_dta(paste0(workdir,"Data/w8Mbf.dta"))
w8data <- zap_label(w8data)
w8data <- zap_labels(w8data)
w8data <- zap_formats(w8data)

names(w8data) <- tolower(names(w8data))
w8data$wave <- 8
w8data <- w8data %>% 
  rename_all(~stringr::str_replace(.,"^m8",""))

w8data <- subset(w8data, select = c(idproj,wave,htcm,wtkg,
                                    age,q66a,q66b,q66c,q66e,ariapgp,sd11_deciles,marital,whobmigroup,bmi,q96c,q96d,q55,q56,
                                    smokst,q51,q52,q53,mnstrs,cesd10,q46atotmin,q46btotmin,q46ctotmin,q46dtotmin,
                                    q45a,q45b,q45c,q45d,pcs_abs,mcs_abs,q79,q23,q29a,q29b,q29c,q29d,q29e,
                                    q30d,q30e,q30f,q30h,q30i,q30k,q30l,q30v,q30w,q30x,q30y,q30z,q30aa,q30cc,q30dd,q30bb,q30gg,
                                    q13h,q14a,q20a,q20b))

w8data <- w8data %>% 
  rename(finfinc = q79,
         hrt = q23,
         seifadis = sd11_deciles,
         dentist = q13h,
         vitamins = q14a,
         pap = q20a,
         mam = q20b)

w8data <- w8data %>% mutate(dentist = case_match(dentist, 2 ~ 0))

w8data <- w8data %>% mutate(pap = case_match(pap, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w8data <- w8data %>% mutate(mam = case_match(mam, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w8data$sleep_prob <- w8data$q29a + w8data$q29b + w8data$q29c + w8data$q29d + w8data$q29e

w8data$heartdis_3yr <- ifelse(w8data$q30i>=1,
                              1,0)
w8data$hypert_3yr <- ifelse(w8data$q30k>=1,
                            1,0)
w8data$stroke_3yr <- ifelse(w8data$q30l>=1,
                            1,0)  
w8data$cancer_3yr <- ifelse(w8data$q30v==1 | w8data$q30w==1 | w8data$q30x==1 | w8data$q30y==1 | w8data$q30z==1 | w8data$q30aa==1,
                            1,0)
w8data <- w8data %>% 
  rename(depression_3yr = q30cc,
         anxiety_3yr = q30dd)
w8data$arthritis_3yr <- ifelse(w8data$q30d==1 | w8data$q30e==1 | w8data$q30f==1,
                               1,0)

w8data$employ <- ifelse(w8data$q66a>1 | w8data$q66b>1 | w8data$q66c>1 | w8data$q66e>1,
                        1,0)
w8data$employ <- factor(w8data$employ,labels=c("Not employed","Employed"))

w8data$live_u18 <- ifelse(w8data$q96c>1,
                          1,0)
w8data$live_o18 <- ifelse(w8data$q96d>1,
                          1,0)

w8data <- w8data %>% 
  rename(alcfre = q51,
         alcqnt = q52,
         alcbng = q53)

w8data <- w8data %>% mutate(alcfre = case_match(alcfre, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1.5,
                                            5 ~ 3.5,
                                            6 ~ 5.5,
                                            7 ~ 7))

w8data <- w8data %>% mutate(alcqnt = case_match(alcqnt, 
                                            1 ~ 1.5,
                                            2 ~ 3.5,
                                            3 ~ 6.5,
                                            4 ~ 9))

w8data <- w8data %>% 
  rename(fruit = q55,
         vegetables = q56)

w8data <- w8data %>% mutate(fruit = case_match(fruit, 
                                           1 ~ 0,
                                           2 ~ 0,
                                           3 ~ 0,
                                           4 ~ 1,
                                           5 ~ 1,
                                           6 ~ 1,
                                           7 ~ 1))

w8data <- w8data %>% mutate(vegetables = case_match(vegetables, 
                                                1 ~ 0,
                                                2 ~ 0,
                                                3 ~ 0,
                                                4 ~ 0,
                                                5 ~ 0,
                                                6 ~ 1,
                                                7 ~ 1))

w8data <- w8data %>% 
  rename(walking_time = q46atotmin,
         moderate_time = q46btotmin,
         vig_leis_time = q46ctotmin,
         vig_hous_time = q46dtotmin)

w8data$walking_time <- ifelse(w8data$q45a==0,0,w8data$walking_time)
w8data$moderate_time <- ifelse(w8data$q45b==0,0,w8data$moderate_time)
w8data$vig_leis_time <- ifelse(w8data$q45c==0,0,w8data$vig_leis_time)
w8data$vig_hous_time <- ifelse(w8data$q45d==0,0,w8data$vig_hous_time)

w8data <- w8data %>% 
  rename(pcsa = pcs_abs,
         mcsa = mcs_abs)

w8data <- subset(w8data, select = -c(q29a,q29b,q29c,q29d,q29e,
                                     q30d,q30e,q30f,q30h,q30i,q30k,q30l,q30v,q30w,q30x,q30y,q30z,q30aa,q30bb,q30gg,
                                     q45a,q45b,q45c,q45d,
                                     q66a,q66b,q66c,q66e,
                                     q96c,q96d))

saveRDS(w8data,file=paste0(workdir,"Paper 3 - Obesity/Data/w8 bmi.rds"))

######################################################################################
# 10. Wave 9 Data
#-------------------------------------------------------------------------------------

w9data <- read_dta(paste0(workdir,"Data/w9Mbf.dta"))
w9data <- zap_label(w9data)
w9data <- zap_labels(w9data)
w9data <- zap_formats(w9data)

names(w9data) <- tolower(names(w9data))
w9data$wave <- 9
w9data <- w9data %>% 
  rename_all(~stringr::str_replace(.,"^m9",""))

w9data <- subset(w9data, select = c(idproj,wave,htcm,wtkg,
                                    age,q75a,q75b,q75c,q75e,ariapgp,sd16_deciles,marital,whobmigroup,bmi,q112c,q112d,q66,q67,
                                    smokst,q62,q63,q64,mnstrs,cesd10,q53atotmin,q53btotmin,q53ctotmin,q53dtotmin,
                                    q52a,q52b,q52c,q52d,pcs_abs,mcs_abs,q91,q32a,q32b,q32c,q32d,q32e,
                                    q33e,q33f,q33g,q33j,q33k,q33l,q33m,q33n,q33p,q33q,
                                    q33dd,q33ee,q33ff,q33gg,q33hh,q33ii,q33kk,q33ll,
                                    q15h,q16a,q22aa,q22ab))

w9data <- w9data %>% 
  rename_all(~stringr::str_replace(.,"^",""))

w9data <- w9data %>% 
  rename(seifadis = sd16_deciles,
         finfinc = q91,
         dentist = q15h,
         vitamins = q16a,
         pap = q22aa,
         mam = q22ab)

w9data <- w9data %>% mutate(dentist = case_match(dentist, 2 ~ 0))

w9data <- w9data %>% mutate(pap = case_match(pap, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w9data <- w9data %>% mutate(mam = case_match(mam, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w9data$sleep_prob <- w9data$q32a + w9data$q32b + w9data$q32c + w9data$q32d + w9data$q32e

w9data$heartdis_3yr <- ifelse(w9data$q33j>=1 | w9data$q33k==1 | w9data$q33l==1 | w9data$q33m==1 | w9data$q33n==1,
                              1,0)
w9data$hypert_3yr <- ifelse(w9data$q33p>=1,
                            1,0)
w9data$stroke_3yr <- ifelse(w9data$q33q>=1,
                            1,0)  
w9data$cancer_3yr <- ifelse(w9data$q33dd==1 | w9data$q33ee==1 | w9data$q33ff==1 | w9data$q33gg==1 | w9data$q33hh==1 | w9data$q33ii==1,
                            1,0)
w9data <- w9data %>% 
  rename(depression_3yr = q33kk,
         anxiety_3yr = q33ll)
w9data$arthritis_3yr <- ifelse(w9data$q33e==1 | w9data$q33f==1 | w9data$q33g==1,
                               1,0)

w9data$employ <- ifelse(w9data$q75a>1 | w9data$q75b>1 | w9data$q75c>1 | w9data$q75e>1,
                        1,0)
w9data$employ <- factor(w9data$employ,labels=c("Not employed","Employed"))

w9data$live_u18 <- ifelse(w9data$q112c>1,
                          1,0)
w9data$live_o18 <- ifelse(w9data$q112d>1,
                          1,0)

w9data <- w9data %>% 
  rename(alcfre = q62,
         alcqnt = q63,
         alcbng = q64)

w9data <- w9data %>% mutate(alcfre = case_match(alcfre, 
                                            1 ~ 0,
                                            2 ~ 0,
                                            3 ~ 0,
                                            4 ~ 1.5,
                                            5 ~ 3.5,
                                            6 ~ 5.5,
                                            7 ~ 7))

w9data <- w9data %>% mutate(alcqnt = case_match(alcqnt, 
                                            1 ~ 1.5,
                                            2 ~ 3.5,
                                            3 ~ 6.5,
                                            4 ~ 9))

w9data <- w9data %>% 
  rename(fruit = q66,
         vegetables = q67)

w9data <- w9data %>% mutate(fruit = case_match(fruit, 
                                           1 ~ 0,
                                           2 ~ 0,
                                           3 ~ 0,
                                           4 ~ 1,
                                           5 ~ 1,
                                           6 ~ 1,
                                           7 ~ 1))

w9data <- w9data %>% mutate(vegetables = case_match(vegetables, 
                                                1 ~ 0,
                                                2 ~ 0,
                                                3 ~ 0,
                                                4 ~ 0,
                                                5 ~ 0,
                                                6 ~ 1,
                                                7 ~ 1))

w9data <- w9data %>% 
  rename(walking_time = q53atotmin,
         moderate_time = q53btotmin,
         vig_leis_time = q53ctotmin,
         vig_hous_time = q53dtotmin)

w9data$walking_time <- ifelse(w9data$q52a==0,0,w9data$walking_time)
w9data$moderate_time <- ifelse(w9data$q52b==0,0,w9data$moderate_time)
w9data$vig_leis_time <- ifelse(w9data$q52c==0,0,w9data$vig_leis_time)
w9data$vig_hous_time <- ifelse(w9data$q52d==0,0,w9data$vig_hous_time)

w9data <- w9data %>% 
  rename(pcsa = pcs_abs,
         mcsa = mcs_abs)

w9data <- subset(w9data, select = -c(q32a,q32b,q32c,q32d,q32e,
                                     q33e,q33f,q33g,q33j,q33k,q33l,q33m,q33n,q33p,q33q,
                                     q33dd,q33ee,q33ff,q33gg,q33hh,q33ii,q52a,q52b,q52c,q52d,
                                     q75a,q75b,q75c,q75e,q112c,q112d))

saveRDS(w9data,file=paste0(workdir,"Paper 3 - Obesity/Data/w9 bmi.rds"))

######################################################################################
# 11. Wave 10 Data
#-------------------------------------------------------------------------------------

w10data <- read_dta(paste0(workdir,"Data/w10Mbf.dta"))
w10data <- zap_label(w10data)
w10data <- zap_labels(w10data)
w10data <- zap_formats(w10data)

names(w10data) <- tolower(names(w10data))
w10data$wave <- 10
w10data <- w10data %>% 
  rename_all(~stringr::str_replace(.,"^m10",""))

w10data <- subset(w10data, select = c(idproj,wave,whobmigroup,bmi,htcm,wtkg,
                                      q15i,q21a,q21b))

w10data <- w10data %>% 
  rename(dentist = q15i,
         pap = q21a,
         mam = q21b)

w10data <- w10data %>% mutate(dentist = case_match(dentist, 2 ~ 0))

w10data <- w10data %>% mutate(pap = case_match(pap, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

w10data <- w10data %>% mutate(mam = case_match(mam, 
                                             1 ~ 1,
                                             2 ~ 2,
                                             3 ~ 2,
                                             4 ~ 3,
                                             .default = NA))

saveRDS(w10data,file=paste0(workdir,"Paper 3 - Obesity/Data/w10 bmi.rds"))

######################################################################################
# 12. Death
#-------------------------------------------------------------------------------------

deathdata <- read_dta(paste0(workdir,"Data/recentmidstatus.dta"))
deathdata <- zap_label(deathdata)
deathdata <- zap_labels(deathdata)
deathdata <- zap_formats(deathdata)

names(deathdata) <- tolower(names(deathdata))

deathdata$attrition110 <- ifelse(deathdata$attrition19==6,6,1)
deathdata$attrition110 <- ifelse((deathdata$deathyear>=2020 & !is.na(deathdata$deathyear) & deathdata$attrition110!=6),6,deathdata$attrition110)
deathdata <- subset(deathdata, select = c(idproj,attrition12,attrition13,attrition14,attrition15,attrition16,attrition17,attrition18,attrition19,attrition110))

deathdata <- pivot_longer(deathdata,
                          2:10,
                          names_to = "wave",
                          values_to = "death",
                          names_pattern = "attrition1(.*)")
deathdata$death <- ifelse(deathdata$death<6,0,1)
deathdata$wave <- as.numeric(deathdata$wave)

saveRDS(deathdata,file=paste0(workdir,"Paper 3 - Obesity/Data/deathdata bmi.rds"))



