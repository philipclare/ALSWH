######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Extract key variables from separate wave datasets
## Date: 29 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/"

libs <- c("haven","plyr","dplyr")
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

w1data <- subset(w1data, select = c(idproj,inarea,m1wtarea,
                                       m1cobcat,m1q90,m1pf,m1mh,m1gh,m1re,m1rp,m1sf,m1bp,m1vt,m1pcsa,m1mcsa))
w1data <- w1data %>% 
  rename_all(~stringr::str_replace(.,"^m1","b_"))

w1data <- w1data %>% 
  rename(b_educ = b_q90)

w1data <- w1data %>% mutate(b_educ = recode(b_educ, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1,
                                            `5` = 1,
                                            `6` = 2,
                                            `7` = 2))
w1data$b_educ <- factor(w1data$b_educ,labels=c("Less than high school","Trade/apprentice/certificate/diploma","University"))
w1data <- w1data %>% mutate(b_cobcat = recode(b_cobcat, 
                                            `1` = 0,
                                            `2` = 1,
                                            `3` = 1,
                                            `4` = 1,
                                            `5` = 1))
w1data$b_cobcat <- factor(w1data$b_cobcat,labels=c("Australia","Other"))

save(w1data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w1 pa.RData"))

######################################################################################
# 3. Wave 2 Data
#-------------------------------------------------------------------------------------

w2data <- read_dta(paste0(workdir,"Data/w2Mbf.dta"))
w2data <- zap_label(w2data)
w2data <- zap_labels(w2data)
w2data <- zap_formats(w2data)

names(w2data) <- tolower(names(w2data))
w2data <- subset(w2data, select = c(idproj,
                                      m2age,m2q54mn,m2q54sc,m2ariapgp,m2i_disad,m2marital,m2whobmigroup,m2q74b,m2q74c,
                                      m2q74d,m2smokst,m2q39,m2q40,m2q41,m2mnstrs,m2cesd10,m2pf,m2mh,m2gh,m2re,m2rp,m2sf,m2bp,m2vt,
                                      m2q20k,m2q20l,m2q20m,m2q20n,m2q20o,m2q20p,m2q20q,m2pcsa,m2mcsa))
w2data$wave <- 2

w2data <- w2data %>% 
  rename_all(~stringr::str_replace(.,"^m2",""))
w2data <- w2data %>% 
  rename(seifadis = i_disad)

w2data$b_cancer_ever <- ifelse(w2data$q20k>=1 | w2data$q20k>=1 | w2data$q20k>=1 | w2data$q20k>=1 | w2data$q20o>=1,
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
w2data <- w2data %>% mutate(alcfre = recode(alcfre, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w2data <- w2data %>% mutate(alcqnt = recode(alcqnt, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

w2data <- subset(w2data, select = -c(q20k,q20l,q20m,q20n,q20o,q20p,q20q,q54mn,q54sc,q74b,q74c,q74d))

save(w2data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w2 pa.RData"))

######################################################################################
# 4. Wave 3 Data
#-------------------------------------------------------------------------------------

w3data <- read_dta(paste0(workdir,"Data/w3Mbf.dta"))
w3data <- zap_label(w3data)
w3data <- zap_labels(w3data)
w3data <- zap_formats(w3data)

names(w3data) <- tolower(names(w3data))
w3data <- subset(w3data, select = c(idproj,
                                    m3age,m3q71a,m3q71b,m3q71c,m3q71e,m3ariapgp,m3i_disad,m3marital,m3whobmigroup,m3q90c,m3q90d,
                                    m3q90e,m3smokst,m3mnstrs,m3cesd10,m3q69ah,m3q69am,m3q69bh,m3q69bm,m3q69ch,m3q69cm,m3q69dh,m3q69dm,
                                    m3pf,m3mh,m3gh,m3re,m3rp,m3sf,m3bp,m3vt,m3pcsa,m3mcsa,
                                    m3q35a,m3q35m,m3q35n,m3q35o,m3q35p,m3q35q,m3q35r,m3q35y))
w3data$wave <- 3
w3data <- w3data %>% 
  rename_all(~stringr::str_replace(.,"^m3",""))

w3data <- w3data %>% 
  rename(seifadis = i_disad)
         
w3data$cancer_3yr <- ifelse(w3data$q35m==1 | w3data$q35n==1 | w3data$q35o==1 | w3data$q35p==1,
                            1,0)
w3data <- w3data %>% 
  rename(depression_3yr = q35q,
         anxiety_3yr = q35r,
         arthritis_3yr = q35a)

w3data$employ <- ifelse(w3data$q71a>1 | w3data$q71b>1 & w3data$q71c>1 | w3data$q71e>1,
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

w3data <- subset(w3data, select = -c(q35m,q35n,q35o,q35p,q35y,
                                     q69ah,q69am,q69bh,q69bm,q69ch,q69cm,q69dh,q69dm,
                                     q71a,q71b,q71c,q71e,
                                     q90c,q90d,q90e))

save(w3data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w3 pa.RData"))

######################################################################################
# 5. Wave 4 Data
#-------------------------------------------------------------------------------------

w4data <- read_dta(paste0(workdir,"Data/w4Mbf.dta"))
w4data <- zap_label(w4data)
w4data <- zap_labels(w4data)
w4data <- zap_formats(w4data)

names(w4data) <- tolower(names(w4data))
w4data <- subset(w4data, select = c(idproj,
                                    m4age,m4q68a,m4q68c,m4ariapgp,m4seifadis,m4marital,m4whobmigroup,m4q89c,m4q89d,m4q89e,m4q63,m4q62,
                                    m4smokst,m4q59,m4q60,m4q61,m4mnstrs,m4cesd10,m4q67ah,m4q67am,m4q67bh,m4q67bm,m4q67ch,m4q67cm,m4q67dh,m4q67dm,
                                    m4pf,m4mh,m4gh,m4re,m4rp,m4sf,m4bp,m4vt,m4pcsa,m4mcsa,
                                    m4q32a,m4q32l,m4q32m,m4q32n,m4q32o,m4q32p,m4q32q,m4q32r,m4q32w))
w4data$wave <- 4
w4data <- w4data %>% 
  rename_all(~stringr::str_replace(.,"^m4",""))

w4data$cancer_3yr <- ifelse(w4data$q32l==1 | w4data$q32m==1 | w4data$q32n==1 | w4data$q32o==1 | w4data$q32p==1,
                            1,0)
w4data <- w4data %>% 
  rename(depression_3yr = q32q,
         anxiety_3yr = q32r,
         arthritis_3yr = q32a)

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
w4data <- w4data %>% mutate(alcfre = recode(alcfre, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w4data <- w4data %>% mutate(alcqnt = recode(alcqnt, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

w4data <- w4data %>% 
  rename(fruit = q63,
         vegetables = q62)
w4data <- w4data %>% mutate(fruit = recode(fruit, 
                                           `0` = 0,
                                           `1` = 0,
                                           `2` = 1,
                                           `3` = 1,
                                           `4` = 1))

w4data <- w4data %>% mutate(vegetables = recode(vegetables, 
                                                `0` = 0,
                                                `1` = 0,
                                                `2` = 0,
                                                `3` = 1,
                                                `4` = 1))

w4data$walking_time <- w4data$q67ah*60 + w4data$q67am
w4data$moderate_time <- w4data$q67bh*60 + w4data$q67bm
w4data$vig_leis_time <- w4data$q67ch*60 + w4data$q67cm
w4data$vig_hous_time <- w4data$q67dh*60 + w4data$q67dm

w4data <- subset(w4data, select = -c(q32l,q32m,q32n,q32o,q32p,q32w,
                                     q67ah,q67am,q67bh,q67bm,q67ch,q67cm,q67dh,q67dm,
                                     q68a,q68c,
                                     q89c,q89d,q89e))

save(w4data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w4 pa.RData"))

######################################################################################
# 6. Wave 5 Data
#-------------------------------------------------------------------------------------

w5data <- read_dta(paste0(workdir,"Data/w5Mbf.dta"))
w5data <- zap_label(w5data)
w5data <- zap_labels(w5data)
w5data <- zap_formats(w5data)

names(w5data) <- tolower(names(w5data))
w5data <- subset(w5data, select = c(idproj,
                                    m5age,m5q85a,m5q85b,m5q85c,m5q85e,m5ariapgp,m5seifadis,m5marital,m5whobmigroup,m5q107c,m5q107d,m5q107e,m5q64,m5q65,
                                    m5smokst,m5q59,m5q60,m5q61,m5mnstrs,m5cesd10,m5q82ah,m5q82am,m5q82bh,m5q82bm,m5q82ch,m5q82cm,m5q82dh,m5q82dm,
                                    m5pf,m5mh,m5gh,m5re,m5rp,m5sf,m5bp,m5vt,m5pcsa,m5mcsa,
                                    m5q38c,m5q38d,m5q38e,m5q38m,m5q38n,m5q38o,m5q38p,m5q38q,m5q38r,m5q38w))
w5data$wave <- 5
w5data <- w5data %>% 
  rename_all(~stringr::str_replace(.,"^m5",""))

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
w5data <- w5data %>% mutate(alcfre = recode(alcfre, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w5data <- w5data %>% mutate(alcqnt = recode(alcqnt, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

w5data <- w5data %>% 
  rename(fruit = q64,
         vegetables = q65)
w5data <- w5data %>% mutate(fruit = recode(fruit, 
                                           `0` = 0,
                                           `1` = 0,
                                           `2` = 0,
                                           `3` = 1,
                                           `4` = 1,
                                           `5` = 1,
                                           `6` = 1))

w5data <- w5data %>% mutate(vegetables = recode(vegetables, 
                                                `0` = 0,
                                                `1` = 0,
                                                `2` = 0,
                                                `3` = 0,
                                                `4` = 0,
                                                `5` = 1,
                                                `6` = 1))

w5data$walking_time <- w5data$q82ah*60 + w5data$q82am
w5data$moderate_time <- w5data$q82bh*60 + w5data$q82bm
w5data$vig_leis_time <- w5data$q82ch*60 + w5data$q82cm
w5data$vig_hous_time <- w5data$q82dh*60 + w5data$q82dm

w5data <- subset(w5data, select = -c(q38c,q38d,q38e,q38m,q38n,q38o,q38p,q38w,
                                     q82ah,q82am,q82bh,q82bm,q82ch,q82cm,q82dh,q82dm,
                                     q85a,q85b,q85c,q85e,
                                     q107c,q107d,q107e))

save(w5data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w5 pa.RData"))

######################################################################################
# 7. Wave 6 Data
#-------------------------------------------------------------------------------------

w6data <- read_dta(paste0(workdir,"Data/w6Mbf.dta"))
w6data <- zap_label(w6data)
w6data <- zap_labels(w6data)
w6data <- zap_formats(w6data)

names(w6data) <- tolower(names(w6data))
w6data <- subset(w6data, select = c(idproj,
                                    m6age,m6q84a,m6q84b,m6q84c,m6q84e,m6ariapgp,m6seifadis,m6marital,m6whobmigroup,m6q111c,m6q111d,m6q111e,m6q64,m6q65,
                                    m6smokst,m6q60,m6q61,m6q62,m6mnstrs,m6cesd10,m6q82atotmin,m6q82btotmin,m6q82ctotmin,m6q82dtotmin,m6q81a,m6q81b,m6q81c,m6q81d,
                                    m6pf,m6mh,m6gh,m6re,m6rp,m6sf,m6bp,m6vt,m6pcsa,m6mcsa,
                                    m6q38c,m6q38d,m6q38e,m6q38m,m6q38n,m6q38o,m6q38p,m6q38q,m6q38r,m6q38x))
w6data$wave <- 6
w6data <- w6data %>% 
  rename_all(~stringr::str_replace(.,"^m6",""))

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
w6data <- w6data %>% mutate(alcfre = recode(alcfre, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w6data <- w6data %>% mutate(alcqnt = recode(alcqnt, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

w6data <- w6data %>% 
  rename(fruit = q64,
         vegetables = q65)
w6data <- w6data %>% mutate(fruit = recode(fruit, 
                                           `0` = 0,
                                           `1` = 0,
                                           `2` = 0,
                                           `3` = 1,
                                           `4` = 1,
                                           `5` = 1,
                                           `6` = 1))

w6data <- w6data %>% mutate(vegetables = recode(vegetables, 
                                                `0` = 0,
                                                `1` = 0,
                                                `2` = 0,
                                                `3` = 0,
                                                `4` = 0,
                                                `5` = 1,
                                                `6` = 1))

w6data <- w6data %>% 
  rename(walking_time = q82atotmin,
         moderate_time = q82btotmin,
         vig_leis_time = q82ctotmin,
         vig_hous_time = q82dtotmin)

w6data$walking_time <- ifelse(w6data$q81a==0,0,w6data$walking_time)
w6data$moderate_time <- ifelse(w6data$q81b==0,0,w6data$moderate_time)
w6data$vig_leis_time <- ifelse(w6data$q81c==0,0,w6data$vig_leis_time)
w6data$vig_hous_time <- ifelse(w6data$q81d==0,0,w6data$vig_hous_time)

w6data <- subset(w6data, select = -c(q38c,q38d,q38e,q38m,q38n,q38o,q38p,q38x,
                                     q81a,q81b,q81c,q81d,
                                     q84a,q84b,q84c,q84e,
                                     q111c,q111d,q111e))

save(w6data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w6 pa.RData"))

######################################################################################
# 8. Wave 7 Data
#-------------------------------------------------------------------------------------

w7data <- read_dta(paste0(workdir,"Data/w7Mbf.dta"))
w7data <- zap_label(w7data)
w7data <- zap_labels(w7data)
w7data <- zap_formats(w7data)

names(w7data) <- tolower(names(w7data))
w7data <- subset(w7data, select = c(idproj,
                                    m7age,m7q83a,m7q83b,m7q83c,m7q83e,m7ariapgp,m7seifadis,m7marital,m7whobmigroup,m7q101c,m7q101d,
                                    m7smokst,m7q51,m7q52,m7q53,m7mnstrs,m7cesd10,m7q47ahrs,m7q47amins,m7q47bhrs,m7q47bmins,m7q47chrs,m7q47cmins,m7q47dhrs,m7q47dmins,
                                    m7pf,m7mh,m7gh,m7re,m7rp,m7sf,m7bp,m7vt,m7pcsa,m7mcsa,
                                    m7q32d,m7q32e,m7q32f,m7q32v,m7q32w,m7q32x,m7q32y,m7q32z,m7q32aa,m7q32cc,m7q32dd,m7q32h,m7q32bb,m7q32gg))
w7data$wave <- 7
w7data <- w7data %>% 
  rename_all(~stringr::str_replace(.,"^m7",""))

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
w7data <- w7data %>% mutate(alcfre = recode(alcfre, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w7data <- w7data %>% mutate(alcqnt = recode(alcqnt, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

w7data$walking_time <- w7data$q47ahrs*60 + w7data$q47amins
w7data$moderate_time <- w7data$q47bhrs*60 + w7data$q47bmins
w7data$vig_leis_time <- w7data$q47chrs*60 + w7data$q47cmins
w7data$vig_hous_time <- w7data$q47dhrs*60 + w7data$q47dmins

w7data <- subset(w7data, select = -c(q32d,q32e,q32f,q32h,q32v,q32w,q32x,q32y,q32z,q32aa,q32bb,q32gg,
                                     q47ahrs,q47amins,q47bhrs,q47bmins,q47chrs,q47cmins,q47dhrs,q47dmins,
                                     q83a,q83b,q83c,q83e,
                                     q101c,q101d))

save(w7data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w7 pa.RData"))

######################################################################################
# 9. Wave 8 Data
#-------------------------------------------------------------------------------------

w8data <- read_dta(paste0(workdir,"Data/w8Mbf.dta"))
w8data <- zap_label(w8data)
w8data <- zap_labels(w8data)
w8data <- zap_formats(w8data)

names(w8data) <- tolower(names(w8data))
w8data <- subset(w8data, select = c(idproj,
                                    m8age,m8q66a,m8q66b,m8q66c,m8q66e,m8ariapgp,m8seifadis,m8marital,m8whobmigroup,m8q96c,m8q96d,m8q55,m8q56,
                                    m8smokst,m8q51,m8q52,m8q53,m8mnstrs,m8cesd10,m8q46atotmin,m8q46btotmin,m8q46ctotmin,m8q46dtotmin,
                                    m8q45a,m8q45b,m8q45c,m8q45d,m8pf,m8mh,m8gh,m8re,m8rp,m8sf,m8bp,m8vt,m8pcs_abs,m8mcs_abs,
                                    m8q30d,m8q30e,m8q30f,m8q30v,m8q30w,m8q30x,m8q30y,m8q30z,m8q30aa,m8q30cc,m8q30dd,m8q30h,m8q30bb,m8q30gg))
w8data$wave <- 8
w8data <- w8data %>% 
  rename_all(~stringr::str_replace(.,"^m8",""))

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
w8data <- w8data %>% mutate(alcfre = recode(alcfre, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w8data <- w8data %>% mutate(alcqnt = recode(alcqnt, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

w8data <- w8data %>% 
  rename(fruit = q55,
         vegetables = q56)
w8data <- w8data %>% mutate(fruit = recode(fruit, 
                                           `1` = 0,
                                           `2` = 0,
                                           `3` = 0,
                                           `4` = 1,
                                           `5` = 1,
                                           `6` = 1,
                                           `7` = 1))

w8data <- w8data %>% mutate(vegetables = recode(vegetables, 
                                                `1` = 0,
                                                `2` = 0,
                                                `3` = 0,
                                                `4` = 0,
                                                `5` = 0,
                                                `6` = 1,
                                                `7` = 1))

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

w8data <- subset(w8data, select = -c(q30d,q30e,q30f,q30h,q30v,q30w,q30x,q30y,q30z,q30aa,q30bb,q30gg,
                                     q45a,q45b,q45c,q45d,
                                     q66a,q66b,q66c,q66e,
                                     q96c,q96d))

save(w8data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w8 pa.RData"))

######################################################################################
# 10. Wave 9 Data
#-------------------------------------------------------------------------------------

w9data <- read_dta(paste0(workdir,"Data/w9Mbf.dta"))
w9data <- zap_label(w9data)
w9data <- zap_labels(w9data)
w9data <- zap_formats(w9data)

names(w9data) <- tolower(names(w9data))
w9data <- subset(w9data, select = c(idproj,m9pf,m9mh,m9gh,m9re,m9rp,m9sf,m9bp,m9vt,m9pcs_abs,m9mcs_abs))
w9data$wave <- 9
w9data <- w9data %>% 
  rename_all(~stringr::str_replace(.,"^m9",""))

w9data <- w9data %>% 
  rename(pcsa = pcs_abs,
         mcsa = mcs_abs)

save(w9data,file=paste0(workdir,"Paper 1 - Health-related quality of life/Data/w9 pa.RData"))

