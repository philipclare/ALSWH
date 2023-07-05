######################################################################################
##   
## Syntax File 1
## Physical Activity Latent Class Analysis
## Extract key variables from separate wave datasets
## Date: 6 October 2022
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "R:/PRJ-prc_alswh/"

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
                                       m1cobcat,m1q90))
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

save(w1data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w1 pa.RData"))

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

w2data <- w2data %>% 
  rename_all(~stringr::str_replace(.,"^m2",""))
w2data <- w2data %>% 
  rename(b_seifadis = i_disad)

w2data$b_cancer_ever <- ifelse(w2data$q20k>=1 | w2data$q20k>=1 | w2data$q20k>=1 | w2data$q20k>=1 | w2data$q20o>=1,
                            1,0)
w2data$b_depression_ever <- ifelse(w2data$q20p>=1,
                               1,0)
w2data$b_anxiety_ever <- ifelse(w2data$q20q>=1,
                               1,0)

w2data$q54sc <- ifelse(is.na(w2data$q54sc),0,w2data$q54sc)
w2data$b_employ <- ifelse(((w2data$q54mn<1 | w2data$q54mn>3) & (w2data$q54sc<1 | w2data$q54sc>3)),
              0,1)
w2data$b_employ <- factor(w2data$b_employ,labels=c("Not employed","Employed"))

w2data$b_live_u18 <- ifelse(w2data$q74b>1 | w2data$q74c>1,
                          1,0)
w2data$b_live_o18 <- ifelse(w2data$q74d>1,
                          1,0)

w2data <- w2data %>% 
  rename(b_alcfre = q39,
         b_alcqnt = q40,
         b_alcbng = q41)
w2data <- w2data %>% mutate(b_alcfre = recode(b_alcfre, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w2data <- w2data %>% mutate(b_alcqnt = recode(b_alcqnt, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

w2data <- w2data %>% 
  rename(b_pcsa = pcsa,
         b_mcsa = mcsa,
         b_pf = pf,
         b_mh = mh,
         b_gh = gh,
         b_re = re,
         b_rp = rp,
         b_sf = sf,
         b_bp = bp,
         b_vt = vt,
         b_age = age,
         b_ariapgp = ariapgp,
         b_marital = marital,
         b_bmigp = whobmigroup,
         b_smokst = smokst,
         b_stress = mnstrs,
         b_cesd = cesd10)

w2data <- w2data %>% mutate(b_ariapgp = recode(b_ariapgp, 
                                                     `1` = 0,
                                                     `2` = 1,
                                                     `3` = 1,
                                                     `4` = 2,
                                                     `5` = 2,
                                                     `6` = NA_real_))
w2data$b_ariapgp <- factor(w2data$b_ariapgp,labels=c("Major city","Regional","Remote"))

w2data <- w2data %>% mutate(b_marital = recode(b_marital, 
                                                     `1` = 0,
                                                     `2` = 0,
                                                     `3` = 1,
                                                     `4` = 1,
                                                     `5` = 2,
                                                     `6` = 1))
w2data$b_marital <- factor(w2data$b_marital,labels=c("Married/de facto","Separated/divorced/never married","Widowed"))

w2data <- w2data %>% mutate(b_smokst = recode(b_smokst, 
                                                    `1` = 0,
                                                    `2` = 1,
                                                    `3` = 2,
                                                    `4` = 2,
                                                    `5` = 2,
                                                    `6` = NA_real_))
w2data$b_smokst <- factor(w2data$b_smokst,labels=c("Never smoker","Ex smoker","Current smoker"))

w2data$b_alcfq <- w2data$b_alcfre * w2data$b_alcqnt

w2data$b_cancer_ever <- with(w2data, ave(b_cancer_ever, idproj, FUN=function(f) min(f, na.rm=T)))
w2data$b_cancer_ever <- ifelse(w2data$b_cancer_ever>1,NA,w2data$b_cancer_ever)

w2data$b_depression_ever <- with(w2data, ave(b_depression_ever, idproj, FUN=function(f) min(f, na.rm=T)))
w2data$b_depression_ever <- ifelse(w2data$b_depression_ever>1,NA,w2data$b_depression_ever)

w2data$b_anxiety_ever <- with(w2data, ave(b_anxiety_ever, idproj, FUN=function(f) min(f, na.rm=T)))
w2data$b_anxiety_ever <- ifelse(w2data$b_anxiety_ever>1,NA,w2data$b_anxiety_ever)

ny_list <- c("b_live_u18","b_live_o18")
w2data[,ny_list] <- lapply(w2data[,ny_list], factor, labels=c("No","Yes"))
w2data$b_bmigp <- factor(w2data$b_bmigp, labels=c("Underweight","Healthy","Overweight","Obese"))

w2data <- subset(w2data, select = -c(b_alcfre,b_alcqnt,q20k,q20l,q20m,q20n,q20o,q20p,q20q,q54mn,q54sc,q74b,q74c,q74d))

save(w2data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w2 pa.RData"))

######################################################################################
# 4. Wave 3 Data
#-------------------------------------------------------------------------------------

w3data <- read_dta(paste0(workdir,"Data/w3Mbf.dta"))
w3data <- zap_label(w3data)
w3data <- zap_labels(w3data)
w3data <- zap_formats(w3data)

names(w3data) <- tolower(names(w3data))
w3data <- subset(w3data, select = c(idproj,
                                    m3q69ah,m3q69am,m3q69bh,m3q69bm,m3q69ch,m3q69cm,m3q69dh,m3q69dm))
w3data <- w3data %>% 
  rename_all(~stringr::str_replace(.,"^m3",""))

w3data$walking_time <- w3data$q69ah*60 + w3data$q69am
w3data$moderate_time <- w3data$q69bh*60 + w3data$q69bm
w3data$vig_leis_time <- w3data$q69ch*60 + w3data$q69cm
w3data$vig_hous_time <- w3data$q69dh*60 + w3data$q69dm

w3data <- subset(w3data, select = -c(q69ah,q69am,q69bh,q69bm,q69ch,q69cm,q69dh,q69dm))

w3data$wave <- 3

save(w3data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w3 pa.RData"))

######################################################################################
# 5. Wave 4 Data
#-------------------------------------------------------------------------------------

w4data <- read_dta(paste0(workdir,"Data/w4Mbf.dta"))
w4data <- zap_label(w4data)
w4data <- zap_labels(w4data)
w4data <- zap_formats(w4data)

names(w4data) <- tolower(names(w4data))
w4data <- subset(w4data, select = c(idproj,
                                    m4q67ah,m4q67am,m4q67bh,m4q67bm,m4q67ch,m4q67cm,m4q67dh,m4q67dm))
w4data <- w4data %>% 
  rename_all(~stringr::str_replace(.,"^m4",""))

w4data$walking_time <- w4data$q67ah*60 + w4data$q67am
w4data$moderate_time <- w4data$q67bh*60 + w4data$q67bm
w4data$vig_leis_time <- w4data$q67ch*60 + w4data$q67cm
w4data$vig_hous_time <- w4data$q67dh*60 + w4data$q67dm

w4data <- subset(w4data, select = -c(q67ah,q67am,q67bh,q67bm,q67ch,q67cm,q67dh,q67dm))

w4data$wave <- 4

save(w4data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w4 pa.RData"))

######################################################################################
# 6. Wave 5 Data
#-------------------------------------------------------------------------------------

w5data <- read_dta(paste0(workdir,"Data/w5Mbf.dta"))
w5data <- zap_label(w5data)
w5data <- zap_labels(w5data)
w5data <- zap_formats(w5data)

names(w5data) <- tolower(names(w5data))
w5data <- subset(w5data, select = c(idproj,
                                    m5q82ah,m5q82am,m5q82bh,m5q82bm,m5q82ch,m5q82cm,m5q82dh,m5q82dm))
w5data <- w5data %>% 
  rename_all(~stringr::str_replace(.,"^m5",""))

w5data$walking_time <- w5data$q82ah*60 + w5data$q82am
w5data$moderate_time <- w5data$q82bh*60 + w5data$q82bm
w5data$vig_leis_time <- w5data$q82ch*60 + w5data$q82cm
w5data$vig_hous_time <- w5data$q82dh*60 + w5data$q82dm

w5data <- subset(w5data, select = -c(q82ah,q82am,q82bh,q82bm,q82ch,q82cm,q82dh,q82dm))

w5data$wave <- 5

save(w5data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w5 pa.RData"))

######################################################################################
# 7. Wave 6 Data
#-------------------------------------------------------------------------------------

w6data <- read_dta(paste0(workdir,"Data/w6Mbf.dta"))
w6data <- zap_label(w6data)
w6data <- zap_labels(w6data)
w6data <- zap_formats(w6data)

names(w6data) <- tolower(names(w6data))
w6data <- subset(w6data, select = c(idproj,
                                    m6q82atotmin,m6q82btotmin,m6q82ctotmin,m6q82dtotmin,m6q81a,m6q81b,m6q81c,m6q81d))
w6data <- w6data %>% 
  rename_all(~stringr::str_replace(.,"^m6",""))

w6data <- w6data %>% 
  rename(walking_time = q82atotmin,
         moderate_time = q82btotmin,
         vig_leis_time = q82ctotmin,
         vig_hous_time = q82dtotmin)

w6data$walking_time <- ifelse(w6data$q81a==0,0,w6data$walking_time)
w6data$moderate_time <- ifelse(w6data$q81b==0,0,w6data$moderate_time)
w6data$vig_leis_time <- ifelse(w6data$q81c==0,0,w6data$vig_leis_time)
w6data$vig_hous_time <- ifelse(w6data$q81d==0,0,w6data$vig_hous_time)

w6data <- subset(w6data, select = -c(q81a,q81b,q81c,q81d))

w6data$wave <- 6

save(w6data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w6 pa.RData"))

######################################################################################
# 8. Wave 7 Data
#-------------------------------------------------------------------------------------

w7data <- read_dta(paste0(workdir,"Data/w7Mbf.dta"))
w7data <- zap_label(w7data)
w7data <- zap_labels(w7data)
w7data <- zap_formats(w7data)

names(w7data) <- tolower(names(w7data))
w7data <- subset(w7data, select = c(idproj,
                                    m7q47ahrs,m7q47amins,m7q47bhrs,m7q47bmins,m7q47chrs,m7q47cmins,m7q47dhrs,m7q47dmins))
w7data <- w7data %>% 
  rename_all(~stringr::str_replace(.,"^m7",""))

w7data$walking_time <- w7data$q47ahrs*60 + w7data$q47amins
w7data$moderate_time <- w7data$q47bhrs*60 + w7data$q47bmins
w7data$vig_leis_time <- w7data$q47chrs*60 + w7data$q47cmins
w7data$vig_hous_time <- w7data$q47dhrs*60 + w7data$q47dmins

w7data <- subset(w7data, select = -c(q47ahrs,q47amins,q47bhrs,q47bmins,q47chrs,q47cmins,q47dhrs,q47dmins))

w7data$wave <- 7

save(w7data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w7 pa.RData"))

######################################################################################
# 9. Wave 8 Data
#-------------------------------------------------------------------------------------

w8data <- read_dta(paste0(workdir,"Data/w8Mbf.dta"))
w8data <- zap_label(w8data)
w8data <- zap_labels(w8data)
w8data <- zap_formats(w8data)

names(w8data) <- tolower(names(w8data))
w8data <- subset(w8data, select = c(idproj,
                                    m8q46atotmin,m8q46btotmin,m8q46ctotmin,m8q46dtotmin,m8q45a,m8q45b,m8q45c,m8q45d))
w8data <- w8data %>% 
  rename_all(~stringr::str_replace(.,"^m8",""))

w8data <- w8data %>% 
  rename(walking_time = q46atotmin,
         moderate_time = q46btotmin,
         vig_leis_time = q46ctotmin,
         vig_hous_time = q46dtotmin)

w8data$walking_time <- ifelse(w8data$q45a==0,0,w8data$walking_time)
w8data$moderate_time <- ifelse(w8data$q45b==0,0,w8data$moderate_time)
w8data$vig_leis_time <- ifelse(w8data$q45c==0,0,w8data$vig_leis_time)
w8data$vig_hous_time <- ifelse(w8data$q45d==0,0,w8data$vig_hous_time)

w8data <- subset(w8data, select = -c(q45a,q45b,q45c,q45d))

w8data$wave <- 8

save(w8data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w8 pa.RData"))

######################################################################################
# 10. Wave 9 Data
#-------------------------------------------------------------------------------------

w9data <- read_dta(paste0(workdir,"Data/w9Mbf.dta"))
w9data <- zap_label(w9data)
w9data <- zap_labels(w9data)
w9data <- zap_formats(w9data)

names(w9data) <- tolower(names(w9data))
w9data <- subset(w9data, select = c(idproj,m9pf,m9mh,m9gh,m9re,m9rp,m9sf,m9bp,m9vt,m9pcs_abs,m9mcs_abs))

w9data <- w9data %>% 
  rename_all(~stringr::str_replace(.,"^m9",""))

w9data <- w9data %>% 
  rename(pcsa9 = pcs_abs,
         mcsa9 = mcs_abs,
         pf9 = pf,
         mh9 = mh,
         gh9 = gh,
         re9 = re,
         rp9 = rp,
         sf9 = sf,
         bp9 = bp,
         vt9 = vt)

save(w9data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/w9 pa.RData"))

######################################################################################
# 11. Merge data.
#-------------------------------------------------------------------------------------

long_data <- rbind.fill(w3data,w4data,w5data,w6data,w7data,w8data)

long_data$walking_time <- ifelse(long_data$walking_time>840,840,long_data$walking_time)
long_data$moderate_time <- ifelse(long_data$moderate_time>840,840,long_data$moderate_time)
long_data$vig_leis_time <- ifelse(long_data$vig_leis_time>840,840,long_data$vig_leis_time)
long_data$vig_hous_time <- ifelse(long_data$vig_hous_time>840,840,long_data$vig_hous_time)

long_data$weighted_activity_time <- (1*long_data$walking_time) + (1*long_data$moderate_time) + (2*long_data$vig_leis_time)

long_data <- subset(long_data, select = -c(walking_time,moderate_time,vig_leis_time,vig_hous_time))

wide_data <- reshape(long_data,
                     timevar=c("wave"), 
                     idvar=c("idproj"),
                     v.names=c("weighted_activity_time"),
                     sep = "",
                     dir="wide")

base_data <- merge(w1data,w2data,by="idproj",
                   all.x=TRUE,
                   all.y=TRUE)
wide_data <- merge(base_data,wide_data,by="idproj",
                   all.x=TRUE,
                   all.y=TRUE)
wide_data <- merge(wide_data,w9data,by="idproj",
                   all.x=TRUE,
                   all.y=TRUE)

imp_data <- wide_data[which(wide_data$b_pf>=54.8 | is.na(wide_data$b_pf)), ]

rm(w1data,w2data,w3data,w4data,w5data,w6data,w7data,w8data,w9data,base_data,long_data,wide_data)

save(imp_data,file=paste0(workdir,"Paper 0 - Latent Class Analysis/Data/imputation data.RData"))
