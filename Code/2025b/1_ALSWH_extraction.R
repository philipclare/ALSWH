######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Extract key variables from separate wave datasets
## Date: 21 June 2023
## Authors: Philip Clare and Neta Hagani
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# Setup Environment----------

workdir <- "Y:/PRJ-Loneliness_ALSWH/Data/"

libs <- c("haven","plyr","dplyr","tidyr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 1. Wave 1 Data----------------
wha1midb <- read_sav(paste0(workdir,"wha1midb.sav"))
w1data <- wha1midb %>%
  select(idproj, m1wtarea, inarea, m1IDIS, m1ariapgp, m1pf, m1mh, m1gh, m1re, m1rp, m1sf, m1bp, m1vt, m1pcsa, 
         m1mcsa, m1mnstrs, m1smokst, m1age, m1cobcat, m1ariapgp, m1marital, 
         m1q90, m1q77a, m1labf, m1lngcat, m1q76a, m1whobmigroup, m1q43, m1q44, m1q45)

w1data <- w1data %>% 
  rename_all(~stringr::str_replace(.,"^m1",""))

# creating a new column called wave with values of 2
w1data$wave <- 1

# renaming Seifa Index of Disadvantage
w1data <- w1data %>% 
  dplyr::rename(seifadis = IDIS)

# static variables
# education
w1data$q90 <- as.numeric(w1data$q90)
w1data <- w1data %>% mutate(b_educ = recode(q90, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1,
                                            `5` = 1,
                                            `6` = 2,
                                            `7` = 2))
# country of birth
w1data$cobcat <- as.numeric(w1data$cobcat)
w1data <- w1data %>% mutate(b_country = recode(cobcat, 
                                              `1` = 0,
                                              `2` = 1,
                                              `3` = 1,
                                              `4` = 1,
                                              `5` = 1))

# rename
w1data <- w1data %>% 
  dplyr::rename(b_wtarea = wtarea)
w1data <- w1data %>% 
  dplyr::rename(b_inarea = inarea)

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w1data <- w1data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                                (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 

# recoding I live alone 0=no 1=yes
# OG coding 1=yes 2=no
w1data <- w1data %>%
  mutate(live_alone = case_when(q77a == 1 ~ 1,
                                q77a == 2 ~ 0)) 

# recoding language spoken at home 1 = English, 2 = European, 3 = Asian/other, 4 = other
w1data <- w1data %>%
  mutate(b_language = case_when(lngcat >=1 & lngcat <=2 ~ 1,
                                lngcat == 3 ~ 2,
                                lngcat == 4 ~ 3,
                                lngcat == 5 ~ 4))

#Employment
w1data$employ <- ifelse(w1data$q76a==9,
                        0,1)

#w1check <- subset(w1data, select = c(activity_time, q63, q64, q65))
#%>% 
  #select(country, cobcat) %>% 
  #View()

# alcohol
w1data$q43 <- as.numeric(w1data$q43)
w1data$q44 <- as.numeric(w1data$q44)
w1data$q45 <- as.numeric(w1data$q45)

w1data <- w1data %>% mutate(alcfre = recode(q43, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))
w1data <- w1data %>% mutate(alcqnt = recode(q44, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))

# recoding binge drinking to match the other waves
w1data <- w1data %>%
  mutate(alcbng = recode(q45,
                         `1` = 1,
                         `2` = 2,
                         `3` = 3,
                         `4` = 4,
                         `5` = 5,
                         `6` = 0)) 
                         
w1datab <- subset(w1data, select = c(idproj, b_educ, b_wtarea, b_inarea, b_language, b_country))

w1datat <- subset(w1data, select = c(idproj, seifadis, ariapgp, pf, mh, gh, re, rp, sf, bp, vt, pcsa, mcsa, mnstrs,
                                     smokst, age, mstat, live_alone, employ, whobmigroup, alcfre, alcqnt, alcbng))

save(w1datat,file=paste0(workdir,"loneliness and mortality w1t.RData"))

save(w1datab,file=paste0(workdir,"loneliness and mortality w1b.RData"))
######################################################################################
# 2. Wave 2 Data-------------------
wha2midb <- read_sav(paste0(workdir,"wha2midb.sav"))
w2data <- wha2midb %>%
  select(idproj, m2I_disAd, m2mnsocsup6, m2q31i, m2q20c, m2q20e, m2q20k, m2q20l, m2q20m,
         m2smokst, m2pf, m2mh, m2gh, m2re, m2rp, m2sf, m2bp, m2vt, m2pcsa, m2mcsa, 
         m2age, m2ariapgp, m2marital, m2q54mn, m2q54sc, m2q74a, m2q74b, m2q74c, m2q74d, m2q74e, 
         m2q74f, m2q74g, m2whobmigroup, m2mnstrs, m2q39, m2q40, m2q41, m2q20p, m2q20q, m2q82a, m2q82b, m2q82c, m2q82d, m2q82e, m2q82f, m2q82g, m2q82h, m2q82i, m2q82j, m2q82k, m2q82l, m2q82m, m2q82n, m2q82o, m2q82p, m2q82q, m2q82r, m2q82s)

# creating a new column called wave with values of 2
w2data$wave <- 2

# removing m2 initial
w2data <- w2data %>% 
  rename_all(~stringr::str_replace(.,"^m2",""))

# life threatening illnesses 1=yes, 0=no
w2data$b_heartdis_ever <- ifelse(w2data$q20c >= 1, 1,0)
w2data$b_stroke_ever <- ifelse(w2data$q20e >= 1, 1,0)
w2data$b_cancer_ever <- ifelse(w2data$q20k >=1 | w2data$q20l >=1 | w2data$q20m >=1,
                               1,0)
# renaming Seifa Index of Disadvantage
w2data <- w2data %>% 
  dplyr::rename(seifadis = I_disAd)

# renaming MOS social support 6-items- higher score- higher social support
w2data <- w2data %>% 
  dplyr::rename(mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w2data <- w2data %>%
  rowwise() %>%
  mutate(mos_long = mean(c(q82a, q82b, q82c, q82d, q82e, q82f, q82g, q82h, q82i, q82j, q82k, q82l, q82m, q82n, q82o, q82p, q82q, q82r, q82s)))

# loneliness as category 
w2data <- w2data %>% 
  dplyr::rename(lonely_category = q31i)

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w2data <- w2data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                             (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                             marital == 5 ~ 3)) 
#w2data$mstat <- factor(w2data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

# living situation- if either of the items are larger than 1 = live with someone = 1, if = 1, not live with someone=0
w2data$live_alone <- ifelse(w2data$q74a>1 | w2data$q74b>1 | w2data$q74c>1| w2data$q74d>1
                            | w2data$q74e>1| w2data$q74f>1| w2data$q74g>1,
                            1,0)

# main and secondary occupations- recode to not employed/employed 
w2data$q54sc <- ifelse(is.na(w2data$q54sc),0,w2data$q54sc)
w2data$employ <- ifelse(((w2data$q54mn<1 | w2data$q54mn>3) & (w2data$q54sc<1 | w2data$q54sc>3)),
                        0,1)
#w2data$employ <- factor(w2data$employ,labels=c("Not employed","Employed"))

#w2check <- subset(w2data, select = c(activity_time,q51a,q51b,q51c))

# alcohol
w2data$q39 <- as.numeric(w2data$q39)
w2data$q40 <- as.numeric(w2data$q40)
w2data$q41 <- as.numeric(w2data$q41)

w2data <- w2data %>% mutate(alcfre = recode(q39, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))
w2data <- w2data %>% mutate(alcqnt = recode(q40, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))
w2data <- w2data %>% 
  dplyr::rename(alcbng = q41)

# depression and anxiety recode
w2data$b_depression_ever <- ifelse(w2data$q20p>=1,
                                   1,0)
w2data$b_anxiety_ever <- ifelse(w2data$q20q>=1,
                                1,0)

#w2check <- subset(w2data, select = c(q20p,depression_ever))
#View(w2check)

w2datat <- subset(w2data, select = c(idproj, wave, seifadis, mos_short, mos_long, lonely_category, smokst, pf, mh, gh, re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, employ, whobmigroup, mnstrs, alcfre, alcqnt, alcbng))

w2datab <- subset(w2data, select = c(idproj, b_depression_ever, b_anxiety_ever, b_heartdis_ever, b_stroke_ever, 
                                     b_cancer_ever))

save(w2datat,file=paste0(workdir,"loneliness and mortality w2t.RData"))
save(w2datab,file=paste0(workdir,"loneliness and mortality w2b.RData"))

######################################################################################
# 3. Wave 3 Data---------------
wha3midb <- read_sav(paste0(workdir,"wha3midb.sav"))
w3data <- wha3midb %>%
  select(idproj, m3mnsocsup6, m3q44i, m3smokst, m3pf, m3mh, m3gh, m3re, m3rp, m3sf, 
         m3bp, m3vt, m3pcsa, m3mcsa, m3ariapgp, m3marital,m3q71a, m3q71b, m3q71c, 
         m3q71e, m3q80A, m3q90a, m3whobmigroup, m3mnstrs, m3age, m3I_disAd, m3q35r,
         m3q35q)

w3data$wave <- 3
w3data <- w3data %>% 
  rename_all(~stringr::str_replace(.,"^m3",""))

w3data <- w3data %>% 
  dplyr::rename(seifadis = I_disAd)

# renaming MOS social support 6-items
w3data <- w3data %>% 
  dplyr::rename(mos_short = mnsocsup6)
# rename living alone
w3data <- w3data %>% 
  dplyr::rename(live_alone = q90a)

# loneliness as category 
w3data <- w3data %>% 
  dplyr::rename(lonely_category = q44i)

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w3data <- w3data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                           (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 
#w3data$mstat <- factor(w3data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

# employment
w3data$employ <- ifelse(w3data$q71a>1 | w3data$q71b>1 | w3data$q71c>1 | w3data$q71e>1,
                        1,0)
#w3data$employ <- factor(w3data$employ,labels=c("Not employed","Employed"))

# depression and anxiety in last 3 years no=0, yes=1
w3data <- w3data %>% 
  dplyr::rename(depression_3yr = q35q,
         anxiety_3yr = q35r)

#w3check <- subset(w3data, select = c(employ,q71a,q71b,q71c,q71e))

w3data <- subset(w3data, select = c(idproj, wave, seifadis, mos_short,
                                    lonely_category, live_alone, smokst, pf, mh, gh, 
                                    re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, employ, 
                                    whobmigroup, mnstrs, anxiety_3yr, depression_3yr))

save(w3data,file=paste0(workdir,"loneliness and mortality w3.RData"))
######################################################################################
# 4. Wave 4 Data------------------
wha4midb <- read_sav(paste0(workdir,"wha4midb.sav"))
w4data <- wha4midb %>%
  select(idproj, m4SeifaDis, m4mnsocsup6, m4q52i, m4pf, m4mh, m4gh, m4re, 
         m4rp, m4sf, m4bp, m4vt, m4pcsa, m4mcsa, m4mnstrs, m4smokst,
         m4age, m4ariapgp, m4marital, m4q89a, m4labf, m4q78A, m4q68a, m4q68c, 
         m4whobmigroup, m4q59, m4q60, m4q61, m4q32r, m4q32q, m4q90a, m4q90b, m4q90c, m4q90d, m4q90e, m4q90f, m4q90g, 
         m4q90h, m4q90i, m4q90j, m4q90k, m4q90l, m4q90m, m4q90n, m4q90o, m4q90p, m4q90q, m4q90r, m4q90s)

w4data$wave <- 4
w4data <- w4data %>% 
  rename_all(~stringr::str_replace(.,"^m4",""))

w4data <- w4data %>% 
  dplyr::rename(seifadis = SeifaDis)

# renaming MOS social support 6-items
w4data <- w4data %>% 
  dplyr::rename(mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w4data <- w4data %>%
  rowwise() %>%
  mutate(mos_long = mean(c(q90a, q90b, q90c, q90d, q90e, q90f, q90g, q90h, q90i, q90j, q90k, q90l, q90m, q90n, q90o, q90p, q90q, q90r, q90s)))

# rename living alone
w4data <- w4data %>% 
  dplyr::rename(live_alone = q89a)

# loneliness as category 
w4data <- w4data %>% 
  dplyr::rename(lonely_category = q52i)

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w4data <- w4data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                           (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 
#w4data$mstat <- factor(w4data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

# employment
w4data$employ <- ifelse(w4data$q68a>1 | w4data$q68c>1,
                        1,0)
#w4data$employ <- factor(w4data$employ,labels=c("Not employed","Employed"))         
    
# alcohol
w4data$q59 <- as.numeric(w4data$q59)
w4data$q60 <- as.numeric(w4data$q60)
w4data$q61 <- as.numeric(w4data$q61)

w4data <- w4data %>% mutate(alcfre = recode(q59, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w4data <- w4data %>% mutate(alcqnt = recode(q60, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))
w4data <- w4data %>% 
  dplyr::rename(alcbng = q61)

# depression and anxiety in last 3 years no=0, yes=1
w4data <- w4data %>% 
  dplyr::rename(depression_3yr = q32q,
         anxiety_3yr = q32r)

w4data <- subset(w4data, select = c(idproj, wave, seifadis, mos_short, mos_long,
                                    lonely_category, live_alone, smokst, pf, mh, gh, 
                                    re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, employ, 
                                    whobmigroup, mnstrs, alcfre, alcqnt,
                                    alcbng, depression_3yr, anxiety_3yr))

save(w4data,file=paste0(workdir,"loneliness and mortality w4.RData"))
######################################################################################
# 5. Wave 5 Data-------------------
wha5midb <- read_sav(paste0(workdir,"wha5midb.sav"))
w5data <- wha5midb %>%
  select(idproj, m5SeifaDis, m5mnsocsup6, m5q52i, m5bp, 
         m5gh, m5mh, m5pf, m5re, m5rp, m5sf, m5vt, m5pcsa, m5mcsa, m5mnstrs, m5smokst, 
         m5age, m5ariapgp, m5marital, m5q107a, m5labf, m5q94a, m5q85a, m5q85b, m5q85c, m5q85e, m5q59, 
         m5q60, m5q61, m5whobmigroup, m5q38r, m5q38q, m5q108a, m5q108b, m5q108c, m5q108d, m5q108e, m5q108f, m5q108g, 
         m5q108h, m5q108i, m5q108j, m5q108k, m5q108l, m5q108m, m5q108n, m5q108o, m5q108p, m5q108q, m5q108r, m5q108s)

w5data$wave <- 5
w5data <- w5data %>% 
  rename_all(~stringr::str_replace(.,"^m5",""))

w5data <- w5data %>% 
  dplyr::rename(seifadis = SeifaDis)

# renaming MOS social support 6-items
w5data <- w5data %>% 
  dplyr::rename(mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w5data <- w5data %>%
  rowwise() %>%
  mutate(mos_long = mean(c(q108a, q108b, q108c, q108d, q108e, q108f, q108g, q108h, q108i, q108j, q108k, q108l, q108m, q108n, q108o, q108p, q108q, q108r, q108s)))

# rename living alone
w5data <- w5data %>% 
  dplyr::rename(live_alone = q107a)

# loneliness as category 
w5data <- w5data %>% 
  dplyr::rename(lonely_category = q52i)

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w5data <- w5data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                           (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 
#w5data$mstat <- factor(w5data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

w5data$employ <- ifelse(w5data$q85a>1 | w5data$q85b>1 | w5data$q85c>1 | w5data$q85e>1,
                        1,0)
#w5data$employ <- factor(w5data$employ,labels=c("Not employed","Employed"))

# alcohol
w5data$q59 <- as.numeric(w5data$q59)
w5data$q60 <- as.numeric(w5data$q60)
w5data$q61 <- as.numeric(w5data$q61)

w5data <- w5data %>% mutate(alcfre = recode(q59, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w5data <- w5data %>% mutate(alcqnt = recode(q60, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))
w5data <- w5data %>% 
  dplyr::rename(alcbng = q61)

# depression and anxiety in last 3 years no=0, yes=1
w5data <- w5data %>% 
  dplyr::rename(depression_3yr = q38q,
         anxiety_3yr = q38r)

#w5check <- subset(w5data, select = c(smoke,smokst))
#View(w5check)

w5data <- subset(w5data, select = c(idproj, wave, seifadis, mos_short, mos_long, lonely_category, 
                                    live_alone, smokst, pf, mh, gh, re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, 
                                    employ, whobmigroup, mnstrs, alcfre, alcqnt, alcbng, depression_3yr, anxiety_3yr))

save(w5data,file=paste0(workdir,"loneliness and mortality w5.RData"))
######################################################################################
# 6. Wave 6 Data---------------
wha6midb <- read_sav(paste0(workdir,"wha6midb.sav"))
w6data <- wha6midb %>%
  select(idproj, m6mnsocsup6, m6Q52i,
         m6bp, m6gh, m6mh, m6pf, m6re, m6rp, m6sf, m6vt, m6pcsa, m6mcsa, m6mnstrs, 
         m6smokst, m6age, m6ariapgp, m6marital, m6Q108, m6Q111a, m6labf, 
         m6Q98a, m6Q84a, m6Q84b, m6Q84c, m6Q84e, m6SeifaDis, m6whobmigroup, m6Q60, m6Q61, m6Q62,
         m6Q38r, m6Q38q, m6Q112a, m6Q112b, m6Q112c, m6Q112d, m6Q112e, m6Q112f, m6Q112g, m6Q112h, m6Q112i, m6Q112j, m6Q112k, m6Q112l, 
         m6Q112m, m6Q112n, m6Q112o, m6Q112p, m6Q112q, m6Q112r, m6Q112s)

w6data$wave <- 6
w6data <- w6data %>% 
  rename_all(~stringr::str_replace(.,"^m6",""))

w6data <- w6data %>% 
  dplyr::rename(seifadis = SeifaDis)
# renaming MOS social support 6-items
w6data <- w6data %>% 
  dplyr::rename(mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w6data <- w6data %>%
  rowwise() %>%
  mutate(mos_long = mean(c(Q112a, Q112b, Q112c, Q112d, Q112e, Q112f, Q112g, Q112h, Q112i, Q112j, Q112k, Q112l, 
                           Q112m, Q112n, Q112o, Q112p, Q112q, Q112r, Q112s)))

# rename living alone
w6data <- w6data %>% 
  dplyr::rename(live_alone = Q111a)

# loneliness as category 
w6data <- w6data %>% 
  dplyr::rename(lonely_category = Q52i)

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w6data <- w6data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                           (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 
#w6data$mstat <- factor(w6data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

#Employment
w6data$employ <- ifelse(w6data$Q84a>1 | w6data$Q84b>1 | w6data$Q84c>1 | w6data$Q84e>1,
                        1,0)
#w6data$employ <- factor(w6data$employ,labels=c("Not employed","Employed"))

# alcohol
w6data$Q60 <- as.numeric(w6data$Q60)
w6data$Q61 <- as.numeric(w6data$Q61)
w6data$Q62 <- as.numeric(w6data$Q62)

w6data <- w6data %>% mutate(alcfre = recode(Q60, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w6data <- w6data %>% mutate(alcqnt = recode(Q61, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))
w6data <- w6data %>% 
  dplyr::rename(alcbng = Q62)

# depression and anxiety in last 3 years no=0, yes=1
w6data <- w6data %>% 
  dplyr::rename(depression_3yr = Q38q,
         anxiety_3yr = Q38r)

w6data <- subset(w6data, select = c(idproj, wave, seifadis, mos_short, mos_long,
                                    lonely_category, live_alone, pf, mh, gh, 
                                    re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, employ, 
                                    whobmigroup, mnstrs, alcfre, alcqnt,
                                    alcbng, smokst, depression_3yr, anxiety_3yr))

save(w6data,file=paste0(workdir,"loneliness and mortality w6.RData"))
######################################################################################
# 7. Wave 7 Data------------------
wha7midb <- read_sav(paste0(workdir,"wha7midb.sav"))
w7data <- wha7midb %>%
  select(idproj, m7mnsocsup6, m7q44i, m7bp, m7gh, m7mh, m7pf, 
         m7re, m7rp, m7sf, m7vt, m7pcsa, m7mcsa, m7mnstrs,
         m7smokst, m7age, m7ariapgp, m7marital, m7q101a, m7labf, m7q91a, m7q83a, m7q83b, 
         m7q83c, m7q83e, m7SeifaDis, m7whobmigroup, m7q51, m7q52, m7q53, m7q32dd, m7q32cc, m7q98a, m7q98b, m7q98c, m7q98d, m7q98e, 
         m7q98f, m7q98g, m7q98h, m7q98i, m7q98j, m7q98k, m7q98l, m7q98m, m7q98n, m7q98o, m7q98p, m7q98q, m7q98r, m7q98s)

w7data$wave <- 7
w7data <- w7data %>% 
  rename_all(~stringr::str_replace(.,"^m7",""))

w7data <- w7data %>% 
  dplyr::rename(seifadis = SeifaDis)

# renaming MOS social support 6-items
w7data <- w7data %>% 
  dplyr::rename(mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w7data <- w7data %>%
  rowwise() %>%
  mutate(mos_long = mean(c(q98a, q98b, q98c, q98d, q98e, q98f, q98g, q98h, q98i, q98j, q98k, q98l, q98m, q98n, q98o, q98p, q98q, q98r, 
                           q98s)))

# rename living alone
w7data <- w7data %>% 
  dplyr::rename(live_alone = q101a)

# loneliness as category 
w7data <- w7data %>% 
  dplyr::rename(lonely_category = q44i)


# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w7data <- w7data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                           (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 
#w7data$mstat <- factor(w7data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

w7data$employ <- ifelse(w7data$q83a>1 | w7data$q83b>1 | w7data$q83c>1 | w7data$q83e>1,
                        1,0)
#w7data$employ <- factor(w7data$employ,labels=c("Not employed","Employed"))

# alcohol
w7data$q51 <- as.numeric(w7data$q51)
w7data$q52 <- as.numeric(w7data$q52)
w7data$q53 <- as.numeric(w7data$q53)

w7data <- w7data %>% mutate(alcfre = recode(q51, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w7data <- w7data %>% mutate(alcqnt = recode(q52, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))
w7data <- w7data %>% 
  dplyr::rename(alcbng = q53)

# depression and anxiety in last 3 years no=0, yes=1
w7data <- w7data %>% 
  dplyr::rename(depression_3yr = q32cc,
         anxiety_3yr = q32dd)

w7data <- subset(w7data, select = c(idproj, wave, seifadis, mos_short, mos_long,
                                    lonely_category, live_alone, smokst, pf, mh, gh, 
                                    re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, employ, 
                                    whobmigroup, mnstrs, alcfre, alcqnt,
                                    alcbng, depression_3yr, anxiety_3yr))

save(w7data,file=paste0(workdir,"loneliness and mortality w7.RData"))
######################################################################################
# 8. Wave 8 Data----------------
wha8midb <- read_sav(paste0(workdir,"wha8midb.sav"))
w8data <- wha8midb %>%
  select(idproj, m8mnsocsup6, M8Q43i, m8bp,
         m8gh, m8mh, m8pf, m8re, m8rp, m8sf, m8vt, m8pcs_abs, m8mcs_abs, m8mnstrs, m8smokst,
         M8AGE, m8ariapgp, m8marital, M8Q96a, m8labf, M8Q75, M8Q66a, M8Q66b, M8Q66c,
         M8Q66e, m8SeifaDis, m8whobmigroup, M8Q51, M8Q52, M8Q53, M8Q30dd, M8Q30cc, M8Q93a, M8Q93b, M8Q93c, M8Q93d, M8Q93e, M8Q93f, 
         M8Q93g, M8Q93h, M8Q93i, M8Q93j, M8Q93k, M8Q93l, M8Q93m, M8Q93n, M8Q93o, M8Q93p, M8Q93q, M8Q93r, M8Q93s)

w8data$wave <- 8
w8data <- w8data %>% 
  rename_all(~stringr::str_replace(.,"^M8",""))
w8data <- w8data %>% 
  rename_all(~stringr::str_replace(.,"^m8",""))

w8data <- w8data %>% 
  dplyr::rename(seifadis = SeifaDis)
w8data <- w8data %>% 
  dplyr::rename(pcsa = pcs_abs,
         mcsa = mcs_abs)

# renaming MOS social support 6-items
w8data <- w8data %>% 
  dplyr::rename(mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w8data <- w8data %>%
  rowwise() %>%
  mutate(mos_long = mean(c(Q93a, Q93b, Q93c, Q93d, Q93e, Q93f, Q93g, Q93h, Q93i, Q93j, Q93k, Q93l, 
                           Q93m, Q93n, Q93o, Q93p, Q93q, Q93r, Q93s)))

# rename living alone
w8data <- w8data %>% 
  dplyr::rename(live_alone = Q96a)

w8data <- w8data %>% 
  dplyr::rename(age = AGE)

# loneliness as category 
w8data <- w8data %>% 
  dplyr::rename(lonely_category = Q43i)

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w8data <- w8data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                           (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 
#w8data$mstat <- factor(w8data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

w8data$employ <- ifelse(w8data$Q66a>1 | w8data$Q66b>1 | w8data$Q66c>1 | w8data$Q66e>1,
                        1,0)
#w8data$employ <- factor(w8data$employ,labels=c("Not employed","Employed"))

# alcohol
w8data$Q51 <- as.numeric(w8data$Q51)
w8data$Q52 <- as.numeric(w8data$Q52)
w8data$Q53 <- as.numeric(w8data$Q53)

w8data <- w8data %>% mutate(alcfre = recode(Q51, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w8data <- w8data %>% mutate(alcqnt = recode(Q52, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))
w8data <- w8data %>% 
  dplyr::rename(alcbng = Q53)

# depression and anxiety in last 3 years no=0, yes=1
w8data <- w8data %>% 
  dplyr::rename(depression_3yr = Q30cc,
         anxiety_3yr = Q30dd)

w8data <- subset(w8data, select = c(idproj, wave, seifadis, mos_short, mos_long,
                                    lonely_category, live_alone, smokst, pf, mh, gh, 
                                    re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, employ, 
                                    whobmigroup, mnstrs, alcfre, alcqnt,
                                    alcbng, depression_3yr, anxiety_3yr))

save(w8data,file=paste0(workdir,"loneliness and mortality w8.RData"))

######################################################################################
# 9. Wave 9 Data------------------
wha9midb <- read_sav(paste0(workdir,"wha9midb.sav"))
w9data <- wha9midb %>%
  select(idproj, m9mnsocsup6, m9q51i,
         m9bp, m9gh, m9mh, m9pf, m9re, m9rp, m9sf, m9vt, m9pcs_abs, m9mcs_abs, m9mnstrs,
         m9smokst, m9age, m9ariapgp, m9marital, m9q112a, m9labf, m9q75a, m9q75b,
         m9q75c, m9q75e, m9whobmigroup, m9q62, m9q63, m9q64, m9q33ll, m9q33kk, m9q108a, m9q108b, m9q108c, 
         m9q108d, m9q108e, m9q108f, m9q108g, m9q108h, m9q108i, m9q108j, m9q108k, m9q108l, 
         m9q108m, m9q108n, m9q108o, m9q108p, m9q108q, m9q108r, m9q108s)
         
w9data <- w9data %>% 
  rename_all(~stringr::str_replace(.,"^m9",""))        
w9data$wave <- 9

w9data <- w9data %>% 
  dplyr::rename(pcsa = pcs_abs,
         mcsa = mcs_abs)

# renaming MOS social support 6-items- higher score- higher social support
w9data <- w9data %>% 
  dplyr::rename(mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w9data <- w9data %>%
  rowwise() %>%
  mutate(mos_long = mean(c(q108a, q108b, q108c, q108d, q108e, q108f, q108g, q108h, q108i, q108j, q108k, q108l, 
                           q108m, q108n, q108o, q108p, q108q, q108r, q108s)))

# rename living alone
w9data <- w9data %>% 
  dplyr::rename(live_alone = q112a)

# loneliness as category 
w9data <- w9data %>% 
  mutate(lonely_category = case_when
         (q51i == 1 ~ 0,
           q51i == 2 ~ 1,
           q51i == 3 ~ 2,
           q51i == 4 ~ 3))

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w9data <- w9data %>%
  mutate(mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                           (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                           marital == 5 ~ 3)) 
#w9data$mstat <- factor(w9data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

w9data$employ <- ifelse(w9data$q75a>1 | w9data$q75b>1 | w9data$q75c>1 | w9data$q75e>1,
                        1,0)
#w9data$employ <- factor(w9data$employ,labels=c("Not employed","Employed"))         
         
# alcohol
w9data$q62 <- as.numeric(w9data$q62)
w9data$q63 <- as.numeric(w9data$q63)
w9data$q64 <- as.numeric(w9data$q64)

w9data <- w9data %>% mutate(alcfre = recode(q62, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1.5,
                                            `5` = 3.5,
                                            `6` = 5.5,
                                            `7` = 7))

w9data <- w9data %>% mutate(alcqnt = recode(q63, 
                                            `1` = 1.5,
                                            `2` = 3.5,
                                            `3` = 6.5,
                                            `4` = 9))
w9data <- w9data %>% 
  dplyr::rename(alcbng = q64)

# depression and anxiety in last 3 years no=0, yes=1
w9data <- w9data %>% 
  dplyr::rename(depression_3yr = q33kk,
         anxiety_3yr = q33ll)

w9data <- subset(w9data, select = c(idproj, wave, mos_short, mos_long, lonely_category, live_alone, smokst, pf, mh, gh, re, rp, sf, bp, vt, pcsa, mcsa, age, ariapgp, mstat, live_alone, employ, 
                                    whobmigroup, mnstrs, alcfre, alcqnt,
                                    alcbng, depression_3yr, anxiety_3yr))
save(w9data,file=paste0(workdir,"loneliness and mortality w9.RData"))         
 
#w9check <- subset(w9data, select = c(employ,q75a,q75b,q75c,q75e))

######################################################################################
# mortality- time of death-----------------------------------------------------
deathdata <- read_sav(paste0(workdir,"recentmidstatus.sav"))

deathdata$death_1 <- 0

deathdata$death_2 <- ifelse(deathdata$deathdate>deathdata$Phase1DateSurveyReturned & 
                              is.na(deathdata$Phase2DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_2 <- ifelse(deathdata$death_1==1,1,deathdata$death_2)

deathdata$death_3 <- ifelse(deathdata$deathdate>deathdata$Phase2DateSurveyReturned & 
                              is.na(deathdata$Phase3DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_3 <- ifelse(deathdata$death_2==1,1,deathdata$death_3)

deathdata$death_4 <- ifelse(deathdata$deathdate>deathdata$Phase3DateSurveyReturned & 
                              is.na(deathdata$Phase4DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_4 <- ifelse(deathdata$death_3==1,1,deathdata$death_4)

deathdata$death_5 <- ifelse(deathdata$deathdate>deathdata$Phase4DateSurveyReturned & 
                              is.na(deathdata$Phase5DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_5 <- ifelse(deathdata$death_4==1,1,deathdata$death_5)

deathdata$death_6 <- ifelse(deathdata$deathdate>deathdata$Phase5DateSurveyReturned & 
                              is.na(deathdata$Phase6DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_6 <- ifelse(deathdata$death_5==1,1,deathdata$death_6)

deathdata$death_7 <- ifelse(deathdata$deathdate>deathdata$Phase6DateSurveyReturned & 
                              is.na(deathdata$Phase7DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_7 <- ifelse(deathdata$death_6==1,1,deathdata$death_7)

deathdata$death_8 <- ifelse(deathdata$deathdate>deathdata$Phase7DateSurveyReturned & 
                              is.na(deathdata$Phase8DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_8 <- ifelse(deathdata$death_7==1,1,deathdata$death_8)

deathdata$death_9 <- ifelse(deathdata$deathdate>deathdata$Phase8DateSurveyReturned & 
                              is.na(deathdata$Phase9DateSurveyReturned) &
                              !is.na(deathdata$deathdate),1,0)
deathdata$death_9 <- ifelse(deathdata$death_8==1,1,deathdata$death_9)

deathdata$censored_1 <- 1
deathdata$censored_2 <- ifelse(deathdata$Attrition12>1 & deathdata$Attrition12<6,0,1)
deathdata$censored_3 <- ifelse(deathdata$Attrition13>1 & deathdata$Attrition13<6,0,1)
deathdata$censored_4 <- ifelse(deathdata$Attrition14>1 & deathdata$Attrition14<6,0,1)
deathdata$censored_5 <- ifelse(deathdata$Attrition15>1 & deathdata$Attrition15<6,0,1)
deathdata$censored_6 <- ifelse(deathdata$Attrition16>1 & deathdata$Attrition16<6,0,1)
deathdata$censored_7 <- ifelse(deathdata$Attrition17>1 & deathdata$Attrition17<6,0,1)
deathdata$censored_8 <- ifelse(deathdata$Attrition18>1 & deathdata$Attrition18<6,0,1)
deathdata$censored_9 <- ifelse(deathdata$Attrition19>1 & deathdata$Attrition19<6,0,1)

deathdata <- as.data.frame(deathdata)

# Pass over waves to treat cases as retained if they completed a subsequent interview
c_vars <- c("censored_2","censored_3","censored_4","censored_5","censored_6","censored_7","censored_8","censored_9")
for (i in 7:1) {
  deathdata[,c_vars[i]] <- ifelse(deathdata[,c_vars[i+1]]==1,1,deathdata[,c_vars[i]])
}

deathdata <- subset(deathdata, select = c(idproj,censored_1,censored_2,censored_3,censored_4,censored_5,censored_6,censored_7,censored_8,censored_9,
                                          death_1,death_2,death_3,death_4,death_5,death_6,death_7,death_8,death_9))

deathdata <- deathdata %>%
  pivot_longer(
    cols = censored_1:death_9,
    names_to = c(".value", "wave"),
    names_pattern = "([A-Za-z]+)_(\\d+)"
  )
deathdata$wave <- as.numeric(deathdata$wave)

save(deathdata,file=paste0(workdir,"death data.RData"))
