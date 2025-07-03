######################################################################################
##   
## Effects of physical activity on incident obesity
## Descriptive sample statistics for Table 1 etc
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
libs <- c("plyr","dplyr","ltmle","gtools","furniture","flextable","openxlsx","table1","survey")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load and process data 
#-------------------------------------------------------------------------------------

imp <- readRDS(file=paste0(workdir,"Data/imputed data - long form.rds"))
raw_data <- imp[[26]]
rm(imp)

raw_data$alcliferisk <- ifelse(raw_data$alcfq>10,1,0)
raw_data$alcepisrisk <- ifelse(raw_data$alcbng>1,1,0)
raw_data$activity_bin <- ifelse(raw_data$weighted_activity_time>=150,1,0)
raw_data$activity_cat <- ifelse(raw_data$weighted_activity_time>=300,2,raw_data$activity_bin)

raw_data$bmi <- raw_data$wtkg/((raw_data$htcm/100)^2)
raw_data$obesity <- ifelse(raw_data$bmi>=30,1,0)
raw_data$obesity_sev <- ifelse(raw_data$bmi>=35,1,0)


raw_data <- subset(raw_data, select = -c(alcfq,alcbng,weighted_activity_time,inarea,b_hypert_ever,hypert_3yr,wtkg,htcm,whobmigroup))

raw_data[,c("alcliferisk","alcepisrisk","b_heartdis_ever","b_stroke_ever","b_cancer_ever","b_depression_ever","b_anxiety_ever")] <- lapply(raw_data[,c("alcliferisk","alcepisrisk","b_heartdis_ever","b_stroke_ever","b_cancer_ever","b_depression_ever","b_anxiety_ever")], factor, labels=c("No","Yes"))

raw_data$seifa_bin <- ifelse(raw_data$seifadis>5, 1, 0)
raw_data$seifa_bin <- factor(raw_data$seifa_bin,
                             labels = c("Bottom 5 quintiles","Top 5 quintiles"))


raw_data <- raw_data[order(raw_data$wave),]

table(raw_data$wave)

raw_data <- reshape(raw_data,
             timevar=c("wave"), 
             idvar=c("idproj"),
             v.names=c("death","obesity","obesity_sev",
                       "activity_bin","activity_cat",
                       "marital","age","ariapgp","employ",
                       "live_u18","live_o18","seifadis","seifa_bin",
                       "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                       "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst",
                       "menopause","hrt","sleep_cat","sleep_prob",
                       "finfinc","pcsa","mcsa","bmi"),
             sep = "",
             dir="wide")

raw_data$b_bmi <- raw_data$bmi2

raw_data <- reshape(raw_data,
             timevar=c("wave"), 
             idvar=c("idproj"),
             v.names=c("death","obesity","obesity_sev",
                       "activity_bin","activity_cat",
                       "marital","age","ariapgp","employ",
                       "live_u18","live_o18","seifadis","seifa_bin",
                       "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                       "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst",
                       "menopause","hrt","sleep_cat","sleep_prob",
                       "finfinc","pcsa","mcsa","bmi"),
             sep = "",
             dir="long")

raw_data$weight_gain_a <- ifelse((raw_data$bmi/raw_data$b_bmi)>1.05,1,0)
raw_data$weight_gain_b <- ifelse((raw_data$bmi/raw_data$b_bmi)>1.1,1,0)

raw_data <- reshape(raw_data,
                    timevar=c("wave"), 
                    idvar=c("idproj"),
                    v.names=c(
                      "death","obesity","obesity_sev","weight_gain_a","weight_gain_b",
                      "activity_bin","activity_cat",
                      "marital","age","ariapgp","employ",
                      "live_u18","live_o18","seifadis","seifa_bin",
                      "heartdis_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
                      "cesd10","mnstrs","vegetables","fruit","alcliferisk","alcepisrisk","smokst",
                      "menopause","hrt","sleep_cat","sleep_prob",
                      "finfinc","pcsa","mcsa","bmi"),
                    sep = "",
                    dir="wide")

raw_data <- raw_data %>%
  mutate(obesity5 = ifelse(obesity4 == 1, 1, obesity5),
         obesity6 = ifelse(obesity5 == 1, 1, obesity6),
         obesity7 = ifelse(obesity6 == 1, 1, obesity7),
         obesity8 = ifelse(obesity7 == 1, 1, obesity8),
         obesity9 = ifelse(obesity8 == 1, 1, obesity9),
         obesity10 = ifelse(obesity9 == 1, 1, obesity10),
         obesity_sev5 = ifelse(obesity_sev4 == 1, 1, obesity_sev5),
         obesity_sev6 = ifelse(obesity_sev5 == 1, 1, obesity_sev6),
         obesity_sev7 = ifelse(obesity_sev6 == 1, 1, obesity_sev7),
         obesity_sev8 = ifelse(obesity_sev7 == 1, 1, obesity_sev8),
         obesity_sev9 = ifelse(obesity_sev8 == 1, 1, obesity_sev9),
         obesity_sev10 = ifelse(obesity_sev9 == 1, 1, obesity_sev10),
         weight_gain_a5 = ifelse(weight_gain_a4 == 1, 1, weight_gain_a5),
         weight_gain_a6 = ifelse(weight_gain_a5 == 1, 1, weight_gain_a6),
         weight_gain_a7 = ifelse(weight_gain_a6 == 1, 1, weight_gain_a7),
         weight_gain_a8 = ifelse(weight_gain_a7 == 1, 1, weight_gain_a8),
         weight_gain_a9 = ifelse(weight_gain_a8 == 1, 1, weight_gain_a9),
         weight_gain_a10 = ifelse(weight_gain_a9 == 1, 1, weight_gain_a10),
         weight_gain_b5 = ifelse(weight_gain_b4 == 1, 1, weight_gain_b5),
         weight_gain_b6 = ifelse(weight_gain_b5 == 1, 1, weight_gain_b6),
         weight_gain_b7 = ifelse(weight_gain_b6 == 1, 1, weight_gain_b7),
         weight_gain_b8 = ifelse(weight_gain_b7 == 1, 1, weight_gain_b8),
         weight_gain_b9 = ifelse(weight_gain_b8 == 1, 1, weight_gain_b9),
         weight_gain_b10 = ifelse(weight_gain_b9 == 1, 1, weight_gain_b10))

svy <- svydesign(ids = ~idproj,
                 weights = ~1,
                 data = raw_data)

unweighted_table <- matrix(rbind(cbind(svymean(~age2,svy,na.rm=TRUE),SE(svymean(~age2,svy,na.rm=TRUE))*sqrt(4779)),
                                 cbind(svytotal(~I(employ2=="Employed"),svy,na.rm=TRUE),
                                       svymean(~I(employ2=="Employed"),svy,na.rm=TRUE)),
                                 rbind(cbind(svytotal(~I(marital2=="Married/de facto"),svy,na.rm=TRUE)[2],
                                             svymean(~I(marital2=="Married/de facto"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(marital2=="Separated/divorced/never married"),svy,na.rm=TRUE)[2],
                                             svymean(~I(marital2=="Separated/divorced/never married"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(marital2=="Widowed"),svy,na.rm=TRUE)[2],
                                             svymean(~I(marital2=="Widowed"),svy,na.rm=TRUE)[2])),
                                 cbind(svytotal(~I(live_u182=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(live_u182=="Yes"),svy,na.rm=TRUE)),
                                 cbind(svytotal(~I(live_o182=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(live_o182=="Yes"),svy,na.rm=TRUE)),
                                 rbind(cbind(svytotal(~I(seifa_bin2=="Bottom 5 quintiles"),svy,na.rm=TRUE)[2],
                                             svymean(~I(seifa_bin2=="Bottom 5 quintiles"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(seifa_bin2=="Top 5 quintiles"),svy,na.rm=TRUE)[2],
                                             svymean(~I(seifa_bin2=="Top 5 quintiles"),svy,na.rm=TRUE)[2])),
                                 rbind(cbind(svytotal(~I(ariapgp2=="Major city"),svy,na.rm=TRUE)[2],
                                             svymean(~I(ariapgp2=="Major city"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(ariapgp2=="Regional"),svy,na.rm=TRUE)[2],
                                             svymean(~I(ariapgp2=="Regional"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(ariapgp2=="Remote"),svy,na.rm=TRUE)[2],
                                             svymean(~I(ariapgp2=="Remote"),svy,na.rm=TRUE)[2])),
                                 cbind(svytotal(~I(alcliferisk2=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(alcliferisk2=="Yes"),svy,na.rm=TRUE)),
                                 cbind(svytotal(~I(alcepisrisk2=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(alcepisrisk2=="Yes"),svy,na.rm=TRUE)),
                                 rbind(cbind(svytotal(~I(smokst2=="Never smoker"),svy,na.rm=TRUE)[2],
                                             svymean(~I(smokst2=="Never smoker"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(smokst2=="Ex smoker"),svy,na.rm=TRUE)[2],
                                             svymean(~I(smokst2=="Ex smoker"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(smokst2=="Current smoker"),svy,na.rm=TRUE)[2],
                                             svymean(~I(smokst2=="Current smoker"),svy,na.rm=TRUE)[2])),
                                 cbind(svymean(~cesd102,svy,na.rm=TRUE),SE(svymean(~cesd102,svy,na.rm=TRUE))*sqrt(4779)),
                                 cbind(svymean(~mnstrs2,svy,na.rm=TRUE),SE(svymean(~mnstrs2,svy,na.rm=TRUE))*sqrt(4779)),
                                 cbind(svymean(~bmi2,svy,na.rm=TRUE),SE(svymean(~bmi2,svy,na.rm=TRUE))*sqrt(4779)),
                                 rbind(cbind(svytotal(~I(menopause2=="Pre"),svy,na.rm=TRUE)[2],
                                             svymean(~I(menopause2=="Pre"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(menopause2=="Peri/post"),svy,na.rm=TRUE)[2],
                                             svymean(~I(menopause2=="Peri/post"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(menopause2=="Surgical"),svy,na.rm=TRUE)[2],
                                             svymean(~I(menopause2=="Surgical"),svy,na.rm=TRUE)[2])),
                                 rbind(cbind(svytotal(~I(hrt2=="Yes"),svy,na.rm=TRUE),
                                             svymean(~I(hrt2=="Yes"),svy,na.rm=TRUE))),
                                 cbind(svytotal(~I(b_heartdis_ever=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(b_heartdis_ever=="Yes"),svy,na.rm=TRUE)),
                                 cbind(svytotal(~I(b_stroke_ever=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(b_stroke_ever=="Yes"),svy,na.rm=TRUE)),
                                 cbind(svytotal(~I(b_cancer_ever=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(b_cancer_ever=="Yes"),svy,na.rm=TRUE)),
                                 cbind(svytotal(~I(b_depression_ever=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(b_depression_ever=="Yes"),svy,na.rm=TRUE)),
                                 cbind(svytotal(~I(b_anxiety_ever=="Yes"),svy,na.rm=TRUE),
                                       svymean(~I(b_anxiety_ever=="Yes"),svy,na.rm=TRUE)),
                                 cbind(svytotal(~I(b_cobcat=="Other"),svy,na.rm=TRUE),
                                       svymean(~I(b_cobcat=="Other"),svy,na.rm=TRUE)),
                                 rbind(cbind(svytotal(~I(b_educ=="Less than high school"),svy,na.rm=TRUE)[2],
                                             svymean(~I(b_educ=="Less than high school"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(b_educ=="Trade/apprentice/certificate/diploma"),svy,na.rm=TRUE)[2],
                                             svymean(~I(b_educ=="Trade/apprentice/certificate/diploma"),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(b_educ=="University"),svy,na.rm=TRUE)[2],
                                             svymean(~I(b_educ=="University"),svy,na.rm=TRUE)[2])),
                                 cbind(svymean(~b_pcsa,svy,na.rm=TRUE),SE(svymean(~b_pcsa,svy,na.rm=TRUE))*sqrt(4779)),
                                 cbind(svymean(~b_mcsa,svy,na.rm=TRUE),SE(svymean(~b_mcsa,svy,na.rm=TRUE))*sqrt(4779)),
                                 rbind(cbind(svytotal(~I(sleep_cat2==1 | sleep_cat2==2),svy,na.rm=TRUE)[2],
                                             svymean(~I(sleep_cat2==1 | sleep_cat2==2),svy,na.rm=TRUE)[2]),
                                       cbind(svytotal(~I(sleep_cat2==3 | sleep_cat2==4),svy,na.rm=TRUE)[2],
                                             svymean(~I(sleep_cat2==3 | sleep_cat2==4),svy,na.rm=TRUE)[2]))),ncol=2)

svy <- svydesign(ids = ~idproj,
                 weights = ~b_wtarea,
                 data = raw_data)

weighted_table <- matrix(rbind(cbind(svymean(~age2,svy,na.rm=TRUE),SE(svymean(~age2,svy,na.rm=TRUE))*sqrt(4779)),
                               cbind(svytotal(~I(employ2=="Employed"),svy,na.rm=TRUE),
                                     svymean(~I(employ2=="Employed"),svy,na.rm=TRUE)),
                               rbind(cbind(svytotal(~I(marital2=="Married/de facto"),svy,na.rm=TRUE)[2],
                                           svymean(~I(marital2=="Married/de facto"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(marital2=="Separated/divorced/never married"),svy,na.rm=TRUE)[2],
                                           svymean(~I(marital2=="Separated/divorced/never married"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(marital2=="Widowed"),svy,na.rm=TRUE)[2],
                                           svymean(~I(marital2=="Widowed"),svy,na.rm=TRUE)[2])),
                               cbind(svytotal(~I(live_u182=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(live_u182=="Yes"),svy,na.rm=TRUE)),
                               cbind(svytotal(~I(live_o182=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(live_o182=="Yes"),svy,na.rm=TRUE)),
                               rbind(cbind(svytotal(~I(seifa_bin2=="Bottom 5 quintiles"),svy,na.rm=TRUE)[2],
                                           svymean(~I(seifa_bin2=="Bottom 5 quintiles"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(seifa_bin2=="Top 5 quintiles"),svy,na.rm=TRUE)[2],
                                           svymean(~I(seifa_bin2=="Top 5 quintiles"),svy,na.rm=TRUE)[2])),
                               rbind(cbind(svytotal(~I(ariapgp2=="Major city"),svy,na.rm=TRUE)[2],
                                           svymean(~I(ariapgp2=="Major city"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(ariapgp2=="Regional"),svy,na.rm=TRUE)[2],
                                           svymean(~I(ariapgp2=="Regional"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(ariapgp2=="Remote"),svy,na.rm=TRUE)[2],
                                           svymean(~I(ariapgp2=="Remote"),svy,na.rm=TRUE)[2])),
                               cbind(svytotal(~I(alcliferisk2=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(alcliferisk2=="Yes"),svy,na.rm=TRUE)),
                               cbind(svytotal(~I(alcepisrisk2=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(alcepisrisk2=="Yes"),svy,na.rm=TRUE)),
                               rbind(cbind(svytotal(~I(smokst2=="Never smoker"),svy,na.rm=TRUE)[2],
                                           svymean(~I(smokst2=="Never smoker"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(smokst2=="Ex smoker"),svy,na.rm=TRUE)[2],
                                           svymean(~I(smokst2=="Ex smoker"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(smokst2=="Current smoker"),svy,na.rm=TRUE)[2],
                                           svymean(~I(smokst2=="Current smoker"),svy,na.rm=TRUE)[2])),
                               cbind(svymean(~cesd102,svy,na.rm=TRUE),SE(svymean(~cesd102,svy,na.rm=TRUE))*sqrt(4779)),
                               cbind(svymean(~mnstrs2,svy,na.rm=TRUE),SE(svymean(~mnstrs2,svy,na.rm=TRUE))*sqrt(4779)),
                               cbind(svymean(~bmi2,svy,na.rm=TRUE),SE(svymean(~bmi2,svy,na.rm=TRUE))*sqrt(4779)),
                               rbind(cbind(svytotal(~I(menopause2=="Pre"),svy,na.rm=TRUE)[2],
                                           svymean(~I(menopause2=="Pre"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(menopause2=="Peri/post"),svy,na.rm=TRUE)[2],
                                           svymean(~I(menopause2=="Peri/post"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(menopause2=="Surgical"),svy,na.rm=TRUE)[2],
                                           svymean(~I(menopause2=="Surgical"),svy,na.rm=TRUE)[2])),
                               rbind(cbind(svytotal(~I(hrt2=="Yes"),svy,na.rm=TRUE),
                                           svymean(~I(hrt2=="Yes"),svy,na.rm=TRUE))),
                               cbind(svytotal(~I(b_heartdis_ever=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(b_heartdis_ever=="Yes"),svy,na.rm=TRUE)),
                               cbind(svytotal(~I(b_stroke_ever=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(b_stroke_ever=="Yes"),svy,na.rm=TRUE)),
                               cbind(svytotal(~I(b_cancer_ever=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(b_cancer_ever=="Yes"),svy,na.rm=TRUE)),
                               cbind(svytotal(~I(b_depression_ever=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(b_depression_ever=="Yes"),svy,na.rm=TRUE)),
                               cbind(svytotal(~I(b_anxiety_ever=="Yes"),svy,na.rm=TRUE),
                                     svymean(~I(b_anxiety_ever=="Yes"),svy,na.rm=TRUE)),
                               cbind(svytotal(~I(b_cobcat=="Other"),svy,na.rm=TRUE),
                                     svymean(~I(b_cobcat=="Other"),svy,na.rm=TRUE)),
                               rbind(cbind(svytotal(~I(b_educ=="Less than high school"),svy,na.rm=TRUE)[2],
                                           svymean(~I(b_educ=="Less than high school"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(b_educ=="Trade/apprentice/certificate/diploma"),svy,na.rm=TRUE)[2],
                                           svymean(~I(b_educ=="Trade/apprentice/certificate/diploma"),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(b_educ=="University"),svy,na.rm=TRUE)[2],
                                           svymean(~I(b_educ=="University"),svy,na.rm=TRUE)[2])),
                               cbind(svymean(~b_pcsa,svy,na.rm=TRUE),SE(svymean(~b_pcsa,svy,na.rm=TRUE))*sqrt(4779)),
                               cbind(svymean(~b_mcsa,svy,na.rm=TRUE),SE(svymean(~b_mcsa,svy,na.rm=TRUE))*sqrt(4779)),
                               rbind(cbind(svytotal(~I(sleep_cat2==1 | sleep_cat2==2),svy,na.rm=TRUE)[2],
                                           svymean(~I(sleep_cat2==1 | sleep_cat2==2),svy,na.rm=TRUE)[2]),
                                     cbind(svytotal(~I(sleep_cat2==3 | sleep_cat2==4),svy,na.rm=TRUE)[2],
                                           svymean(~I(sleep_cat2==3 | sleep_cat2==4),svy,na.rm=TRUE)[2]))),ncol=2)

combined <- cbind(unweighted_table,weighted_table)

wb <- createWorkbook()
addWorksheet(wb,"Table")
writeData(wb,sheet="Table",combined,startRow=1)
saveWorkbook(wb, file = paste0(workdir,"Results/descriptives_table.xlsx"), overwrite = TRUE)

weighted_table2 <- cbind(matrix(rbind(cbind(svymean(~bmi4,svy,na.rm=TRUE),SE(svymean(~bmi4,svy,na.rm=TRUE))*sqrt(table(!is.na(raw_data$bmi4))[2])),
                                      cbind(svymean(~bmi5,svy,na.rm=TRUE),SE(svymean(~bmi5,svy,na.rm=TRUE))*sqrt(table(!is.na(raw_data$bmi5))[2])),
                                      cbind(svymean(~bmi6,svy,na.rm=TRUE),SE(svymean(~bmi6,svy,na.rm=TRUE))*sqrt(table(!is.na(raw_data$bmi6))[2])),
                                      cbind(svymean(~bmi7,svy,na.rm=TRUE),SE(svymean(~bmi7,svy,na.rm=TRUE))*sqrt(table(!is.na(raw_data$bmi7))[2])),
                                      cbind(svymean(~bmi8,svy,na.rm=TRUE),SE(svymean(~bmi8,svy,na.rm=TRUE))*sqrt(table(!is.na(raw_data$bmi8))[2])),
                                      cbind(svymean(~bmi9,svy,na.rm=TRUE),SE(svymean(~bmi9,svy,na.rm=TRUE))*sqrt(table(!is.na(raw_data$bmi9))[2])),
                                      cbind(svymean(~bmi10,svy,na.rm=TRUE),SE(svymean(~bmi10,svy,na.rm=TRUE))*sqrt(table(!is.na(raw_data$bmi10))[2]))),ncol=2),
                         matrix(rbind(svymean(~obesity4,svy,na.rm=TRUE),
                                      svymean(~obesity5,svy,na.rm=TRUE),
                                      svymean(~obesity6,svy,na.rm=TRUE),
                                      svymean(~obesity7,svy,na.rm=TRUE),
                                      svymean(~obesity8,svy,na.rm=TRUE),
                                      svymean(~obesity9,svy,na.rm=TRUE),
                                      svymean(~obesity10,svy,na.rm=TRUE)),ncol=1),
                         matrix(rbind(svymean(~obesity_sev4,svy,na.rm=TRUE),
                                      svymean(~obesity_sev5,svy,na.rm=TRUE),
                                      svymean(~obesity_sev6,svy,na.rm=TRUE),
                                      svymean(~obesity_sev7,svy,na.rm=TRUE),
                                      svymean(~obesity_sev8,svy,na.rm=TRUE),
                                      svymean(~obesity_sev9,svy,na.rm=TRUE),
                                      svymean(~obesity_sev10,svy,na.rm=TRUE)),ncol=1),
                         matrix(rbind(svymean(~weight_gain_a4,svy,na.rm=TRUE),
                                      svymean(~weight_gain_a5,svy,na.rm=TRUE),
                                      svymean(~weight_gain_a6,svy,na.rm=TRUE),
                                      svymean(~weight_gain_a7,svy,na.rm=TRUE),
                                      svymean(~weight_gain_a8,svy,na.rm=TRUE),
                                      svymean(~weight_gain_a9,svy,na.rm=TRUE),
                                      svymean(~weight_gain_a10,svy,na.rm=TRUE)),ncol=1),
                         matrix(rbind(svymean(~weight_gain_b4,svy,na.rm=TRUE),
                                      svymean(~weight_gain_b5,svy,na.rm=TRUE),
                                      svymean(~weight_gain_b6,svy,na.rm=TRUE),
                                      svymean(~weight_gain_b7,svy,na.rm=TRUE),
                                      svymean(~weight_gain_b8,svy,na.rm=TRUE),
                                      svymean(~weight_gain_b9,svy,na.rm=TRUE),
                                      svymean(~weight_gain_b10,svy,na.rm=TRUE)),ncol=1))

wb <- createWorkbook()
addWorksheet(wb,"Table")
writeData(wb,sheet="Table",weighted_table2,startRow=1)
saveWorkbook(wb, file = paste0(workdir,"Results/outcome_descriptives.xlsx"), overwrite = TRUE)