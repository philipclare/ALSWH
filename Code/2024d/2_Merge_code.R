######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Merge data into combined, wide-form data, ready for imputation
## Date: 24 July 2023
## Authors: Philip Clare and Neta Hagani
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration:https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-Loneliness_ALSWH/"

libs <- c("haven","plyr","dplyr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load individual wave data and merge into long form
#-------------------------------------------------------------------------------------
load(file=paste0(workdir,"Data/loneliness and mortality w1t.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w1b.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w2t.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w2b.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w3.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w4.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w5.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w6.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w7.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w8.RData"))
load(file=paste0(workdir,"Data/loneliness and mortality w9.RData"))
load(file=paste0(workdir,"Data/death data.RData"))

#turn the longitudinal data to long format
long_data <- rbind.fill(w2datat, w3data, w4data, w5data, w6data, w7data, w8data, w9data)

# join the baseline/mortality data to long format
long_data <-
  merge(
    long_data,
    deathdata,
    by = c("idproj", "wave"),
    all.x = TRUE,
    all.y = TRUE
  )
long_data <- merge(long_data, w1datab, by = "idproj")
long_data <- merge(long_data, w2datab, by = "idproj")

# subset data for initial latent class analyses
latent_class_data <- subset(long_data, select = c(idproj, wave, b_wtarea, lonely_category))
save(latent_class_data,file=paste0(workdir,"Data/latent_class_data.RData"))

# remove the chosen data sets from the environment
rm(w1datab, w1datat, w2datab, w2datat, w3data, w4data, w5data, w6data, w7data, w8data)
######################################################################################
# 3. Master coded/derived variables----------------

# changing variables to numeric so they wont cause issues when reshaping
long_data$lonely_category <- as.numeric(long_data$lonely_category)
long_data$whobmigroup <- as.numeric(long_data$whobmigroup)
long_data$anxiety_3yr <- as.numeric(long_data$anxiety_3yr)
long_data$depression_3yr <- as.numeric(long_data$depression_3yr)

# aria
long_data$ariapgp <- as.numeric(long_data$ariapgp)
long_data <- long_data %>% mutate(ariapgp = recode(ariapgp, 
                                                   `1` = 0,
                                                   `2` = 1,
                                                   `3` = 1,
                                                   `4` = 2,
                                                   `5` = 2,
                                                   `6` = NA_real_))
# smoking
long_data$smokst <- as.numeric(long_data$smokst)
long_data <- long_data %>% mutate(smokst = recode(smokst, 
                                                  `1` = 0,
                                                  `2` = 1,
                                                  `3` = 2,
                                                  `4` = 2,
                                                  `5` = 2,
                                                  `6` = NA_real_))
# alcohol
long_data$alcfq <- long_data$alcfre * long_data$alcqnt

# removing unnecessary variables 
long_data <- subset(long_data, select = -c(alcfre, alcqnt))

######################################################################################
# 4. Reshape to Wide and Back to long 
#-------------------------------------------------------------------------------------
# 4.1 Generate indicator of whether wave was completed, then reshape to wide
wide_data <- reshape(long_data,
                     timevar=c("wave"), 
                     idvar=c("idproj"),
                     v.names=c("censored", "mstat", "age", "ariapgp", "employ", "seifadis", "mnstrs", "whobmigroup",
                               "alcfq", "alcbng", "smokst", "pcsa", "depression_3yr", "anxiety_3yr",
                               "mcsa", "pf", "rp", "bp", "gh", "vt", "sf", "re", "mh",
                               "mos_short", "mos_long", "lonely_category", "live_alone", "death"),
                     sep = "",
                     dir="wide")

# Drop cases with  with history of major illness
wide_data <- wide_data[!(wide_data$b_heartdis_ever>0),] 
wide_data <- wide_data[!(wide_data$b_stroke_ever>0),]
wide_data <- wide_data[!(wide_data$b_cancer_ever>0),]

# removing unnecessary variables 
wide_data <- subset(wide_data, select = -c(b_heartdis_ever, b_stroke_ever, b_cancer_ever))

# 4.2 Drop participants who died before wave 4
#wide_data <- wide_data[which(wide_data$death2=="No" & wide_data$death3=="No"), ]

# 4.2 Reshape back to long- code had warnnig about N/A so using the pivot code instead
#imp_data <- reshape(wide_data,
                    #varying = c("censored9":"death6"),
                   # timevar=c("wave"), 
                   # idvar = c("idproj"),
                  #  v.names=c("censored", "mstat", "age", "ariapgp", "employ", "seifadis", "mnstrs", "whobmigroup",
                            #  "alcfq", "alcbng", "smokst", "pcsa", "depression_3yr", "anxiety_3yr",
                            #  "mcsa", "pf", "rp", "bp", "gh", "vt", "sf", "re", "mh",
                            #  "mos_short", "mos_long", "lonely_category", "live_alone"),
                 #   sep = "",
                  #  dir="long")

# Code for when the variables are of different classes. But its better to make them all numerical and not using this code 
#wide_data_2 <- as.data.frame(lapply(wide_data, as.character))
# 4.2 reshape from wide to long using pivot
imp_data <- wide_data %>% 
  pivot_longer(cols = c(censored9:death6)) %>%
  separate(col = name,
           sep = -1,
           into = c("variables", "year")) %>%
  pivot_wider(names_from = "variables",
              values_from = "value")

# add labels to variables
imp_data$ariapgp <- factor(imp_data$ariapgp,labels=c("Major city","Regional","Remote"))
imp_data$smokst <- factor(imp_data$smokst,labels=c("Never smoker","Ex smoker","Current smoker"))
imp_data$alcbng <- ordered(imp_data$alcbng)
imp_data$whobmigroup <- factor(imp_data$whobmigroup, labels=c("Underweight","Healthy","Overweight","Obese"))
imp_data$b_educ <- factor(imp_data$b_educ,labels=c("Less than high school","Trade/apprentice/certificate/diploma","University"))
imp_data$b_country <- factor(imp_data$b_country,labels=c("Australia","Other"))
imp_data$b_language <- factor(imp_data$b_language,labels=c("English","European", "Asian", "Other"))
imp_data$mstat <- factor(imp_data$mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))
imp_data$employ <- factor(imp_data$employ,labels=c("Not employed","Employed"))

imp_data$lonely_category <- factor(imp_data$lonely_category,labels=c("Rarely/none","Sometimes",
                                                                    "Most/all of time", "Occasionally"))
# factorise all the variables on the list no/yes
ny_list <- c("live_alone", "depression_3yr","anxiety_3yr", "b_depression_ever", "b_anxiety_ever", 
             "death", "censored")
imp_data[,ny_list] <- lapply(imp_data[,ny_list], factor, labels=c("No","Yes"))

# rename year to wave
imp_data <- imp_data %>% 
  dplyr::rename(wave = year)


# 4.2 Create alternative long form data with censored waves excluded
imp_data_2 <- imp_data[which(imp_data$censored==1),]
imp_data_2 <- subset(imp_data_2, select=-censored)

######################################################################################
# 5. Save data in long form, ready for imputation
#-------------------------------------------------------------------------------------
save(imp_data,file=paste0(workdir,"Paper 1. Causal effect of loneliness on mortality/Data/imputation data.RData"))
#save(imp_data_2,file=paste0(workdir,"imputation data2.RData"))
