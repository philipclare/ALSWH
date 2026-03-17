######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Descriptive sample statistics for Table 1
## Date: 30 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# 1.1. Specify paths to Katana/windows PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh/"
} else { # Manually defined for PC
  workdir <- "R:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("plyr","dplyr","ltmle","gtools","furniture","flextable","table1")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load and process data 
#-------------------------------------------------------------------------------------

load(file=paste0(workdir,"Data/imputed data - long form.RData"))
raw_data <- imp[[41]]
rm(imp)

raw_data$alcliferisk <- ifelse(raw_data$alcfq>10,1,0)
raw_data$alcepisrisk <- ifelse(raw_data$alcbng>1,1,0)
raw_data$activity_bin <- ifelse(raw_data$weighted_activity_time>=150,1,0)
raw_data$activity_bin_sens1 <- ifelse(raw_data$weighted_activity_time>=75,1,0)
raw_data$activity_bin_sens2 <- ifelse(raw_data$weighted_activity_time>=300,1,0)

raw_data <- subset(raw_data, select = -c(alcfq,alcbng,weighted_activity_time,inarea))

raw_data <- raw_data[order(raw_data$wave),]
raw_data <- reshape(raw_data,
             timevar=c("wave"), 
             idvar=c("idproj"),
             v.names=c(
               "censored","activity_bin","activity_bin_sens1","activity_bin_sens2","marital","age","ariapgp","employ","seifadis","live_u18","live_o18",
               "heartdis_3yr","hypert_3yr","stroke_3yr","cancer_3yr","arthritis_3yr","depression_3yr","anxiety_3yr",
               "cesd10","mnstrs","whobmigroup","vegetables","fruit",
               "alcliferisk","alcepisrisk","smokst",
               "pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"),
             sep = "",
             dir="wide")

raw_data <- subset(raw_data, select=-censored2)

raw_data[,c("b_heartdis_ever","b_stroke_ever","b_cancer_ever","b_depression_ever","b_anxiety_ever")] <- lapply(raw_data[,c("b_heartdis_ever","b_stroke_ever","b_cancer_ever","b_depression_ever","b_anxiety_ever")], factor, labels=c("No","Yes"))

strata <- c(list("Mean (SD) / n (%)"=raw_data))


labels <- list(
  variables=list(age2 = "Age",
                 employ2 = "Employment status",
                 marital2 = "Marital status",
                 live_u182 = "Live with children under age 18",
                 live_o182 = "Live with childrenaged 18+",
                 seifadis2 = "Area-level SES (SEIFA)",
                 ariapgp2 = "Remoteness (ARIA+)",
                 alcliferisk2 = "Lifetime risky drinking",
                 alcepisrisk2 = "Heavy episodic drinking",
                 smokst2 = "Smoking status",
                 cesd102 = "CESD10 Depression Score",
                 mnstrs2 = "Mean Stress Score",
                 whobmigroup2 = "BMI Category",
                 b_heartdis_ever = "Ever diagnosed/treated for heart disease",
                 b_stroke_ever = "Ever diagnosed/treated for stroke",
                 b_cancer_ever = "Ever diagnosed/treated for cancer",
                 b_depression_ever = "Ever diagnosed/treated for depression",
                 b_anxiety_ever = "Ever diagnosed/treated for anxiety",
                 b_cobcat = "Country of birth",
                 b_educ = "Highest level of education"))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=4), c("",
                                                           "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%2.1f%%)", FREQ, PCT))))
}

tbl1 <- table1(strata,
       labels=labels,
       render.continuous=my.render.cont,
       render.categorical=my.render.cat,
       render.missing=NULL)

tbl1
t1flex(tbl1) %>% 
  save_as_docx(path=paste0(workdir,"Results/Table 1.docx"))