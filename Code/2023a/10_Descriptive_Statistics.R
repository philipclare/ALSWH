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

workdir <- "R:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"

libs <- c("plyr","dplyr","ltmle","gtools","furniture","flextable","table1")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load data 
#-------------------------------------------------------------------------------------

load(paste0(workdir,"Data/raw data for table 1 - wide form.RData"))

raw_data[,c("b_cancer_ever","b_depression_ever","b_anxiety_ever")] <- lapply(raw_data[,c("b_cancer_ever","b_depression_ever","b_anxiety_ever")], factor, labels=c("No","Yes"))

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