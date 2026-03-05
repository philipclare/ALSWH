
library("dplyr")
library("haven")
library("lme4")
library("readxl")
library("datawizard")
library("tidyverse")


workdir <- "C:/Users/pcla5984/Dropbox (Sydney Uni)/Lancet series data analysis/Paper 1-analysis of living arrangement"

load(paste0(workdir,"/Data/CORESIDENCE_DATABASE.RData"))

national <- CORESIDENCE_DB[[1]]

regions <- read_xlsx(paste0(workdir,"/Data/UN Regions.xlsx"))
hdi <- read.csv(paste0(workdir,"/Data/HDI.csv"))
hdi <- pivot_longer(hdi,
                    cols = c(3:32),
                    names_to = c("T1"),
                    names_pattern = "hdi_(.*)",
                    values_to = "hdi")

avg_size <- national %>% select(c(C1,T1,HS17,HS01,P1,P4,D1,D6,S2))

avg_size <- merge(avg_size,regions[,c("C1","Region")],by="C1")
avg_size <- merge(avg_size,hdi[,c("C1","T1","hdi")],by=c("C1","T1"))

avg_size$C1 <- factor(avg_size$C1)
avg_size$Region <- factor(avg_size$Region)
avg_size$S2 <- factor(avg_size$S2)

avg_size <- rescale_weights(avg_size, group="C1", "P1", nest = FALSE)
avg_size$P1 <- (avg_size$P1-mean(avg_size$P1, na.rm=TRUE))/sd(avg_size$P1, na.rm=TRUE)
avg_size$D6 <- (avg_size$D6-mean(avg_size$D6, na.rm=TRUE))/sd(avg_size$D6, na.rm=TRUE)

write_dta(avg_size,paste0(workdir,"/Data/average_size.dta"))

