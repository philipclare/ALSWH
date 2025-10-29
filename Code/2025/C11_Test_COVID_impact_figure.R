######################################################################################
##   
## Project: Trends in loneliness and isolation in Australia
## Program: C2 - Missing Data.R
## Purpose: Create plots from results of LTMLE analyses
## Author: Philip Clare
## Date: 24 October 2023
## OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-hilda_data/Loneliness trends/"

libs <- c("Amelia","ggplot2","tidyr","ggpubr","openxlsx","readxl","stringr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)


logit <- function (p) {
  log(p/(1-p))
}

invlogit <- function (b) {
  exp(b)/(1+exp(b))
}

######################################################################################
# 2. Load the primary and sensitivity model results
#-------------------------------------------------------------------------------------

# 2.1 Primary analysis results
mod_1a <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "lonely")
mod_2a <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "lonely_chronic")
mod_3a <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "support")
mod_4a <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "support_chronic")

mod_a <- as.data.frame(cbind(year=seq(2001,2023),
                           lonely_point_e=matrix(as.matrix(mod_1a[,-1]),ncol=3,byrow=TRUE)[,1],
                           lonely_point_l=matrix(as.matrix(mod_1a[,-1]),ncol=3,byrow=TRUE)[,2],
                           lonely_point_u=matrix(as.matrix(mod_1a[,-1]),ncol=3,byrow=TRUE)[,3],
                           lonely_chronic_e=matrix(c(NA,NA,NA,as.matrix(mod_2a[,-1])),ncol=3,byrow=TRUE)[,1],
                           lonely_chronic_l=matrix(c(NA,NA,NA,as.matrix(mod_2a[,-1])),ncol=3,byrow=TRUE)[,2],
                           lonely_chronic_u=matrix(c(NA,NA,NA,as.matrix(mod_2a[,-1])),ncol=3,byrow=TRUE)[,3],
                           isolation_point_e=matrix(as.matrix(mod_3a[,-1]),ncol=3,byrow=TRUE)[,1],
                           isolation_point_l=matrix(as.matrix(mod_3a[,-1]),ncol=3,byrow=TRUE)[,2],
                           isolation_point_u=matrix(as.matrix(mod_3a[,-1]),ncol=3,byrow=TRUE)[,3],
                           isolation_chronic_e=matrix(c(NA,NA,NA,as.matrix(mod_4a[,-1])),ncol=3,byrow=TRUE)[,1],
                           isolation_chronic_l=matrix(c(NA,NA,NA,as.matrix(mod_4a[,-1])),ncol=3,byrow=TRUE)[,2],
                           isolation_chronic_u=matrix(c(NA,NA,NA,as.matrix(mod_4a[,-1])),ncol=3,byrow=TRUE)[,3]))

mod_1b <- read_xlsx(paste0(workdir,"Results/Raw output/modeled - covid interaction.xlsx"), sheet = "lonely")
mod_2b <- read_xlsx(paste0(workdir,"Results/Raw output/modeled - covid interaction.xlsx"), sheet = "lonely_chronic")
mod_3b <- read_xlsx(paste0(workdir,"Results/Raw output/modeled - covid interaction.xlsx"), sheet = "support")
mod_4b <- read_xlsx(paste0(workdir,"Results/Raw output/modeled - covid interaction.xlsx"), sheet = "support_chronic")

mod_b <- as.data.frame(cbind(year=seq(2001,2023),
                             lonely_point_pre_e=matrix(c(as.matrix(mod_1b[,-1])),ncol=6,byrow=TRUE)[,1],
                             lonely_point_pre_l=matrix(c(as.matrix(mod_1b[,-1])),ncol=6,byrow=TRUE)[,2],
                             lonely_point_pre_u=matrix(c(as.matrix(mod_1b[,-1])),ncol=6,byrow=TRUE)[,3],
                             lonely_point_post_e=matrix(c(as.matrix(mod_1b[,-1])),ncol=6,byrow=TRUE)[,4],
                             lonely_point_post_l=matrix(c(as.matrix(mod_1b[,-1])),ncol=6,byrow=TRUE)[,5],
                             lonely_point_post_u=matrix(c(as.matrix(mod_1b[,-1])),ncol=6,byrow=TRUE)[,6],
                             lonely_chronic_pre_e=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_2b[,-1])),ncol=6,byrow=TRUE)[,1],
                             lonely_chronic_pre_l=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_2b[,-1])),ncol=6,byrow=TRUE)[,2],
                             lonely_chronic_pre_u=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_2b[,-1])),ncol=6,byrow=TRUE)[,3],
                             lonely_chronic_post_e=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_2b[,-1])),ncol=6,byrow=TRUE)[,4],
                             lonely_chronic_post_l=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_2b[,-1])),ncol=6,byrow=TRUE)[,5],
                             lonely_chronic_post_u=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_2b[,-1])),ncol=6,byrow=TRUE)[,6],
                             isolation_point_pre_e=matrix(c(as.matrix(mod_3b[,-1])),ncol=6,byrow=TRUE)[,1],
                             isolation_point_pre_l=matrix(c(as.matrix(mod_3b[,-1])),ncol=6,byrow=TRUE)[,2],
                             isolation_point_pre_u=matrix(c(as.matrix(mod_3b[,-1])),ncol=6,byrow=TRUE)[,3],
                             isolation_point_post_e=matrix(c(as.matrix(mod_3b[,-1])),ncol=6,byrow=TRUE)[,4],
                             isolation_point_post_l=matrix(c(as.matrix(mod_3b[,-1])),ncol=6,byrow=TRUE)[,5],
                             isolation_point_post_u=matrix(c(as.matrix(mod_3b[,-1])),ncol=6,byrow=TRUE)[,6],
                             isolation_chronic_pre_e=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_4b[,-1])),ncol=6,byrow=TRUE)[,1],
                             isolation_chronic_pre_l=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_4b[,-1])),ncol=6,byrow=TRUE)[,2],
                             isolation_chronic_pre_u=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_4b[,-1])),ncol=6,byrow=TRUE)[,3],
                             isolation_chronic_post_e=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_4b[,-1])),ncol=6,byrow=TRUE)[,4],
                             isolation_chronic_post_l=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_4b[,-1])),ncol=6,byrow=TRUE)[,5],
                             isolation_chronic_post_u=matrix(c(NA,NA,NA,NA,NA,NA,as.matrix(mod_4b[,-1])),ncol=6,byrow=TRUE)[,6]))

mod_longer_a <- mod_a %>%
  pivot_longer(c=2:13,
               names_to = c("outcome","type", ".value"),
               names_pattern = "(.+)_(.+)_(.+)"
  )
mod_longer_a$analysis <- "no"

mod_longer_b <- mod_b %>%
  pivot_longer(c=2:25,
               names_to = c("outcome","type","analysis", ".value"),
               names_pattern = "(.+)_(.+)_(.+)_(.+)"
  )
mod_longer_b <- mod_longer_b[,c(1,2,3,5,6,7,4)]

mod_longer <- rbind(mod_longer_a,mod_longer_b)

mod_longer$outcome <- factor(mod_longer$outcome,levels=c("lonely","isolation"),labels=c("Loneliness","Isolation"))
mod_longer$type <- factor(mod_longer$type,levels=c("point","chronic"),labels=c("Point","Chronic"))
mod_longer$analysis <- factor(mod_longer$analysis,labels=c("Primary","Sensitivity","Main"))
mod_longer <- mod_longer[which(mod_longer$analysis!="Main"),]

######################################################################################
# 3. Define theme and common properties
#-------------------------------------------------------------------------------------

pd <- position_dodge(0.1)
figure_theme <- theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", size = 0.3),
        text = element_text(size = 16),
        axis.line = element_line(colour = 'grey80', size = 0.3),
        axis.ticks = element_line(colour = "grey80", size = 0.3),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.title=element_blank())

######################################################################################
# 4. Plot primary against sensitivity trends
#-------------------------------------------------------------------------------------

trend_test <- ggplot(mod_longer, 
                                 aes(x=year, y=e, color=analysis)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = analysis), position=pd, size=0.2, show.legend = F, alpha=0.2) +
  geom_line(aes(color=analysis)) +
  geom_point(size=2, shape=18, aes(color=analysis)) +
  xlab("Year") +
  ylab("Prevalence") +
  expand_limits(y=c(0, 0.36),x=c(2000,2024)) +
  scale_y_continuous(breaks=seq(0, 0.35, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(2001, 2023, by = 4), expand = c(0, 0)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue")) + 
  facet_grid(rows=vars(type),cols=vars(outcome))

trend_test

ggsave(paste0(workdir,"Results/covid trend test 20250526.tiff"),
       trend_test,
       width = 2400,
       height = 2400,
       units = "px")