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
# 2. Load the pooled results and clean for figure creation
#-------------------------------------------------------------------------------------

# 2.1 Primary analysis results
raw_1 <- read_xlsx(paste0(workdir,"Results/Raw output/raw.xlsx"), sheet = "lonely")
raw_2 <- read_xlsx(paste0(workdir,"Results/Raw output/raw.xlsx"), sheet = "lonely_chronic")
raw_3 <- read_xlsx(paste0(workdir,"Results/Raw output/raw.xlsx"), sheet = "support")
raw_4 <- read_xlsx(paste0(workdir,"Results/Raw output/raw.xlsx"), sheet = "support_chronic")

raw <- as.data.frame(cbind(year=seq(2001,2023),
                           lonely_point=as.numeric(as.matrix(raw_1[,-1])),
                           lonely_chronic=c(NA,as.matrix(raw_2[,-1])),
                           isolation_point=as.numeric(as.matrix(raw_3[,-1])),
                           isolation_chronic=c(NA,as.matrix(raw_4[,-1]))))

raw_longer <- raw %>%
  pivot_longer(c=2:5,
               names_to = c("outcome","type"),
               names_pattern = "(.+)_(.+)"
  )

raw_longer$outcome <- factor(raw_longer$outcome,levels=c("lonely","isolation"),labels=c("Loneliness","Isolation"))

mod_1 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "lonely")
mod_2 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "lonely_chronic")
mod_3 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "support")
mod_4 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled.xlsx"), sheet = "support_chronic")

mod <- as.data.frame(cbind(year=seq(2001,2023),
                           lonely_point_e=matrix(as.matrix(mod_1[,-1]),ncol=3,byrow=TRUE)[,1],
                           lonely_point_l=matrix(as.matrix(mod_1[,-1]),ncol=3,byrow=TRUE)[,2],
                           lonely_point_u=matrix(as.matrix(mod_1[,-1]),ncol=3,byrow=TRUE)[,3],
                           lonely_chronic_e=matrix(c(NA,NA,NA,as.matrix(mod_2[,-1])),ncol=3,byrow=TRUE)[,1],
                           lonely_chronic_l=matrix(c(NA,NA,NA,as.matrix(mod_2[,-1])),ncol=3,byrow=TRUE)[,2],
                           lonely_chronic_u=matrix(c(NA,NA,NA,as.matrix(mod_2[,-1])),ncol=3,byrow=TRUE)[,3],
                           isolation_point_e=matrix(as.matrix(mod_3[,-1]),ncol=3,byrow=TRUE)[,1],
                           isolation_point_l=matrix(as.matrix(mod_3[,-1]),ncol=3,byrow=TRUE)[,2],
                           isolation_point_u=matrix(as.matrix(mod_3[,-1]),ncol=3,byrow=TRUE)[,3],
                           isolation_chronic_e=matrix(c(NA,NA,NA,as.matrix(mod_4[,-1])),ncol=3,byrow=TRUE)[,1],
                           isolation_chronic_l=matrix(c(NA,NA,NA,as.matrix(mod_4[,-1])),ncol=3,byrow=TRUE)[,2],
                           isolation_chronic_u=matrix(c(NA,NA,NA,as.matrix(mod_4[,-1])),ncol=3,byrow=TRUE)[,3]))

mod_longer <- mod %>%
  pivot_longer(c=2:13,
               names_to = c("outcome","type", ".value"),
               names_pattern = "(.+)_(.+)_(.+)"
  )

mod_longer$outcome <- factor(mod_longer$outcome,levels=c("lonely","isolation"),labels=c("Loneliness","Isolation"))

######################################################################################
# 3. Define theme and common properties
#-------------------------------------------------------------------------------------

pd <- position_dodge(0.1)
figure_theme <- theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
        text = element_text(size = 16),
        axis.line = element_line(colour = 'grey80', linewidth = 0.3),
        axis.ticks = element_line(colour = "grey80", linewidth = 0.3),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.title=element_blank())

######################################################################################
# 4. Primary Analysis Plots
#-------------------------------------------------------------------------------------

# 4.1 Raw results - split by point/chronic
raw_point_fig <- ggplot(raw_longer[which(raw_longer$type=="point"),],
                    aes(x=year, y=value, color=outcome)) +
  geom_line(aes(color=outcome)) +
  geom_point(size=2, shape=18, aes(color=outcome)) +
  xlab("Year") +
  ylab("Prevalence") +
  expand_limits(y=c(0, 0.4),x=c(2000,2024)) +
  scale_y_continuous(breaks=seq(0, 0.4, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(2001, 2023, by = 2), expand = c(0, 0)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

raw_point_fig

raw_chronic_fig <- ggplot(raw_longer[which(raw_longer$type=="chronic"),],
                      aes(x=year, y=value, color=outcome)) +
  geom_line(aes(color=outcome)) +
  geom_point(size=2, shape=18, aes(color=outcome)) +
  xlab("Year") +
  ylab("Prevalence") +
  expand_limits(y=c(0, 0.25),x=c(2001,2024)) +
  scale_y_continuous(breaks=seq(0, 0.25, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(2002, 2023, by = 2), expand = c(0, 0)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

raw_chronic_fig

ggsave(paste0(workdir,"Results/F1 - point - raw 20250515.tiff"),
       raw_point_fig,
       width = 2400,
       height = 1600,
       units = "px")

ggsave(paste0(workdir,"Results/F2 - chronic - raw 20250515.tiff"),
       raw_chronic_fig,
       width = 2400,
       height = 1600,
       units = "px")

# 4.2 Modeled results - split by point/chronic
mod_point_fig <- ggplot(mod_longer[which(mod_longer$type=="point"),],
                         aes(x=year, y=e, color=outcome)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = outcome), position=pd, size=0.2, show.legend = F) +
  geom_line(aes(color=outcome)) +
  geom_point(size=2, shape=18, aes(color=outcome)) +
  xlab("Year") +
  ylab("Prevalence") +
  expand_limits(y=c(0, 0.4),x=c(2000,2024)) +
  scale_y_continuous(breaks=seq(0, 0.4, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(2001, 2023, by = 2), expand = c(0, 0)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

mod_point_fig

mod_chronic_fig <- ggplot(mod_longer[which(mod_longer$type=="chronic"),],
                        aes(x=year, y=e, color=outcome)) +
  geom_ribbon(aes(ymin=l, ymax=u, fill = outcome), position=pd, size=0.2, show.legend = F) +
  geom_line(aes(color=outcome)) +
  geom_point(size=2, shape=18, aes(color=outcome)) +
  xlab("Year") +
  ylab("Prevalence") +
  expand_limits(y=c(0, 0.25),x=c(2001,2024)) +
  scale_y_continuous(breaks=seq(0, 0.25, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(2002, 2023, by = 2), expand = c(0, 0)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

mod_chronic_fig

ggsave(paste0(workdir,"Results/F3 - point - modeled 20250515.tiff"),
       mod_point_fig,
       width = 2400,
       height = 1600,
       units = "px")

ggsave(paste0(workdir,"Results/F4 - chronic - modeled 20250515.tiff"),
       mod_chronic_fig,
       width = 2400,
       height = 1600,
       units = "px")
