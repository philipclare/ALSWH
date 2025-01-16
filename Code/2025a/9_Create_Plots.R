######################################################################################
##   
## Effects of physical activity on mortality
## Create plots from results of LTMLE analyses
## Date: 7 October 2022
## OSF Registration: https://osf.io/pytzx
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
workdir <- "//surefsn025/ProfileR025$/philipclare/Documents/ALSWH/"

libs <- c("Amelia","ggplot2","tidyr","ggpubr","openxlsx","stringr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load the pooled results and clean for figure creation
#-------------------------------------------------------------------------------------

# 2.1 Primary analysis results
primary_mi_res_sust <- readRDS(file=paste0(workdir,"Results/primary results - sustained.rds"))
primary_fig_sust <- primary_mi_res_sust %>%
  pivot_longer(c=1:6,
               names_to = c("analysis", ".value"),
               names_pattern = "(.+)_(.+)"
  )
primary_fig_sust$analysis = factor(primary_fig_sust$analysis, 
                                   levels=c("primary","s1","s2"), 
                                   labels=c("primary","s1","s2"))

primary_fig_sust$outcome <- "allcause"
primary_fig_sust$outcome <- factor(primary_fig_sust$outcome, 
                                   levels=c("allcause","cvd","cancer"), 
                                   labels=c("allcause","cvd","cancer"))

primary_fig_sust$group <- ifelse(primary_fig_sust$result=="stop_45" | primary_fig_sust$result=="stop_70",1,0)
primary_fig_sust$group <- factor(primary_fig_sust$group)

primary_mi_res_init <- readRDS(file=paste0(workdir,"Results/primary results - initiation.rds"))

primary_fig_init <- primary_mi_res_init %>%
  pivot_longer(c=1:6,
               names_to = c("analysis", ".value"),
               names_pattern = "(.+)_(.+)"
  )
primary_fig_init$analysis = factor(primary_fig_init$analysis, 
                                   levels=c("primary","s1","s2"), 
                                   labels=c("primary","s1","s2"))

primary_fig_init$outcome <- "allcause"
primary_fig_init$outcome <- factor(primary_fig_init$outcome, 
                                   levels=c("allcause","cvd","cancer"), 
                                   labels=c("allcause","cvd","cancer"))

primary_fig_init$group <- ifelse(primary_fig_init$result=="start_45" | primary_fig_init$result=="start_70",1,0)
primary_fig_init$group <- factor(primary_fig_init$group)

# 2.2 CVD analysis results
cvd_mi_res_sust <- readRDS(file=paste0(workdir,"Results/cvd results - sustained.rds"))
cvd_fig_sust <- cvd_mi_res_sust %>%
  pivot_longer(c=1:6,
               names_to = c("analysis", ".value"),
               names_pattern = "(.+)_(.+)"
  )
cvd_fig_sust$analysis <- factor(cvd_fig_sust$analysis, 
                                levels=c("primary","s1","s2"), 
                                labels=c("primary","s1","s2"))

cvd_fig_sust$outcome <- "cvd"
cvd_fig_sust$outcome <- factor(cvd_fig_sust$outcome, 
                               levels=c("allcause","cvd","cancer"), 
                               labels=c("allcause","cvd","cancer"))

cvd_fig_sust$group <- ifelse(cvd_fig_sust$result=="stop_45" | cvd_fig_sust$result=="stop_70",1,0)
cvd_fig_sust$group <- factor(cvd_fig_sust$group)

cvd_mi_res_init <- readRDS(file=paste0(workdir,"Results/cvd results - initiation.rds"))

cvd_fig_init <- cvd_mi_res_init %>%
  pivot_longer(c=1:6,
               names_to = c("analysis", ".value"),
               names_pattern = "(.+)_(.+)"
  )
cvd_fig_init$analysis <- factor(cvd_fig_init$analysis, 
                                levels=c("primary","s1","s2"), 
                                labels=c("primary","s1","s2"))

cvd_fig_init$outcome <- "cvd"
cvd_fig_init$outcome <- factor(cvd_fig_init$outcome, 
                               levels=c("allcause","cvd","cancer"), 
                               labels=c("allcause","cvd","cancer"))

cvd_fig_init$group <- ifelse(cvd_fig_init$result=="start_45" | cvd_fig_init$result=="start_70",1,0)
cvd_fig_init$group <- factor(cvd_fig_init$group)

# 2.3 Cancer analysis results
cancer_mi_res_sust <- readRDS(file=paste0(workdir,"Results/cancer results - sustained.rds"))
cancer_fig_sust <- cancer_mi_res_sust %>%
  pivot_longer(c=1:6,
               names_to = c("analysis", ".value"),
               names_pattern = "(.+)_(.+)"
  )
cancer_fig_sust$analysis <- factor(cancer_fig_sust$analysis, 
                                   levels=c("primary","s1","s2"), 
                                   labels=c("primary","s1","s2"))

cancer_fig_sust$outcome <- "cancer"
cancer_fig_sust$outcome <- factor(cancer_fig_sust$outcome, 
                                  levels=c("allcause","cvd","cancer"), 
                                  labels=c("allcause","cvd","cancer"))

cancer_fig_sust$group <- ifelse(cancer_fig_sust$result=="stop_45" | cancer_fig_sust$result=="stop_70",1,0)
cancer_fig_sust$group <- factor(cancer_fig_sust$group)

cancer_mi_res_init <- readRDS(file=paste0(workdir,"Results/cancer results - initiation.rds"))

cancer_fig_init <- cancer_mi_res_init %>%
  pivot_longer(c=1:6,
               names_to = c("analysis", ".value"),
               names_pattern = "(.+)_(.+)"
  )
cancer_fig_init$analysis <- factor(cancer_fig_init$analysis, 
                                   levels=c("primary","s1","s2"), 
                                   labels=c("primary","s1","s2"))

cancer_fig_init$outcome <- "cancer"
cancer_fig_init$outcome <- factor(cancer_fig_init$outcome, 
                                  levels=c("allcause","cvd","cancer"), 
                                  labels=c("allcause","cvd","cancer"))

cancer_fig_init$group <- ifelse(cancer_fig_init$result=="start_45" | cancer_fig_init$result=="start_70",1,0)
cancer_fig_init$group <- factor(cancer_fig_init$group)

# ######################################################################################
# # 2. Save summary output to excel for tables
# #-------------------------------------------------------------------------------------

fig_data_sust <- rbind(primary_fig_sust,cvd_fig_sust,cancer_fig_sust)
fig_data_init <- rbind(primary_fig_init,cvd_fig_init,cancer_fig_init)

wb <- createWorkbook()

addWorksheet(wb,"Sustained")
addWorksheet(wb,"Initiation")

writeData(wb,sheet="Sustained",primary_mi_res_sust,startRow=1)
writeData(wb,sheet="Sustained",cvd_mi_res_sust,startRow=11)
writeData(wb,sheet="Sustained",cancer_mi_res_sust,startRow=21)

writeData(wb,sheet="Initiation",primary_mi_res_init,startRow=1)
writeData(wb,sheet="Initiation",cvd_mi_res_init,startRow=11)
writeData(wb,sheet="Initiation",cancer_mi_res_init,startRow=21)

saveWorkbook(wb, file = paste0(workdir,"Results/model-results.xlsx"), overwrite = TRUE)

rm(primary_mi_res_sust)
rm(primary_mi_res_init)
rm(cvd_mi_res_sust)
rm(cvd_mi_res_init)
rm(cancer_mi_res_sust)
rm(cancer_mi_res_init)

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
        legend.position="none")

######################################################################################
# 4. Primary Analysis Plots
#-------------------------------------------------------------------------------------

# 4.1 Sustained activity plots
pr_sust_fig <- ggplot(fig_data_sust[which(fig_data_sust$analysis=="primary"),], 
                         aes(x=result, y=prob, color=group)) +
  geom_errorbar(aes(ymin=prob-(qnorm(0.9975)*se), ymax=prob+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age (years) stopped meeting guidelines") +
  ylab("Incidence-risk") +
  expand_limits(y=c(0, 0.3)) +
  scale_y_continuous(breaks=seq(0, 0.3, by = 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(allcause="(a) All-cause mortality",
                                          cvd="(b) CVD mortality",
                                          cancer="(c) Cancer mortality"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/f1-primary-sustained.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

# 4.2 Initiating activity plots
pr_init_fig <- ggplot(fig_data_init[which(fig_data_sust$analysis=="primary"),], 
                         aes(x=result, y=prob, color=group)) +
  geom_errorbar(aes(ymin=prob-(qnorm(0.9975)*se), ymax=prob+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age (years) started meeting guidelines") +
  ylab("Incidence-risk") +
  expand_limits(y=c(0, 0.3)) +
  scale_y_continuous(breaks=seq(0, 0.3, by = 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(allcause="(a) All-cause mortality",
                                          cvd="(b) CVD mortality",
                                          cancer="(c) Cancer mortality"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/f2-primary-initiation.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

######################################################################################
# 5. S1 Analysis Plots
#-------------------------------------------------------------------------------------

# 5.1 Sustained activity plots
pr_cvd_sust_fig <- ggplot(fig_data_sust[which(fig_data_sust$analysis=="s1"),], 
                          aes(x=result, y=prob, color=group)) +
  geom_errorbar(aes(ymin=prob-(qnorm(0.9975)*se), ymax=prob+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age (years) stopped exercising at least 75 mins per week") +
  ylab("Incidence-risk") +
  expand_limits(y=c(0, 0.3)) +
  scale_y_continuous(breaks=seq(0, 0.3, by = 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(allcause="(a) All-cause mortality",
                                          cvd="(b) CVD mortality",
                                          cancer="(c) Cancer mortality"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/f3-s1-sustained.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

# 5.2 Initiating activity plots
pr_cvd_init_fig <- ggplot(fig_data_init[which(fig_data_init$analysis=="s1"),], 
                          aes(x=result, y=prob, color=group)) +
  geom_errorbar(aes(ymin=prob-(qnorm(0.9975)*se), ymax=prob+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age (years) started exercising at least 75 mins per week") +
  ylab("Incidence-risk") +
  expand_limits(y=c(0, 0.3)) +
  scale_y_continuous(breaks=seq(0, 0.3, by = 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(allcause="(a) All-cause mortality",
                                          cvd="(b) CVD mortality",
                                          cancer="(c) Cancer mortality"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/f4-s1-initiation.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

######################################################################################
# 6. S2 Analysis Plots
#-------------------------------------------------------------------------------------

# 6.1 Sustained activity plots
pr_cancer_sust_fig <- ggplot(fig_data_sust[which(fig_data_sust$analysis=="s2"),], 
                             aes(x=result, y=prob, color=group)) +
  geom_errorbar(aes(ymin=prob-(qnorm(0.9975)*se), ymax=prob+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age (years) stopped exercising at least 300 mins per week") +
  ylab("Incidence-risk") +
  expand_limits(y=c(0, 0.3)) +
  scale_y_continuous(breaks=seq(0, 0.3, by = 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(allcause="(a) All-cause mortality",
                                          cvd="(b) CVD mortality",
                                          cancer="(c) Cancer mortality"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/f5-s2-sustained.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

# 6.2 Initiating activity plots
pr_cancer_init_fig <- ggplot(fig_data_init[which(fig_data_init$analysis=="s2"),], 
                             aes(x=result, y=prob, color=group)) +
  geom_errorbar(aes(ymin=prob-(qnorm(0.9975)*se), ymax=prob+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age (years) started exercising at least 75 mins per week") +
  ylab("Incidence-risk") +
  expand_limits(y=c(0, 0.3)) +
  scale_y_continuous(breaks=seq(0, 0.3, by = 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(allcause="(a) All-cause mortality",
                                          cvd="(b) CVD mortality",
                                          cancer="(c) Cancer mortality"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/f6-s2-initiation.tiff"),
       width = 2400,
       height = 2400,
       units = "px")