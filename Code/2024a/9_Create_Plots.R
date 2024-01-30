######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Create plots from results of LTMLE analysis
## Date: 7 October 2022
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
libs <- c("Amelia","ggplot2","tidyr","ggpubr","XLConnect","stringr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load the pooled results and clean for figure creation
#-------------------------------------------------------------------------------------

# 2.1 Primary analysis results
primary_mi_res_sust <- readRDS(paste0(workdir,"Results/Processed/primary-sustained-results.rds"))
pr_fig_sust <- primary_mi_res_sust %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
pr_fig_sust$outcome = factor(pr_fig_sust$outcome, 
                                 levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))


pr_fig_sust$group <- ifelse(pr_fig_sust$result=="stop_45" | pr_fig_sust$result=="stop_70",1,0)
pr_fig_sust$group <- factor(pr_fig_sust$group)

primary_mi_res_init <- readRDS(paste0(workdir,"Results/Processed/primary-iniation-results.rds"))
pr_fig_init <- primary_mi_res_init %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
pr_fig_init$outcome = factor(pr_fig_init$outcome, 
                                 levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))

pr_fig_init$group <- ifelse(pr_fig_init$result=="start_45" | pr_fig_init$result=="start_70",1,0)
pr_fig_init$group <- factor(pr_fig_init$group)

# 2.2 Sensitivity analysis 1 results
sensitivity_1_mi_res_sust <- readRDS(paste0(workdir,"Results/Processed/sensitivity_1-sustained-results.rds"))
s1_fig_sust <- sensitivity_1_mi_res_sust %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s1_fig_sust$outcome = factor(s1_fig_sust$outcome, 
                                 levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))

s1_fig_sust$group <- ifelse(s1_fig_sust$result=="stop_45" | s1_fig_sust$result=="stop_70",1,0)
s1_fig_sust$group <- factor(s1_fig_sust$group)

sensitivity_1_mi_res_init <- readRDS(paste0(workdir,"Results/Processed/sensitivity_1-iniation-results.rds"))
s1_fig_init <- sensitivity_1_mi_res_init %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s1_fig_init$outcome = factor(s1_fig_init$outcome, 
                                 levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))

s1_fig_init$group <- ifelse(s1_fig_init$result=="start_45" | s1_fig_init$result=="start_70",1,0)
s1_fig_init$group <- factor(s1_fig_init$group)

# 2.3 Sensitivity analysis 2 results
sensitivity_2_mi_res_sust <- readRDS(paste0(workdir,"Results/Processed/sensitivity_2-sustained-results.rds"))
s2_fig_sust <- sensitivity_2_mi_res_sust %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s2_fig_sust$outcome = factor(s2_fig_sust$outcome, 
                                 levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))

s2_fig_sust$group <- ifelse(s2_fig_sust$result=="stop_45" | s2_fig_sust$result=="stop_70",1,0)
s2_fig_sust$group <- factor(s2_fig_sust$group)

sensitivity_2_mi_res_init <- readRDS(paste0(workdir,"Results/Processed/sensitivity_2-iniation-results.rds"))
s2_fig_init <- sensitivity_2_mi_res_init %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s2_fig_init$outcome = factor(s2_fig_init$outcome, 
                                 levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))

s2_fig_init$group <- ifelse(s2_fig_init$result=="start_45" | s2_fig_init$result=="start_70",1,0)
s2_fig_init$group <- factor(s2_fig_init$group)

# 2.4 Sensitivity analysis 3 results
sensitivity_3_mi_res_sust <- readRDS(paste0(workdir,"Results/Processed/sensitivity_3-sustained-results.rds"))
s3_fig_sust <- sensitivity_3_mi_res_sust %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s3_fig_sust$outcome = factor(s3_fig_sust$outcome, 
                             levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                             labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))

s3_fig_sust$group <- ifelse(s3_fig_sust$result=="stop_45" | s3_fig_sust$result=="stop_70",1,0)
s3_fig_sust$group <- factor(s3_fig_sust$group)

sensitivity_3_mi_res_init <- readRDS(paste0(workdir,"Results/Processed/sensitivity_3-iniation-results.rds"))
s3_fig_init <- sensitivity_3_mi_res_init %>%
  pivot_longer(c=1:20,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s3_fig_init$outcome = factor(s3_fig_init$outcome, 
                             levels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"), 
                             labels=c("pcsa","mcsa","gh","pf","rp","bp","vt","sf","re","mh"))

s3_fig_init$group <- ifelse(s3_fig_init$result=="start_45" | s3_fig_init$result=="start_70",1,0)
s3_fig_init$group <- factor(s3_fig_init$group)

######################################################################################
# 2. Save summary output to excel for tables
#-------------------------------------------------------------------------------------

primary_mi_res_sust <- primary_mi_res_sust[,c(1:4,11:12,5:10,13:21)]
primary_mi_res_init <- primary_mi_res_init[,c(1:4,11:12,5:10,13:21)]
sensitivity_1_mi_res_sust <- sensitivity_1_mi_res_sust[,c(1:4,11:12,5:10,13:21)]
sensitivity_1_mi_res_init <- sensitivity_1_mi_res_init[,c(1:4,11:12,5:10,13:21)]
sensitivity_2_mi_res_sust <- sensitivity_2_mi_res_sust[,c(1:4,11:12,5:10,13:21)]
sensitivity_2_mi_res_init <- sensitivity_2_mi_res_init[,c(1:4,11:12,5:10,13:21)]
sensitivity_3_mi_res_sust <- sensitivity_3_mi_res_sust[,c(1:4,11:12,5:10,13:21)]
sensitivity_3_mi_res_init <- sensitivity_3_mi_res_init[,c(1:4,11:12,5:10,13:21)]

wb <- loadWorkbook(paste0(workdir,"Results/model-results 20230608.xlsx"), create = TRUE)
createSheet(wb, name = "Primary")
createSheet(wb, name = "S1")
createSheet(wb, name = "S2")
createSheet(wb, name = "S3")

writeWorksheet(wb,primary_mi_res_sust,"Primary",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,primary_mi_res_init,"Primary",startRow = 10, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_1_mi_res_sust,"S1",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_1_mi_res_init,"S1",startRow = 10, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_2_mi_res_sust,"S2",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_2_mi_res_init,"S2",startRow = 10, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_3_mi_res_sust,"S3",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_3_mi_res_init,"S3",startRow = 10, startCol = 1, header = FALSE)
saveWorkbook(wb)

rm(primary_mi_res_sust)
rm(primary_mi_res_init)
rm(sensitivity_1_mi_res_sust)
rm(sensitivity_1_mi_res_init)
rm(sensitivity_2_mi_res_sust)
rm(sensitivity_2_mi_res_init)
rm(sensitivity_3_mi_res_sust)
rm(sensitivity_3_mi_res_init)

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
pr_pr_sust_fig <- ggplot(pr_fig_sust[which(pr_fig_sust$outcome=="pcsa" | pr_fig_sust$outcome=="mcsa"),], 
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/F2-primary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

pr_sc_sust_fig <- ggplot(pr_fig_sust[which(pr_fig_sust$outcome!="pcsa" & pr_fig_sust$outcome!="mcsa"),],
                           aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S2-secondary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

# 4.2 Initiating activity plots
pr_pr_init_fig <- ggplot(pr_fig_init[which(pr_fig_sust$outcome=="pcsa" | pr_fig_sust$outcome=="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/F1-primary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

pr_sc_init_fig <- ggplot(pr_fig_init[which(pr_fig_sust$outcome!="pcsa" & pr_fig_sust$outcome!="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S1-secondary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

######################################################################################
# 5. Sensitivity Analysis 1 Plots
#-------------------------------------------------------------------------------------

# 5.1 Sustained activity plots
s1_pr_sust_fig <- ggplot(s1_fig_sust[which(s1_fig_sust$outcome=="pcsa" | s1_fig_sust$outcome=="mcsa"),], 
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S4-primary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

s1_sc_sust_fig <- ggplot(s1_fig_sust[which(s1_fig_sust$outcome!="pcsa" & s1_fig_sust$outcome!="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S6-secondary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

# 5.2 Initiating activity plots
s1_pr_init_fig <- ggplot(s1_fig_init[which(s1_fig_sust$outcome=="pcsa" | s1_fig_sust$outcome=="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S3-primary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

s1_sc_init_fig <- ggplot(s1_fig_init[which(s1_fig_sust$outcome!="pcsa" & s1_fig_sust$outcome!="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S5-secondary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

######################################################################################
# 6. Sensitivity Analysis 2 Plots
#-------------------------------------------------------------------------------------

# 6.1 Sustained activity plots
s2_pr_sust_fig <- ggplot(s2_fig_sust[which(s2_fig_sust$outcome=="pcsa" | s2_fig_sust$outcome=="mcsa"),], 
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S8-primary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

s2_sc_sust_fig <- ggplot(s2_fig_sust[which(s2_fig_sust$outcome!="pcsa" & s2_fig_sust$outcome!="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S10-secondary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

# 6.2 Initiating activity plots
s2_pr_init_fig <- ggplot(s2_fig_init[which(s2_fig_sust$outcome=="pcsa" | s2_fig_sust$outcome=="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S7-primary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

s2_sc_init_fig <- ggplot(s2_fig_init[which(s2_fig_sust$outcome!="pcsa" & s2_fig_sust$outcome!="mcsa"),],
                      aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S9-secondary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

######################################################################################
# 7. Sensitivity Analysis 3 Plots
#-------------------------------------------------------------------------------------

# 7.1 Sustained activity plots
s3_pr_sust_fig <- ggplot(s3_fig_sust[which(s3_fig_sust$outcome=="pcsa" | s3_fig_sust$outcome=="mcsa"),], 
                         aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S12-primary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

s3_sc_sust_fig <- ggplot(s3_fig_sust[which(s3_fig_sust$outcome!="pcsa" & s3_fig_sust$outcome!="mcsa"),],
                         aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age stopped meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Did not meet guidelines in any wave","50","55","60","65","Met guidelines in all waves"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S14-secondary-sustained 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

# 7.2 Initiating activity plots
s3_pr_init_fig <- ggplot(s3_fig_init[which(s3_fig_sust$outcome=="pcsa" | s3_fig_sust$outcome=="mcsa"),],
                         aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=35:65) +
  scale_y_continuous(breaks=seq(35, 65, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S11-primary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

s3_sc_init_fig <- ggplot(s3_fig_init[which(s3_fig_sust$outcome!="pcsa" & s3_fig_sust$outcome!="mcsa"),],
                         aes(x=result, y=mean, color=group)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  xlab("Age began meeting guidelines (years)") +
  ylab("Expected mean") +
  expand_limits(y=50:100) + 
  scale_y_continuous(breaks=seq(50, 100, by = 10), expand = c(0, 0)) +
  scale_x_discrete(labels=str_wrap(c("Met guidelines in all waves","50","55","60","65","Did not meet guidelines in any wave"),width=10)) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(gh="(a) General Health",
                                         pf="(b) Physical Functioning",
                                         rp="(c) Role Physical",
                                         bp="(d) Bodily pain",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

ggsave(paste0(workdir,"Results/S13-secondary-initiation 20230608.tiff"),
       width = 2400,
       height = 2400,
       units = "px")