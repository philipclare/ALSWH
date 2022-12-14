######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 7 October 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"

libs <- c("Amelia","ggplot2","tidyr","ggpubr","stringr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load the pooled results and clean for figure creation
#-------------------------------------------------------------------------------------

# 2.1 Primary analysis results
load(paste0(workdir,"Results/primary-sustained-results.RData"))
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

load(paste0(workdir,"Results/primary-iniation-results.RData"))
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
load(paste0(workdir,"Results/sensitivity_1-sustained-results.RData"))
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

load(paste0(workdir,"Results/sensitivity_1-iniation-results.RData"))
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

# 2.3 Sensitivity analysis 1 results
load(paste0(workdir,"Results/sensitivity_2-sustained-results.RData"))
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

load(paste0(workdir,"Results/sensitivity_2-iniation-results.RData"))
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

######################################################################################
# 2. Save summary output to excel for tables
#-------------------------------------------------------------------------------------

wb <- loadWorkbook(paste0(workdir,"Results/model-results.xlsx"), create = TRUE)
createSheet(wb, name = "Primary")
createSheet(wb, name = "S1")
createSheet(wb, name = "S2")

writeWorksheet(wb,primary_mi_res_sust,"Primary",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,primary_mi_res_init,"Primary",startRow = 10, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_1_mi_res_sust,"S1",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_1_mi_res_init,"S1",startRow = 10, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_2_mi_res_sust,"S2",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,sensitivity_2_mi_res_init,"S2",startRow = 10, startCol = 1, header = FALSE)
saveWorkbook(wb)

rm(primary_mi_res_sust)
rm(primary_mi_res_init)
rm(sensitivity_1_mi_res_sust)
rm(sensitivity_1_mi_res_init)
rm(sensitivity_2_mi_res_sust)
rm(sensitivity_2_mi_res_init)

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

ggsave(paste0(workdir,"Results/primary-sustained.tiff"),
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

ggsave(paste0(workdir,"Results/secondary-sustained.tiff"),
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

ggsave(paste0(workdir,"Results/primary-initiation.tiff"),
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

ggsave(paste0(workdir,"Results/secondary-initiation.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

######################################################################################
# 5. Primary Analysis Plots
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

ggsave(paste0(workdir,"Results/s1-primary-sustained.tiff"),
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

ggsave(paste0(workdir,"Results/s1-secondary-sustained.tiff"),
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

ggsave(paste0(workdir,"Results/s1-primary-initiation.tiff"),
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

ggsave(paste0(workdir,"Results/s1-secondary-initiation.tiff"),
       width = 2400,
       height = 2400,
       units = "px")

######################################################################################
# 6. Primary Analysis Plots
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

ggsave(paste0(workdir,"Results/s2-primary-sustained.tiff"),
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

ggsave(paste0(workdir,"Results/s2-secondary-sustained.tiff"),
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

ggsave(paste0(workdir,"Results/s2-primary-initiation.tiff"),
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

ggsave(paste0(workdir,"Results/s2-secondary-initiation.tiff"),
       width = 2400,
       height = 2400,
       units = "px")