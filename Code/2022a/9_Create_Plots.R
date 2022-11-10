######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 16 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/Physical activity trajectories/"

libs <- c("Amelia","ggplot2","tidyr")
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
  pivot_longer(c=1:4,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
pr_fig_sust$outcome = factor(pr_fig_sust$outcome, 
                                 levels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"))
rm(primary_mi_res_sust)

load(paste0(workdir,"Results/primary-iniation-results.RData"))
pr_fig_init <- primary_mi_res_init %>%
  pivot_longer(c=1:4,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
pr_fig_init$outcome = factor(pr_fig_init$outcome, 
                                 levels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"))
rm(primary_mi_res_init)

# 2.2 Sensitivity analysis 1 results
load(paste0(workdir,"Results/sensitivity_1-sustained-results.RData"))
s1_fig_sust <- sensitivity_1_mi_res_sust %>%
  pivot_longer(c=1:4,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s1_fig_sust$outcome = factor(s1_fig_sust$outcome, 
                                 levels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"))
rm(sensitivity_1_mi_res_sust)

load(paste0(workdir,"Results/sensitivity_1-iniation-results.RData"))
s1_fig_init <- sensitivity_1_mi_res_init %>%
  pivot_longer(c=1:4,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s1_fig_init$outcome = factor(s1_fig_init$outcome, 
                                 levels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"))
rm(sensitivity_1_mi_res_init)

# 2.3 Sensitivity analysis 1 results
load(paste0(workdir,"Results/sensitivity_2-sustained-results.RData"))
s2_fig_sust <- sensitivity_2_mi_res_sust %>%
  pivot_longer(c=1:4,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s2_fig_sust$outcome = factor(s2_fig_sust$outcome, 
                                 levels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"))
rm(sensitivity_2_mi_res_sust)

load(paste0(workdir,"Results/sensitivity_2-iniation-results.RData"))
s2_fig_init <- sensitivity_2_mi_res_init %>%
  pivot_longer(c=1:4,
               names_to = c("outcome", ".value"),
               names_pattern = "(.+)_(.+)"
  )
s2_fig_init$outcome = factor(s2_fig_init$outcome, 
                                 levels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"), 
                                 labels=c("pcsa","mcsa","pf","rp","bp","gh","vt","sf","re","mh"))
rm(sensitivity_2_mi_res_init)

######################################################################################
# 2. Define theme and common properties
#-------------------------------------------------------------------------------------

pd <- position_dodge(0.1)
figure_theme <- theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", size = 0.2),
        text = element_text(size = 5),
        axis.line = element_line(colour = 'grey80', size = 0.2),
        axis.ticks = element_line(colour = "grey80", size = 0.2),
        strip.background = element_blank(), # remove the background
        strip.placement = "outside",
        strip.text = element_text(hjust = 0))

######################################################################################
# 3. Primary Analysis Plots
#-------------------------------------------------------------------------------------

# 3.1 Sustained activity plots
pr_sust_fig <- ggplot(pr_fig_sust[which(pr_fig_sust$outcome=="pcsa" | pr_fig_sust$outcome=="mcsa"),], 
                      aes(x=result, y=mean)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age stopped meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) +
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=c("≤45","50","55","60","65","70+")) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme

ggsave(paste0(workdir,"Results/primary-sustained.tiff"),
       width = 1000,
       height = 1000,
       units = "px")

sc_sust_fig <- ggplot(pr_fig_sust[which(pr_fig_sust$outcome!="pcsa" & pr_fig_sust$outcome!="mcsa"),],
                           aes(x=result, y=mean)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age stopped meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) + 
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=c("≤45","50","55","60","65","70+")) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(pf="(a) Physical Functioning",
                                         rp="(b) Role Physical",
                                         bp="(c) Bodily pain",
                                         gh="(d) General Health",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme

ggsave(paste0(workdir,"Results/secondary-sustained.tiff"),
       width = 1000,
       height = 1000,
       units = "px")

# 3.2 Initiating activity plots
pr_init_fig <- ggplot(pr_fig_init[which(pr_fig_sust$outcome=="pcsa" | pr_fig_sust$outcome=="mcsa"),],
                      aes(x=result, y=mean)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age began meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) +
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=c("≤45","50","55","60","65","70+")) +
  facet_wrap(outcome ~ .,
             ncol=1,
             labeller=labeller(outcome=c(pcsa="(a) Physical Component Score",
                                         mcsa="(b) Mental Component Score"))) +
  figure_theme

ggsave(paste0(workdir,"Results/primary-initiation.tiff"),
       width = 1000,
       height = 1000,
       units = "px")

sc_init_fig <- ggplot(pr_fig_init[which(pr_fig_sust$outcome!="pcsa" & pr_fig_sust$outcome!="mcsa"),],
                      aes(x=result, y=mean)) +
  geom_errorbar(aes(ymin=mean-(qnorm(0.9975)*se), ymax=mean+(qnorm(0.9975)*se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age began meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) + 
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +
  scale_x_discrete(labels=c("≤45","50","55","60","65","70+")) +
  facet_wrap(outcome ~ .,
             ncol=2,
             labeller=labeller(outcome=c(pf="(a) Physical Functioning",
                                         rp="(b) Role Physical",
                                         bp="(c) Bodily pain",
                                         gh="(d) General Health",
                                         vt="(e) Vitality",
                                         sf="(f) Social Functioning",
                                         re="(g) Role Emotional",
                                         mh="(h) Mental Health"))) +
  figure_theme

ggsave(paste0(workdir,"Results/secondary-initiation.tiff"),
       width = 1000,
       height = 1000,
       units = "px")
