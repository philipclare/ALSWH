######################################################################################
##   
## Effects of physical activity on incident obesity
## Create plots from results of LTMLE analyses
## Date: 03 July 2025
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://osf.io/fyszg
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
# 1.1. Specify paths to Katana/windows/Mac paths based on system
if (Sys.info()[['sysname']]=="Linux") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/Obesity/"
} else if (Sys.info()[['sysname']]=="Windows") {
  workdir <- "Y:/PRJ-prc_alswh/Paper 3 - Obesity/"
} else if (Sys.info()[['sysname']]=="Darwin") {
  workdir <- "/Volumes/research-data/PRJ-prc_alswh/Paper 3 - Obesity/" # MAC
}

libs <- c("Amelia","ggplot2","tidyr","ggpubr","openxlsx","stringr","grid")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load the pooled results and clean for figure creation
#-------------------------------------------------------------------------------------

res_mn_pr <- readRDS(file=paste0(workdir,"Results/Processed/means - binary.rds"))
res_rr_pr <- readRDS(file=paste0(workdir,"Results/Processed/rrs - binary.rds"))
res_rd_pr <- readRDS(file=paste0(workdir,"Results/Processed/rds - binary.rds"))

res_mn_cat <- readRDS(file=paste0(workdir,"Results/Processed/means - categorical.rds"))
res_rr_cat <- readRDS(file=paste0(workdir,"Results/Processed/rrs - categorical.rds"))
res_rd_cat <- readRDS(file=paste0(workdir,"Results/Processed/rds - categorical.rds"))

res_mn_sev <- readRDS(file=paste0(workdir,"Results/Processed/means - severe outcome.rds"))
res_rr_sev <- readRDS(file=paste0(workdir,"Results/Processed/rrs - severe outcome.rds"))
res_rd_sev <- readRDS(file=paste0(workdir,"Results/Processed/rds - severe outcome.rds"))

res_mn_five <- readRDS(file=paste0(workdir,"Results/Processed/means - five percent outcome.rds"))
res_rr_five <- readRDS(file=paste0(workdir,"Results/Processed/rrs - five percent outcome.rds"))
res_rd_five <- readRDS(file=paste0(workdir,"Results/Processed/rds - five percent outcome.rds"))

res_mn_ten <- readRDS(file=paste0(workdir,"Results/Processed/means - ten percent outcome.rds"))
res_rr_ten <- readRDS(file=paste0(workdir,"Results/Processed/rrs - ten percent outcome.rds"))
res_rd_ten <- readRDS(file=paste0(workdir,"Results/Processed/rds - ten percent outcome.rds"))

res_mn_strat <- readRDS(file=paste0(workdir,"Results/Processed/means - education stratified.rds"))
res_rr_strat <- readRDS(file=paste0(workdir,"Results/Processed/rrs - education stratified.rds"))
res_rd_strat <- readRDS(file=paste0(workdir,"Results/Processed/rds - education stratified.rds"))

res_mn_sns1 <- readRDS(file=paste0(workdir,"Results/Processed/means - sensitivity 1 - descendant adjustment.rds"))
res_rr_sns1 <- readRDS(file=paste0(workdir,"Results/Processed/rrs - sensitivity 1 - descendant adjustment.rds"))
res_rd_sns1 <- readRDS(file=paste0(workdir,"Results/Processed/rds - sensitivity 1 - descendant adjustment.rds"))

res_mn_sns2 <- readRDS(file=paste0(workdir,"Results/Processed/means - sensitivity 2 - drop fully imputed.rds"))
res_rr_sns2 <- readRDS(file=paste0(workdir,"Results/Processed/rrs - sensitivity 2 - drop fully imputed.rds"))
res_rd_sns2 <- readRDS(file=paste0(workdir,"Results/Processed/rds - sensitivity 2 - drop fully imputed.rds"))

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
        legend.position="none")

######################################################################################
# 4. Primary Analysis Plots
#-------------------------------------------------------------------------------------

# 4.1 Incidence-risk
mn_pr <- ggplot(res_mn_pr,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of obesity") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 4.2 Relative risk
rr_pr <- ggplot(res_rr_pr,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.1, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio in obesity") +
  expand_limits(y=c(0, 2.0)) +
  scale_y_continuous(breaks=seq(0, 2.0, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 4.2. Risk difference
rd_pr <- ggplot(res_rd_pr,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference in obesity") +
  expand_limits(y=c(-0.05, 0.1)) +
  scale_y_continuous(breaks=seq(-0.05, 0.1, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 4.4. Combined difference plot
diff_pr <- ggarrange(rr_pr + rremove("xlab")  + rremove("x.text"),
                     rd_pr,
                     ncol=1,
                     align="v")

mn_pr
diff_pr

# 4.5. Save plots
ggsave(paste0(workdir,"Results/f1-incidence-risk.tiff"),
       mn_pr,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/f2-differences.tiff"),
       diff_pr,
       width = 2400,
       height = 2200,
       units = "px")

######################################################################################
# 5. 300+ analysis plots
#-------------------------------------------------------------------------------------

# 5.1 Incidence-risk
mn_300 <- ggplot(res_mn_cat[which(res_mn_cat$exp=="exceed"),],aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of obesity") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 5.2 Relative risk
rr_300 <- ggplot(res_rr_cat[which(res_rr_cat$exp=="exceed"),],aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.1, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio") +
  expand_limits(y=c(0, 2.0)) +
  scale_y_continuous(breaks=seq(0, 2.0, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 5.3. Risk difference
rd_300 <- ggplot(res_rd_cat[which(res_rd_cat$exp=="exceed"),],aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference") +
  expand_limits(y=c(-0.05, 0.1)) +
  scale_y_continuous(breaks=seq(-0.05, 0.1, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 5.4. Combined difference plot
diff_300 <- ggarrange(rr_300  + rremove("xlab")  + rremove("x.text"),
                      rd_300,
                      ncol=1,
                      align="v")

mn_300
diff_300

# 5.5. Save plots
ggsave(paste0(workdir,"Results/s1-incidence-risk-300.tiff"),
       mn_300,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/s2-differences-300.tiff"),
       diff_300,
       width = 2400,
       height = 2200,
       units = "px")

######################################################################################
# 6. Severe obesity outcome plots
#-------------------------------------------------------------------------------------

# 6.1 Incidence-risk
mn_sev <- ggplot(res_mn_sev,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.003, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of severe obesity") +
  expand_limits(y=c(0, 0.05)) +
  scale_y_continuous(breaks=seq(0, 0.05, by = 0.01), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 6.2 Relative risk
rr_sev <- ggplot(res_rr_sev,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.2, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio") +
  expand_limits(y=c(0, 2.5)) +
  scale_y_continuous(breaks=seq(0, 2.5, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 6.3. Risk difference
rd_sev <- ggplot(res_rd_sev,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference") +
  expand_limits(y=c(-0.1, 0.05)) +
  scale_y_continuous(breaks=seq(-0.1, 0.05, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 6.4. Combined difference plot
diff_sev <- ggarrange(rr_sev  + rremove("xlab")  + rremove("x.text"),
                      rd_sev,
                      ncol=1,
                      align="v")

mn_sev
diff_sev

# 6.5. Save plots
ggsave(paste0(workdir,"Results/s3-incidence-risk-severe-outcome.tiff"),
       mn_sev,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/s4-differences-severe-outcome.tiff"),
       diff_sev,
       width = 2400,
       height = 2200,
       units = "px")

######################################################################################
# 7. Five percent weight increase
#-------------------------------------------------------------------------------------

# 7.1 Incidence-risk
mn_five <- ggplot(res_mn_five,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.05, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of severe obesity") +
  expand_limits(y=c(0, 0.8)) +
  scale_y_continuous(breaks=seq(0, 0.8, by = 0.1), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 7.2 Relative risk
rr_five <- ggplot(res_rr_five,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.1, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio") +
  expand_limits(y=c(0, 1.5)) +
  scale_y_continuous(breaks=seq(0, 1.5, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 7.3. Risk difference
rd_five <- ggplot(res_rd_five,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.02, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference") +
  expand_limits(y=c(-0.05, 0.2)) +
  scale_y_continuous(breaks=seq(-0.05, 0.2, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 7.4. Combined difference plot
diff_five <- ggarrange(rr_five  + rremove("xlab")  + rremove("x.text"),
                      rd_five,
                      ncol=1,
                      align="v")

mn_five
diff_five

# 7.5. Save plots
ggsave(paste0(workdir,"Results/s5-incidence-risk-five-percent.tiff"),
       mn_five,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/s6-differences-five-percent.tiff"),
       diff_five,
       width = 2400,
       height = 2200,
       units = "px")

######################################################################################
# 8. Ten percent weight increase
#-------------------------------------------------------------------------------------

# 8.1 Incidence-risk
mn_ten <- ggplot(res_mn_ten,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.04, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of severe obesity") +
  expand_limits(y=c(0, 0.6)) +
  scale_y_continuous(breaks=seq(0, 0.6, by = 0.1), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 8.2 Relative risk
rr_ten <- ggplot(res_rr_ten,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.1, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio") +
  expand_limits(y=c(0, 2.0)) +
  scale_y_continuous(breaks=seq(0, 2.0, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 8.3. Risk difference
rd_ten <- ggplot(res_rd_ten,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.02, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference") +
  expand_limits(y=c(-0.05, 0.2)) +
  scale_y_continuous(breaks=seq(-0.05, 0.2, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 8.4. Combined difference plot
diff_ten <- ggarrange(rr_ten  + rremove("xlab")  + rremove("x.text"),
                      rd_ten,
                      ncol=1,
                      align="v")

mn_ten
diff_ten

# 8.5. Save plots
ggsave(paste0(workdir,"Results/s7-incidence-risk-ten-percent.tiff"),
       mn_ten,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/s8-differences-ten-percent.tiff"),
       diff_ten,
       width = 2400,
       height = 2200,
       units = "px")

######################################################################################
# 9. Stratified analysis
#-------------------------------------------------------------------------------------

res_mn_strat$offset <- ifelse(res_mn_strat$educ=="Tertiary/trade",-0.02,0.02)
res_rr_strat$offset <- ifelse(res_rr_strat$educ=="Tertiary/trade",0.2,-0.2)
res_rd_strat$offset <- ifelse(res_rd_strat$educ=="Tertiary/trade",0.02,-0.02)

# 9.1 Incidence-risk
mn_strat <- ggplot(res_mn_strat,aes(x=num, y=est, label=label, fill=educ, colour=educ)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  geom_text(aes(y = est + offset), position=pd, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of obesity") +
  expand_limits(y=c(0, 0.3)) +
  scale_y_continuous(breaks=seq(0, 0.3, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  scale_colour_manual(values = c("darkblue","darkred")) +
  scale_fill_manual(values = c("darkblue","darkred")) +
  figure_theme + theme(legend.position="bottom",legend.title=element_blank())

# 9.2 Relative risk
rr_strat <- ggplot(res_rr_strat,aes(x=num, y=est, label=label, fill=educ, colour=educ)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  geom_text(aes(y = est + offset), position=pd, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio") +
  expand_limits(y=c(0, 3.0)) +
  scale_y_continuous(breaks=seq(0, 3.0, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  scale_colour_manual(values = c("darkblue","darkred")) +
  scale_fill_manual(values = c("darkblue","darkred")) +
  figure_theme + theme(legend.position="bottom",legend.title=element_blank())

# 9.3. Risk difference
rd_strat <- ggplot(res_rd_strat,aes(x=num, y=est, label=label, fill=educ, colour=educ)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=20) +
  geom_text(aes(y = est + offset), position=pd, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference") +
  expand_limits(y=c(-0.05, 0.15)) +
  scale_y_continuous(breaks=seq(-0.05, 0.15, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  scale_colour_manual(values = c("darkblue","darkred")) +
  scale_fill_manual(values = c("darkblue","darkred")) +
  figure_theme + theme(legend.position="bottom",legend.title=element_blank())

# 9.4. Combined difference plot
diff_strat <- ggarrange(rr_strat  + rremove("xlab")  + rremove("x.text")  + rremove("legend"),
                        rd_strat,
                        ncol=1,
                        align="v")

mn_strat
diff_strat

# 9.5. Save plots
ggsave(paste0(workdir,"Results/s9-incidence-risk-stratified.tiff"),
       mn_strat,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/s10-differences-stratified.tiff"),
       diff_strat,
       width = 2400,
       height = 2200,
       units = "px")

######################################################################################
# 10. Sensitivity analysis 1 - Descendent adjustment
#-------------------------------------------------------------------------------------

# 10.1 Incidence-risk
mn_sns1 <- ggplot(res_mn_sns1,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of obesity") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 10.2 Relative risk
rr_sns1 <- ggplot(res_rr_sns1,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.1, nudge_x = 0, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio") +
  expand_limits(y=c(0, 2.0)) +
  scale_y_continuous(breaks=seq(0, 2.0, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 10.3. Risk difference
rd_sns1 <- ggplot(res_rd_sns1,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference") +
  expand_limits(y=c(-0.05, 0.1)) +
  scale_y_continuous(breaks=seq(-0.05, 0.1, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 10.4. Combined difference plot
diff_sns1 <- ggarrange(rr_sns1  + rremove("xlab")  + rremove("x.text"),
                       rd_sns1,
                       ncol=1,
                       align="v")

mn_sns1
diff_sns1

# 10.5. Save plots
ggsave(paste0(workdir,"Results/s11-incidence-risk-sens1.tiff"),
       mn_sns1,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/s12-differences-sens1.tiff"),
       diff_sns1,
       width = 2400,
       height = 2200,
       units = "px")

######################################################################################
# 11. Sensitivity analysis 2 - Omitting wholly imputed variables
#-------------------------------------------------------------------------------------

# 11.1 Incidence-risk
mn_sns2 <- ggplot(res_mn_sns2,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Incidence-risk of obesity") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 11.2 Relative risk
rr_sns2 <- ggplot(res_rr_sns2,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.1, nudge_x = 0, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk ratio") +
  expand_limits(y=c(0, 2.0)) +
  scale_y_continuous(breaks=seq(0, 2.0, by = 0.5), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 11.3. Risk difference
rd_sns2 <- ggplot(res_rd_sns2,aes(x=num, y=est, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.3, fill="darkblue") +
  geom_line(position=pd, colour="darkblue") +
  geom_point(position=pd, size=3, shape=20, colour="darkblue") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves meeting recommendations") +
  ylab("Risk difference") +
  expand_limits(y=c(-0.05, 0.1)) +
  scale_y_continuous(breaks=seq(-0.05, 0.1, by = 0.05), expand = c(0, 0), labels = scales::percent) +
  scale_x_continuous(breaks=seq(0, 7, by = 1)) +
  figure_theme

# 11.4. Combined difference plot
diff_sns2 <- ggarrange(rr_sns2  + rremove("xlab")  + rremove("x.text"),
                       rd_sns2,
                       ncol=1,
                       align="v")

mn_sns2
diff_sns2

# 11.5. Save plots
ggsave(paste0(workdir,"Results/s13-incidence-risk-sens2.tiff"),
       mn_sns2,
       width = 2400,
       height = 1200,
       units = "px")

ggsave(paste0(workdir,"Results/s14-differences-sens2.tiff"),
       diff_sns2,
       width = 2400,
       height = 2200,
       units = "px")

