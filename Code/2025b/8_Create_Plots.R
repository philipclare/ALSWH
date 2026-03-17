######################################################################################
##   
## Causal analysis of the effects of loneliness on mortality
## Create plots from results of LTMLE analyses
## Date: 15 November 2023
## Authors: Philip Clare
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## OSF Registration: https://doi.org/10.17605/OSF.IO/H9RZN
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

rm(list = ls())
# workdir <- "Y:/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"
workdir <- "/Volumes/research-data/PRJ-Loneliness_ALSWH/Paper 1. Causal effect of loneliness on mortality/"

libs <- c("Amelia","ggplot2","tidyr","ggpubr","openxlsx","stringr")
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
pr_mi_res <- readRDS(file=paste0(workdir,"Results/Raw results/primary pooled means.rds"))

fig_data_num_p <- as.data.frame(pr_mi_res[,c(1:4)])
colnames(fig_data_num_p) <- c("num","prob","lb","ub")
fig_data_consec_p <- as.data.frame(pr_mi_res[,c(1,5,6,7)])
colnames(fig_data_consec_p) <- c("num","prob","lb","ub")

# 2.2 Sensitivty analysis 1 results
s1_mi_res <- readRDS(file=paste0(workdir,"Results/Raw results/s1 pooled means.rds"))

fig_data_num_s1 <- as.data.frame(s1_mi_res[,c(1:4)])
colnames(fig_data_num_s1) <- c("num","prob","lb","ub")
fig_data_consec_s1 <- as.data.frame(s1_mi_res[,c(1,5,6,7)])
colnames(fig_data_consec_s1) <- c("num","prob","lb","ub")

# 2.3 Sensitivty analysis 2 results
s2_mi_res <- readRDS(file=paste0(workdir,"Results/Raw results/s2 pooled means.rds"))

fig_data_num_s2 <- as.data.frame(s2_mi_res[,c(1:4)])
colnames(fig_data_num_s2) <- c("num","prob","lb","ub")
fig_data_consec_s2 <- as.data.frame(s2_mi_res[,c(1,5,6,7)])
colnames(fig_data_consec_s2) <- c("num","prob","lb","ub")

# 2.3 Sensitivty analysis 3 results
s3_mi_res <- readRDS(file=paste0(workdir,"Results/Raw results/s3 pooled means.rds"))

fig_data_num_s3 <- as.data.frame(s3_mi_res[,c(1:4)])
colnames(fig_data_num_s3) <- c("num","prob","lb","ub")
fig_data_consec_s3 <- as.data.frame(s3_mi_res[,c(1,5,6,7)])
colnames(fig_data_consec_s3) <- c("num","prob","lb","ub")

######################################################################################
# 3. Save summary output to excel for tables
#-------------------------------------------------------------------------------------

wb_mn <- createWorkbook()

addWorksheet(wb_mn,"Primary")
addWorksheet(wb_mn,"S1")
addWorksheet(wb_mn,"S2")
addWorksheet(wb_mn,"S3")

writeData(wb_mn,sheet="Primary",pr_mi_res,startRow=1)
writeData(wb_mn,sheet="S1",s1_mi_res,startRow=1)
writeData(wb_mn,sheet="S2",s2_mi_res,startRow=1)
writeData(wb_mn,sheet="S3",s3_mi_res,startRow=1)

saveWorkbook(wb_mn, file = paste0(workdir,"Results/Raw results/pr-results.xlsx"), overwrite = TRUE)

mi_rr_p <- readRDS(file=paste0(workdir,"Results/Raw results/primary pooled RRs.rds"))
mi_rr_s1 <- readRDS(file=paste0(workdir,"Results/Raw results/s1 pooled RRs.rds"))
mi_rr_s2 <- readRDS(file=paste0(workdir,"Results/Raw results/s2 pooled RRs.rds"))
mi_rr_s3 <- readRDS(file=paste0(workdir,"Results/Raw results/s3 pooled RRs.rds"))

wb_rr <- createWorkbook()

addWorksheet(wb_rr,"Primary")
addWorksheet(wb_rr,"S1")
addWorksheet(wb_rr,"S2")
addWorksheet(wb_rr,"S3")

writeData(wb_rr,sheet="Primary",mi_rr_p,startRow=1)
writeData(wb_rr,sheet="S1",mi_rr_s1,startRow=1)
writeData(wb_rr,sheet="S2",mi_rr_s2,startRow=1)
writeData(wb_rr,sheet="S3",mi_rr_s3,startRow=1)

saveWorkbook(wb_rr, file = paste0(workdir,"Results/Raw results/rr-results.xlsx"), overwrite = TRUE)

mi_rd_p <- readRDS(file=paste0(workdir,"Results/Raw results/primary pooled RDs.rds"))
mi_rd_s1 <- readRDS(file=paste0(workdir,"Results/Raw results/s1 pooled RDs.rds"))
mi_rd_s2 <- readRDS(file=paste0(workdir,"Results/Raw results/s2 pooled RDs.rds"))
mi_rd_s3 <- readRDS(file=paste0(workdir,"Results/Raw results/s3 pooled RDs.rds"))

wb_rd <- createWorkbook()

addWorksheet(wb_rd,"Primary")
addWorksheet(wb_rd,"S1")
addWorksheet(wb_rd,"S2")
addWorksheet(wb_rd,"S3")

writeData(wb_rd,sheet="Primary",mi_rd_p,startRow=1)
writeData(wb_rd,sheet="S1",mi_rd_s1,startRow=1)
writeData(wb_rd,sheet="S2",mi_rd_s2,startRow=1)
writeData(wb_rd,sheet="S3",mi_rd_s3,startRow=1)

saveWorkbook(wb_rd, file = paste0(workdir,"Results/Raw results/rd-results.xlsx"), overwrite = TRUE)

######################################################################################
# 4. Create labels
#-------------------------------------------------------------------------------------

fig_data_num_p$label <- paste0(sprintf('%.1f%%',fig_data_num_p$prob*100),"\n(",sprintf('%.1f%%',fig_data_num_p$lb*100),", ",sprintf('%.1f%%',fig_data_num_p$ub*100),")")
fig_data_consec_p$label <- paste0(sprintf('%.1f%%',fig_data_consec_p$prob*100),"\n(",sprintf('%.1f%%',fig_data_consec_p$lb*100),", ",sprintf('%.1f%%',fig_data_consec_p$ub*100),")")

fig_data_num_s1$label <- paste0(sprintf('%.1f%%',fig_data_num_s1$prob*100),"\n(",sprintf('%.1f%%',fig_data_num_s1$lb*100),", ",sprintf('%.1f%%',fig_data_num_s1$ub*100),")")
fig_data_consec_s1$label <- paste0(sprintf('%.1f%%',fig_data_consec_s1$prob*100),"\n(",sprintf('%.1f%%',fig_data_consec_s1$lb*100),", ",sprintf('%.1f%%',fig_data_consec_s1$ub*100),")")

fig_data_num_s2$label <- paste0(sprintf('%.1f%%',fig_data_num_s2$prob*100),"\n(",sprintf('%.1f%%',fig_data_num_s2$lb*100),", ",sprintf('%.1f%%',fig_data_num_s2$ub*100),")")
fig_data_consec_s2$label <- paste0(sprintf('%.1f%%',fig_data_consec_s2$prob*100),"\n(",sprintf('%.1f%%',fig_data_consec_s2$lb*100),", ",sprintf('%.1f%%',fig_data_consec_s2$ub*100),")")

fig_data_num_s3$label <- paste0(sprintf('%.1f%%',fig_data_num_s3$prob*100),"\n(",sprintf('%.1f%%',fig_data_num_s3$lb*100),", ",sprintf('%.1f%%',fig_data_num_s3$ub*100),")")
fig_data_consec_s3$label <- paste0(sprintf('%.1f%%',fig_data_consec_s3$prob*100),"\n(",sprintf('%.1f%%',fig_data_consec_s3$lb*100),", ",sprintf('%.1f%%',fig_data_consec_s3$ub*100),")")


######################################################################################
# 5. Define theme and common properties
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
# 6. Primary Analysis Plots
#-------------------------------------------------------------------------------------

# 6.1 Number of waves of loneliness
num_fig <- ggplot(fig_data_num_p, 
                         aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
    figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

num_fig

ggsave(paste0(workdir,"Results/num_waves_pr_20241220.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

# 6.2 Number of consecutive waves of loneliness
consec_fig <- ggplot(fig_data_consec_p, 
                  aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of consecutive waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

consec_fig

ggsave(paste0(workdir,"Results/consec_waves_pr_20241220.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

######################################################################################
# 7. Sensitivity analysis 1 - lower cut-point for loneliness
#-------------------------------------------------------------------------------------

# 7.1 Number of waves of loneliness
num_fig_s1 <- ggplot(fig_data_num_s1, 
                  aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

num_fig_s1

ggsave(paste0(workdir,"Results/num_waves_s1_20241220.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

# 7.2 Number of consecutive waves of loneliness
consec_fig_s1 <- ggplot(fig_data_consec_s1, 
                     aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of consecutive waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

consec_fig_s1

ggsave(paste0(workdir,"Results/consec_waves_s1_20241220.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

######################################################################################
# 8. Sensitivity analysis 2 - higher cut-point for loneliness
#-------------------------------------------------------------------------------------

# 8.1 Number of waves of loneliness
num_fig_s2 <- ggplot(fig_data_num_s2, 
                     aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.4)) +
  scale_y_continuous(breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

num_fig_s2

ggsave(paste0(workdir,"Results/num_waves_s2_20241220.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

# 8.2 Number of consecutive waves of loneliness
consec_fig_s2 <- ggplot(fig_data_consec_s2, 
                        aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of consecutive waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.4)) +
  scale_y_continuous(breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

consec_fig_s2

ggsave(paste0(workdir,"Results/consec_waves_s2_20241220.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

######################################################################################
# 9. Sensitivity analysis 2 - higher cut-point for loneliness
#-------------------------------------------------------------------------------------

# 9.1 Number of waves of loneliness
num_fig_s3 <- ggplot(fig_data_num_s3, 
                     aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

num_fig_s3

ggsave(paste0(workdir,"Results/num_waves_s3_20250109.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

# 9.2 Number of consecutive waves of loneliness
consec_fig_s3 <- ggplot(fig_data_consec_s3, 
                        aes(x=num, y=prob, label=label)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), position=pd, size=0.2, fill = "lightblue") +
  geom_line(position=pd, colour="navy") +
  geom_point(position=pd, size=2, shape=18, colour="navy") +
  geom_text(hjust = 0.5, nudge_y = -0.01, nudge_x = 0, size=2.5) +
  xlab("Number of consecutive waves of loneliness") +
  ylab("Incidence risk of all-cause mortality") +
  expand_limits(y=c(0, 0.2)) +
  scale_y_continuous(breaks=seq(0, 0.2, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 6, by = 1)) +
  figure_theme +
  scale_color_manual(values=c("darkred", "darkblue"))

consec_fig_s3

ggsave(paste0(workdir,"Results/consec_waves_s3_20250109.tiff"),
       width = 2400,
       height = 1600,
       units = "px")

