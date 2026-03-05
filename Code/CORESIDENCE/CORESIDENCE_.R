
library("ggplot2")

workdir <- "C:/Users/pcla5984/Dropbox (Sydney Uni)/Lancet series data analysis/Paper 1-analysis of living arrangement"

geog <- data.frame(cbind(hdi_cat=c(1,2,3,4),
                         cont=c(1,2,2,3,4,4,5,3,1,2,2,3,5,2,3)))

size <- data.frame(read.csv(paste0(workdir,"/Results/Processed/Size - by HDI.csv")))
singlep <- read.csv(paste0(workdir,"/Results/Processed/Single-person - by HDI.csv"))

size_adj <- read.csv(paste0(workdir,"/Results/Processed/Size - by HDI pop-adj.csv"))
singlep_adj <- read.csv(paste0(workdir,"/Results/Processed/Single-person - by HDI pop-adj.csv"))

size <- merge(size,geog)
size$year <- size$year+1990
size$hdi_cat <- factor(size$hdi_cat, 
                       labels=c("Low","Medium","High","Very high"))
size$cont <- factor(size$cont,
                    levels=c(2,3,4,1,5),
                    labels=c("Asia","Europe","Americas","Oceania","Africa"))

size_adj <- merge(size_adj,geog)
size_adj$year <- size_adj$year+1990
size_adj$hdi_cat <- factor(size$hdi_cat, 
                           labels=c("Low","Medium","High","Very high"))
size_adj$cont <- factor(size_adj$cont,
                        levels=c(2,3,4,1,5),
                        labels=c("Asia","Europe","Americas","Oceania","Africa"))

singlep <- merge(singlep,geog)
singlep$year <- singlep$year+1990
singlep$hdi_cat <- factor(singlep$hdi_cat, 
                          labels=c("Low","Medium","High","Very high"))
singlep$cont <- factor(singlep$cont,
                       levels=c(2,3,4,1,5),
                       labels=c("Asia","Europe","Americas","Oceania","Africa"))

singlep_adj <- merge(singlep_adj,geog)
singlep_adj$year <- singlep_adj$year+1990
singlep_adj$hdi_cat <- factor(singlep_adj$hdi_cat, 
                              labels=c("Low","Medium","High","Very high"))
singlep_adj$cont <- factor(singlep_adj$cont,
                           levels=c(2,3,4,1,5),
                           labels=c("Asia","Europe","Americas","Oceania","Africa"))

pd <- position_dodge(0.1)
figure_theme <- theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
        text = element_text(size = 16),
        axis.line = element_line(colour = 'grey80', linewidth = 0.3),
        axis.ticks = element_line(colour = "grey80", linewidth = 0.3),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01),
        legend.position="bottom",
        legend.text=element_text(size=5),
        legend.title=element_blank(),
        legend.key.size = unit(0.2, "cm"))

size_fig_a <- ggplot(size,
                     aes(x=year, y=b, ymin=ll, ymax=ul, color=hdi_cat)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=hdi_cat), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Mean") +
  scale_y_continuous(limits=c(2,7), breaks=seq(2, 7, by = 1), expand = c(0, 0)) +
  figure_theme
ggsave(size_fig_a,
       filename = paste0(workdir,"/Results/Size - by HDI.jpg"),
       width = 15,
       height = 9,
       units = "cm")

size_fig_b <- ggplot(size_adj,
                     aes(x=year, y=b, ymin=ll, ymax=ul, color=hdi_cat)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=hdi_cat), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Mean") +
  scale_y_continuous(limits=c(0,7), breaks=seq(0, 7, by = 1), expand = c(0, 0)) +
  figure_theme
ggsave(size_fig_b,
       filename = paste0(workdir,"/Results/Size - by HDI pop-adj.jpg"),
       width = 15,
       height = 9,
       units = "cm")

singlep_fig_a <- ggplot(singlep,
                        aes(x=year, y=b, ymin=ll, ymax=ul, color=hdi_cat)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=hdi_cat), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Percentage") +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.2), expand = c(0, 0)) +
  figure_theme
ggsave(singlep_fig_a,
       filename = paste0(workdir,"/Results/Single-person - by HDI.jpg"),
       width = 15,
       height = 9,
       units = "cm")

singlep_fig_b <- ggplot(singlep_adj,
                        aes(x=year, y=b, ymin=ll, ymax=ul, color=hdi_cat)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=hdi_cat), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Percentage") +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.2), expand = c(0, 0)) +
  figure_theme
ggsave(singlep_fig_b,
       filename = paste0(workdir,"/Results/Single-person - by HDI pop-adj.jpg"),
       width = 15,
       height = 9,
       units = "cm")
