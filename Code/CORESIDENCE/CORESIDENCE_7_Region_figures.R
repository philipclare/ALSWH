
library("ggplot2")

workdir <- "C:/Users/pcla5984/Dropbox (Sydney Uni)/Lancet series data analysis/Paper 1-analysis of living arrangement"

geog <- data.frame(cbind(region=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                         cont=c(1,2,2,3,4,4,5,3,1,2,2,3,5,2,3)))

size <- data.frame(read.csv(paste0(workdir,"/Results/Processed/Size - by region.csv")))
singlep <- read.csv(paste0(workdir,"/Results/Processed/Single-person - by region.csv"))

size_adj <- read.csv(paste0(workdir,"/Results/Processed/Size - by region pop-adj.csv"))
singlep_adj <- read.csv(paste0(workdir,"/Results/Processed/Single-person - by region pop-adj.csv"))

size <- merge(size,geog)
size$year <- size$year+1990
size$region <- factor(size$region, 
                      labels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe",
                               "Latin America and the Caribbean","North America","Northern Africa",
                               "Northern Europe","Pacific Islands","South-eastern Asia","Southern Asia",
                               "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"))
size$cont <- factor(size$cont,
                    levels=c(2,3,4,1,5),
                    labels=c("Asia","Europe","Americas","Oceania","Africa"))

size_adj <- merge(size_adj,geog)
size_adj$year <- size_adj$year+1990
size_adj$region <- factor(size$region, 
                          labels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe",
                                   "Latin America and the Caribbean","North America","Northern Africa",
                                   "Northern Europe","Pacific Islands","South-eastern Asia","Southern Asia",
                                   "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"))
size_adj$cont <- factor(size_adj$cont,
                        levels=c(2,3,4,1,5),
                        labels=c("Asia","Europe","Americas","Oceania","Africa"))

singlep <- merge(singlep,geog)
singlep$year <- singlep$year+1990
singlep$region <- factor(singlep$region, 
                         labels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe",
                                  "Latin America and the Caribbean","North America","Northern Africa",
                                  "Northern Europe","Pacific Islands","South-eastern Asia","Southern Asia",
                                  "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"))
singlep$cont <- factor(singlep$cont,
                       levels=c(2,3,4,1,5),
                       labels=c("Asia","Europe","Americas","Oceania","Africa"))

singlep_adj <- merge(singlep_adj,geog)
singlep_adj$year <- singlep_adj$year+1990
singlep_adj$region <- factor(singlep_adj$region, 
                             labels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe",
                                      "Latin America and the Caribbean","North America","Northern Africa",
                                      "Northern Europe","Pacific Islands","South-eastern Asia","Southern Asia",
                                      "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"))
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
                   aes(x=year, y=b, ymin=ll, ymax=ul, color=region)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=region), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Mean") +
  figure_theme +
  facet_wrap(facets=size$cont,
             nrow=5)
ggsave(size_fig_a,
       filename = paste0(workdir,"/Results/Size - by region.jpg"),
       width = 14,
       height = 20,
       units = "cm")

size_fig_b <- ggplot(size_adj,
                     aes(x=year, y=b, ymin=ll, ymax=ul, color=region)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=region), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Mean") +
  figure_theme +
  facet_wrap(facets=size$cont,
             nrow=5)
ggsave(size_fig_b,
       filename = paste0(workdir,"/Results/Size - by region pop-adj.jpg"),
       width = 14,
       height = 20,
       units = "cm")

singlep_fig_a <- ggplot(singlep,
                      aes(x=year, y=b, ymin=ll, ymax=ul, color=region)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=region), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Percentage") +
  figure_theme +
  facet_wrap(facets=size$cont,
             nrow=5)
ggsave(singlep_fig_a,
       filename = paste0(workdir,"/Results/Single-person - by region.jpg"),
       width = 14,
       height = 20,
       units = "cm")

singlep_fig_b <- ggplot(singlep_adj,
                        aes(x=year, y=b, ymin=ll, ymax=ul, color=region)) +
  geom_ribbon(aes(ymin=ll, ymax=ul, x=year, fill=region), alpha=0.2, linetype=0) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Percentage") +
  figure_theme +
  facet_wrap(facets=size$cont,
             nrow=5)
ggsave(singlep_fig_b,
       filename = paste0(workdir,"/Results/Single-person - by region pop-adj.jpg"),
       width = 14,
       height = 20,
       units = "cm")
