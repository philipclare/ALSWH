
# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("ggplot2","ggpubr","ggsci","ggthemes","grid","gridExtra","haven","lemon","openxlsx")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

workdir <- "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Social cohesion"

hilda_coh <- read.xlsx(paste0(workdir,"/Results/HILDA results.xlsx"),
                       sheet = "cohesion")
hilda_rub <- read.xlsx(paste0(workdir,"/Results/HILDA results.xlsx"),
                       sheet = "rubbish")
hilda_van <- read.xlsx(paste0(workdir,"/Results/HILDA results.xlsx"),
                       sheet = "vandalism")

hrs_coh <- read.xlsx(paste0(workdir,"/Results/HRS results.xlsx"),
                     sheet = "cohesion")
hrs_rub <- read.xlsx(paste0(workdir,"/Results/HRS results.xlsx"),
                     sheet = "rubbish")
hrs_van <- read.xlsx(paste0(workdir,"/Results/HRS results.xlsx"),
                     sheet = "vandalism")

elsi_coh <- read.xlsx(paste0(workdir,"/Results/ELSI results.xlsx"),
                      sheet = "cohesion")
elsi_rub <- read.xlsx(paste0(workdir,"/Results/ELSI results.xlsx"),
                      sheet = "rubbish")
elsi_van <- read.xlsx(paste0(workdir,"/Results/ELSI results.xlsx"),
                      sheet = "vandalism")

hilda_coh$exp <- "cohesion"
hilda_coh$exp_level <- seq(1,7)
hilda_coh$exp <- factor(hilda_coh$exp,
                        levels = c("cohesion","rubbish","vandalism"),
                        labels = c("Social cohesion","Perception of rubbish in neighbourhood","Perception of vandalism in neighbourhood"))

hilda_rub$exp <- "rubbish"
hilda_rub$exp_level <- seq(1,5)
hilda_rub$exp_level <- factor(hilda_rub$exp_level,labels=c("Never \nhappens","Very \nrare","Not \ncommon","Fairly \ncommon","Very \ncommon"))
hilda_van$exp <- "vandalism"
hilda_van$exp_level <- seq(1,5)
hilda_van$exp_level <- factor(hilda_van$exp_level,labels=c("Never \nhappens","Very \nrare","Not \ncommon","Fairly \ncommon","Very \ncommon"))

hilda_neg <- rbind(hilda_rub[,c("exp","exp_level","b","se","ll","ul")],
                   hilda_van[,c("exp","exp_level","b","se","ll","ul")])
hilda_neg$exp <- factor(hilda_neg$exp,
                        levels = c("cohesion","rubbish","vandalism"),
                        labels = c("Social cohesion","Perception of rubbish in neighbourhood","Perception of vandalism in neighbourhood"))

hrs_coh$exp <- "cohesion"
hrs_coh$exp_level <- seq(1,7)
hrs_coh$exp <- factor(hrs_coh$exp,
                      levels = c("cohesion","rubbish","vandalism"),
                      labels = c("Social cohesion","Perception of rubbish in neighbourhood","Perception of vandalism in neighbourhood"))

hrs_rub$exp <- "rubbish"
hrs_rub$exp_level <- seq(1,7)
hrs_van$exp <- "vandalism"
hrs_van$exp_level <- seq(1,7)

hrs_neg <- rbind(hrs_rub[,c("exp","exp_level","b","se","ll","ul")],
                 hrs_van[,c("exp","exp_level","b","se","ll","ul")])
hrs_neg$exp <- factor(hrs_neg$exp,
                      levels = c("cohesion","rubbish","vandalism"),
                      labels = c("Social cohesion","Perception of rubbish in neighbourhood","Perception of vandalism in neighbourhood"))

elsi_coh$exp <- "cohesion"
elsi_coh$exp_level <- c("No","More or less","Yes")
elsi_coh$exp <- factor(elsi_coh$exp,
                       levels = c("cohesion","rubbish","vandalism"),
                       labels = c("Social cohesion","Perception of rubbish in neighbourhood","Perception of vandalism in neighbourhood"))

elsi_rub$exp2 <- "other"
elsi_rub$exp <- "rubbish"
elsi_rub$exp_level <- c("No","Yes")
elsi_van$exp2 <- "other"
elsi_van$exp <- "vandalism"
elsi_van$exp_level <- c("No","Yes")

elsi_neg <- rbind(elsi_rub[,c("exp2","exp","exp_level","b","se","ll","ul")],
                  elsi_van[,c("exp2","exp","exp_level","b","se","ll","ul")])

elsi_neg$exp <- factor(elsi_neg$exp,
                       levels=c("cohesion","rubbish","vandalism"),
                       labels = c("Social cohesion","Perception of rubbish in neighbourhood","Perception of vandalism in neighbourhood"))

hilda_figure_coh <- ggplot(hilda_coh,
                           aes(x=exp_level, y=b, group=exp, colour=exp, fill=exp)) +
  geom_line(aes(linetype=exp), show.legend = TRUE) + 
  geom_ribbon(aes(x=exp_level,ymin=ll,ymax=ul, fill=exp), colour=NA, alpha=0.2, show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.4), breaks=seq(0,0.4, by = 0.1), expand = c(0, 0)) +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Level of exposure") +
  labs(title = "(a) HILDA") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0,
                                  size = 10),
        legend.key=element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title=element_blank(),
        legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = -0.01),
        strip.text = element_text(colour = 'black')) +
  scale_linetype(drop=FALSE) +
  scale_color_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE) +
  scale_fill_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE)

hilda_figure_coh

hilda_figure_neg <- ggplot(hilda_neg,
                           aes(x=exp_level, y=b, group=exp, colour=exp, fill=exp)) +
  geom_line(aes(linetype=exp), show.legend = TRUE) + 
  geom_ribbon(aes(x=exp_level,ymin=ll,ymax=ul, fill=exp), colour=NA, alpha=0.2, show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.4), breaks=seq(0,0.4, by = 0.1), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Level of exposure") +
  theme_light() +
  theme(legend.key=element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title=element_blank(),
        legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = -0.01),
        strip.text = element_text(colour = 'black')) +
  scale_linetype(drop=FALSE) +
  scale_color_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE) +
  scale_fill_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE)

hilda_figure_neg

hrs_figure_coh <- ggplot(hrs_coh,
                     aes(x=exp_level, y=b, group=exp, colour=exp, fill=exp)) +
  geom_line(aes(linetype=exp), show.legend = TRUE) + 
  geom_ribbon(aes(x=exp_level,ymin=ll,ymax=ul, fill=exp), colour=NA, alpha=0.2, show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.4), breaks=seq(0,0.4, by = 0.1), expand = c(0, 0)) +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Level of exposure") +
  labs(title = "(B) HRS") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0,
                                  size = 10),
        legend.key=element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title=element_blank(),
        legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = -0.01),
        strip.text = element_text(colour = 'black')) +
  scale_linetype(drop=FALSE) +
  scale_color_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE) +
  scale_fill_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE)

hrs_figure_coh

hrs_figure_neg <- ggplot(hrs_neg,
                         aes(x=exp_level, y=b, group=exp, colour=exp, fill=exp)) +
  geom_line(aes(linetype=exp), show.legend = TRUE) + 
  geom_ribbon(aes(x=exp_level,ymin=ll,ymax=ul, fill=exp), colour=NA, alpha=0.2, show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.4), breaks=seq(0,0.4, by = 0.1), expand = c(0, 0)) +
  scale_x_continuous(limits=c(0,8), breaks=seq(1,7, by = 1), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Level of exposure") +
  theme_light() +
  theme(legend.key=element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title=element_blank(),
        legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = -0.01),
        strip.text = element_text(colour = 'black')) +
  scale_linetype(drop=FALSE) +
  scale_color_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE) +
  scale_fill_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE)

hrs_figure_neg

elsi_figure_coh <- ggplot(elsi_coh,
                      aes(x=exp_level, y=b, group=exp, colour=exp, fill=exp)) +
  geom_line(aes(linetype=exp), show.legend = TRUE) + 
  geom_ribbon(aes(x=exp_level,ymin=ll,ymax=ul, fill=exp), colour=NA, alpha=0.2, show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.2), breaks=seq(0,0.2, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Level of exposure") +
  labs(title = "(c) ELSI") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0,
                                  size = 10),
        legend.key=element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title=element_blank(),
        legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = -0.01),
        strip.text = element_text(colour = 'black')) +
  scale_linetype(drop=FALSE) +
  scale_color_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE) +
  scale_fill_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE)

elsi_figure_coh

elsi_figure_neg <- ggplot(elsi_neg,
                          aes(x=exp_level, y=b, group=exp, colour=exp, fill=exp)) +
  geom_line(aes(linetype=exp), show.legend = TRUE) + 
  geom_ribbon(aes(x=exp_level,ymin=ll,ymax=ul, fill=exp), colour=NA, alpha=0.2, show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.2), breaks=seq(0,0.2, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Level of exposure") +
  theme_light() +
  theme(legend.key=element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title=element_blank(),
        legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = -0.01),
        strip.text = element_text(colour = 'black')) +
  scale_linetype(drop=FALSE) +
  scale_color_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE) +
  scale_fill_manual(values=c("#00468B","#ED0000","#42B540"), drop=FALSE)

elsi_figure_neg

cohesion_figure <- ggarrange(hilda_figure_coh + theme(axis.title.y = element_blank(),
                                                      axis.title.x = element_blank()),
                             hilda_figure_neg + theme(axis.title.y = element_blank(),
                                                      axis.title.x = element_blank(),
                                                      axis.text.y = element_blank()),
                             hrs_figure_coh + theme(axis.title.y = element_blank(),
                                                    axis.title.x = element_blank()),
                             hrs_figure_neg + theme(axis.title.y = element_blank(),
                                                    axis.title.x = element_blank(),
                                                    axis.text.y = element_blank()),
                             elsi_figure_coh + theme(axis.title.y = element_blank()),
                             elsi_figure_neg + theme(axis.title.y = element_blank(),
                                                    axis.text.y = element_blank()),
                             ncol=2,nrow=3, 
                             align='hv',
                             common.legend = TRUE, legend="bottom") +
  theme(legend.title=element_blank())
cohesion_figure <- annotate_figure(cohesion_figure,
                                  left = text_grob("Loneliness %", rot = 90))
cohesion_figure

ggsave(paste0(workdir,"/Results/Combined social cohesion figure 20260303.jpg"),
       cohesion_figure)
# ggsave(paste0(workdir,"/Results/HRS figure 20260212.jpg"),
#        hrs_figure)
# ggsave(paste0(workdir,"/Results/ELSI figure 20260212.jpg"),
#        elsi_figure)
# 
# 
