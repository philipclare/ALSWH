
# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("ggplot2","ggsci","ggthemes","haven","lemon","openxlsx")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

workdir <- "D:/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-ESS/"

ess_results <- read.xlsx(paste0(workdir,"Results/regional trends by sex.xlsx"))
ess_results_age <- read.xlsx(paste0(workdir,"Results/regional trends by age and sex.xlsx"))

ess_results$region <- factor(ess_results$region,
                             levels=c("Eastern Europe","Northern Europe","Southern Europe","Western Asia","Western Europe"),
                             labels=c("(a) Eastern Europe","(b) Northern Europe","(c) Southern Europe","(d) Western Asia","(e) Western Europe"))
ess_results$group <- factor(ess_results$group,
                            levels=c("overall","female","male"),
                            labels=c("Overall","Female","Male"))

ess_results_age$region <- factor(ess_results_age$region,
                                 levels=c("Eastern Europe","Northern Europe","Southern Europe","Western Asia","Western Europe"),
                                 labels=c("(a) Eastern Europe","(b) Northern Europe","(c) Southern Europe","(d) Western Asia","(e) Western Europe"))
ess_results_age$group <- factor(ess_results_age$group,
                            levels=c("overall","female","male"),
                            labels=c("Overall","Female","Male"))
ess_results_age$agecat <- factor(ess_results_age$agecat,
                                 levels=c("young","middle"),
                                 labels=c("Young (18-29)","Middle aged (30-59)"))

ess_figure <- ggplot(ess_results,
                                 aes(x=year, y=b, group=group, colour=group, fill=group)) +
  geom_line(aes(linetype=group)) + 
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul, fill=group), colour=NA,alpha=0.2) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.2), breaks=seq(0,0.2, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  facet_wrap(~region,
             ncol=1)+
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
  scale_color_lancet() + scale_fill_lancet()

ess_figure

ggsave(paste0(workdir,"Results/ESS figure 20260219.jpg"),
       ess_figure,
       width=1500,
       height=2000,
       units="px")

custom_labeler <- function(labels, multi_line = FALSE) {
  # Combine columns with a dash
  label_col <- do.call(paste, c(labels, sep = " - "))
  # Return as a list of characters
  list(label_col)
}

ess_figure_age <- ggplot(ess_results_age,
                     aes(x=year, y=b, group=group, colour=group, fill=group)) +
  geom_line(aes(linetype=group)) + 
  geom_ribbon(aes(x=year,ymin=ll,ymax=ul, fill=group), colour=NA,alpha=0.2) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.2), breaks=seq(0,0.2, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  facet_wrap(~region + agecat,
             ncol=2,
             labeller = custom_labeler) +
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
  scale_color_lancet() + scale_fill_lancet() 

ess_figure_age

ggsave(paste0(workdir,"Results/ESS figure by age 20260220.jpg"),
       ess_figure_age,
       width=2000,
       height=2000,
       units="px")