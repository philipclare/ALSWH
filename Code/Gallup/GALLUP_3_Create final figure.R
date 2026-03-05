
# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("dplyr","lemon","ggplot2","ggsci","ggthemes","gtable","haven","data.table","openxlsx","readxl","survey")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

workdir <- "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Gallup/"

live_alone <- read.xlsx(paste0(workdir,"Lives_alone_3agecats.xlsx"),sheet = "Model-predicted probabilities")

live_alone$lower <- ifelse(live_alone$estimate-qnorm(0.975)*live_alone$std.error<0,0,live_alone$estimate-qnorm(0.975)*live_alone$std.error)
live_alone$upper <- ifelse(live_alone$estimate+qnorm(0.975)*live_alone$std.error>1,1,live_alone$estimate+qnorm(0.975)*live_alone$std.error)

live_alone$UN.Region <- factor(live_alone$UN.Region,
                               levels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe","Latin America/Caribbean",
                                        "North America","Northern Africa","Northern Europe","South-eastern Asia","Southern Asia",
                                        "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"),
                               labels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe","Latin America/Caribbean",
                                        "North America","Northern Africa","Northern Europe","South-eastern Asia","Southern Asia",
                                        "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"))

support <- read.xlsx(paste0(workdir,"Social_support_3agecats.xlsx"),sheet = "Sheet1")

support$lower <- ifelse(support$estimate-qnorm(0.975)*support$std.error<0,0,support$estimate-qnorm(0.975)*support$std.error)
support$upper <- ifelse(support$estimate+qnorm(0.975)*support$std.error>1,1,support$estimate+qnorm(0.975)*support$std.error)

support$UN.Region <- factor(support$UN.Region,
                            levels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe","Latin America/Caribbean",
                                     "North America","Northern Africa","Northern Europe","South-eastern Asia","Southern Asia",
                                     "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"),
                            labels=c("Australia and New Zealand","Central Asia","Eastern Asia","Eastern Europe","Latin America/Caribbean",
                                     "North America","Northern Africa","Northern Europe","South-eastern Asia","Southern Asia",
                                     "Southern Europe","Sub-Saharan Africa","Western Asia","Western Europe"))

margins_region_year <- read.csv(paste0(workdir,"plotted_GWP_complete.csv"))
margins_region_year <- rename(margins_region_year, Region = UN.Region)
region <- read_xlsx("C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-analysis of living arrangement/Data/UN Regions.xlsx")[,c(4,5)]
region <- region[!duplicated(region), ]
margins_region_year <- merge(as.data.table(margins_region_year),as.data.table(region))

margins_region_year$Region <- factor(margins_region_year$Region,
                                     levels=c("Central Asia","Eastern Asia","South-eastern Asia","Southern Asia","Western Asia",
                                              "Eastern Europe","Northern Europe","Southern Europe","Western Europe",
                                              "Latin America/Caribbean","North America","Northern Africa","Sub-Saharan Africa","Australia and New Zealand"),
                                     labels=c("Central Asia","Eastern Asia","South-eastern Asia","Southern Asia","Western Asia",
                                              "Eastern Europe","Northern Europe","Southern Europe","Western Europe",
                                              "Latin America/Caribbean","North America","Northern Africa","Sub-Saharan Africa","Australia and New Zealand"))

margins_region_year$lower <- ifelse(margins_region_year$estimate-qnorm(0.975)*margins_region_year$std.error<0,0,margins_region_year$estimate-qnorm(0.975)*margins_region_year$std.error)
margins_region_year$upper <- ifelse(margins_region_year$estimate+qnorm(0.975)*margins_region_year$std.error>1,1,margins_region_year$estimate+qnorm(0.975)*margins_region_year$std.error)


live_alone_figure <- ggplot(data=live_alone,
                            aes(x=YEAR_CALENDAR, y=estimate, colour=Region2, fill=Region2, linetype=Age_Group)) +
  geom_line(linewidth=0.2) +
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,colour=NA) +
  scale_x_continuous(limits=c(2009,2022), breaks=seq(2009,2022, by = 4), expand = c(0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.6), breaks=seq(0,0.6, by = 0.2), expand = c(0, 0)) +
  ylab("Live alone %") + 
  xlab("Year") +
  theme_classic(base_size = 7) +
  theme(legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size=5),
        legend.title=element_blank(),
        legend.box = "horizontal",
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01))+
  facet_wrap(~UN.Region,
             labeller=labeller(UN.Region=c(`Australia and New Zealand` = "(a) Australia and New Zealand",
                                           `Central Asia` = "(b) Central Asia",
                                           `Eastern Asia` = "(c) Eastern Asia",
                                           `Eastern Europe` = "(d) Eastern Europe",
                                           `Latin America/Caribbean` = "(e) Latin America/Caribbean",
                                           `North America` = "(f) North America",
                                           `Northern Africa` = "(g) Northern Africa",
                                           `Northern Europe` = "(h) Northern Europe",
                                           `South-eastern Asia` = "(i) South-eastern Asia",
                                           `Southern Asia` = "(j) Southern Asia",
                                           `Southern Europe` = "(k) Southern Europe",
                                           `Sub-Saharan Africa` = "(l) Sub-Saharan Africa",
                                           `Western Asia` = "(m) Western Asia",
                                           `Western Europe` = "(n) Western Europe"))) +
  scale_color_lancet() + scale_fill_lancet()

live_alone_figure <- reposition_legend(live_alone_figure, 'center', panel=c('panel-4-3','panel-4-4'))

ggsave(paste0(workdir,"Livealone_regional_trends_by_sex.jpg"),
       live_alone_figure,
       width=15,
       height=10,
       units="cm")

support_figure <- ggplot(data=support,
                         aes(x=YEAR_CALENDAR, y=estimate, colour=Region2, fill=Region2, linetype=Age_Group)) +
  geom_line(linewidth=0.2) +
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,colour=NA) +
  scale_x_continuous(limits=c(2009,2022), breaks=seq(2009,2022, by = 4), expand = c(0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent, limits=c(0.4,1), breaks=seq(0.4,1, by = 0.2), expand = c(0, 0)) +
  ylab("Social support %") + 
  xlab("Year") +
  theme_classic(base_size = 7) +
  theme(legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size=5),
        legend.title=element_blank(),
        legend.box = "horizontal",
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01))+
  facet_wrap(~UN.Region,
             labeller=labeller(UN.Region=c(`Australia and New Zealand` = "(a) Australia and New Zealand",
                                           `Central Asia` = "(b) Central Asia",
                                           `Eastern Asia` = "(c) Eastern Asia",
                                           `Eastern Europe` = "(d) Eastern Europe",
                                           `Latin America/Caribbean` = "(e) Latin America/Caribbean",
                                           `North America` = "(f) North America",
                                           `Northern Africa` = "(g) Northern Africa",
                                           `Northern Europe` = "(h) Northern Europe",
                                           `South-eastern Asia` = "(i) South-eastern Asia",
                                           `Southern Asia` = "(j) Southern Asia",
                                           `Southern Europe` = "(k) Southern Europe",
                                           `Sub-Saharan Africa` = "(l) Sub-Saharan Africa",
                                           `Western Asia` = "(m) Western Asia",
                                           `Western Europe` = "(n) Western Europe"))) +
  scale_color_lancet() + scale_fill_lancet()

support_figure <- reposition_legend(support_figure, 'center', panel=c('panel-4-3','panel-4-4'))

ggsave(paste0(workdir,"Support_regional_trends_by_sex.jpg"),
       support_figure,
       width=15,
       height=10,
       units="cm")