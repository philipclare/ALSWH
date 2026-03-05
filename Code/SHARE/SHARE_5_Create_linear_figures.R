
# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("ggplot2","ggsci","ggthemes","haven")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

workdir <- "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-SHARE/"

share_scale_data <- read.csv(paste0(workdir,"Results/linear_scale_by_wave.csv"))
colnames(share_scale_data) <- c("region","year","est","se")
share_scale_data$year <- factor(share_scale_data$year,
                                labels=c("2013","2015","2017","2019/20","2021/22"))
share_scale_data$region <- factor(share_scale_data$region,
                                  labels=c("Eastern","Northern","Southern","Western"))
share_scale_data$sex <- "overall"

share_scale_data_male <- read.csv(paste0(workdir,"Results/linear_scale_by_wave_male.csv"))
colnames(share_scale_data_male) <- c("region","year","est","se")
share_scale_data_male$year <- factor(share_scale_data_male$year,
                                     labels=c("2013","2015","2017","2019/20","2021/22"))
share_scale_data_male$region <- factor(share_scale_data_male$region,
                                       labels=c("Eastern","Northern","Southern","Western","Western Asia"))
share_scale_data_male$sex <- "male"

share_scale_data_female <- read.csv(paste0(workdir,"Results/linear_scale_by_wave_female.csv"))
colnames(share_scale_data_female) <- c("region","year","est","se")
share_scale_data_female$year <- factor(share_scale_data_female$year,
                                       labels=c("2013","2015","2017","2019/20","2021/22"))
share_scale_data_female$region <- factor(share_scale_data_female$region,
                                         labels=c("Eastern","Northern","Southern","Western","Western Asia"))
share_scale_data_female$sex <- "female"

share_scale_data_sex <- rbind(share_scale_data,share_scale_data_male,share_scale_data_female)
share_scale_data_sex$sex <- factor(share_scale_data_sex$sex,
                                   levels=c("overall","female","male"),
                                   labels=c("Overall","Female","Male"))

share_single_data <- read.csv(paste0(workdir,"Results/linear_single_by_wave.csv"))
colnames(share_single_data) <- c("region","year","est","se")
share_single_data$year <- factor(share_single_data$year,
                                 labels=c("2013","2015","2017","2019/20","2021/22"))
share_single_data$region <- factor(share_single_data$region,
                                   labels=c("Eastern","Northern","Southern","Western"))
share_single_data$sex <- "overall"

share_single_data_male <- read.csv(paste0(workdir,"Results/linear_single_by_wave_male.csv"))
colnames(share_single_data_male) <- c("region","year","est","se")
share_single_data_male$year <- factor(share_single_data_male$year,
                                      labels=c("2013","2015","2017","2019/20","2021/22"))
share_single_data_male$region <- factor(share_single_data_male$region,
                                        labels=c("Eastern","Northern","Southern","Western","Western Asia"))
share_single_data_male$sex <- "male"

share_single_data_female <- read.csv(paste0(workdir,"Results/linear_single_by_wave_female.csv"))
colnames(share_single_data_female) <- c("region","year","est","se")
share_single_data_female$year <- factor(share_single_data_female$year,
                                        labels=c("2013","2015","2017","2019/20","2021/22"))
share_single_data_female$region <- factor(share_single_data_female$region,
                                          labels=c("Eastern","Northern","Southern","Western","Western Asia"))
share_single_data_female$sex <- "female"

share_single_data_sex <- rbind(share_single_data,share_single_data_male,share_single_data_female)
share_single_data_sex$sex <- factor(share_single_data_sex$sex,
                                    levels=c("overall","female","male"),
                                    labels=c("Overall","Female","Male"))

share_region0 <- read.csv(paste0(workdir,"Results/scale_region0.csv"))
colnames(share_region0) <- c("est","se","country","n","wave_str")
share_region0$region <- 0
share_region1 <- read.csv(paste0(workdir,"Results/scale_region1.csv"))
colnames(share_region1) <- c("est","se","country","n","wave_str")
share_region1$region <- 1
share_region2 <- read.csv(paste0(workdir,"Results/scale_region2.csv"))
colnames(share_region2) <- c("est","se","country","n","wave_str")
share_region2$region <- 2
share_region3 <- read.csv(paste0(workdir,"Results/scale_region3.csv"))
colnames(share_region3) <- c("est","se","country","n","wave_str")
share_region3$region <- 3
share_region <- rbind(share_region0,share_region1,share_region2,share_region3)
share_region$country <- factor(share_region$country,
                               levels=c(11,12,13,14,15,16,17,18,19,20,23,25,28,29,30,31,32,33,34,35,47,48,51,53,55,57,59,61,63),
                               labels=c('Austria','Germany','Sweden','Netherlands','Spain','Italy','France',"Denmark","Greece",
                                        "Switzerland","Belgium","Israel","Czech Republic","Poland","Ireland","Luxembourg",
                                        "Hungary","Portugal","Slovenia","Estonia","Croatia","Lithuania","Bulgaria","Cyprus",
                                        "Finland","Latvia","Malta","Romania","Slovakia"))
share_region$region <- factor(share_region$region,
                              labels=c("Eastern","Northern","Southern","Western"))
share_region$year <- as.numeric(gsub(".*?([0-9]+).*", "\\1", share_region$wave_str))
share_region$year <- factor(share_region$year,
                            labels=c("2013","2015","2017","2019/20","2021/22"))
share_region$n <- share_region$n/1000

abbr <- read.csv(paste0(workdir,"Data/abbreviations.csv"))

share_region <- merge(share_region, abbr, by="country")

SHARE_scale_figure_sex <- ggplot(share_scale_data_sex[which(share_scale_data_sex$region!="Western Asia"),],
                                 aes(x=year, y=est, group=sex, colour=sex, fill=sex)) +
  geom_line(aes(linetype=sex)) + 
  geom_ribbon(aes(x=year,ymin=est-qnorm(0.975)*se,ymax=est+qnorm(0.975)*se, fill=sex), colour=NA,alpha=0.2) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.3), breaks=seq(0,0.3, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  facet_wrap(~region,
             ncol=2,
             labeller=labeller(region=c(Eastern="(a) Eastern Europe",
                                     Northern="(b) Northern Europe",
                                     Southern="(b) Southern Europe",
                                     Western="(b) Western Europe")))+
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
  scale_color_lancet() + scale_fill_lancet()

SHARE_single_figure_sex <- ggplot(share_single_data_sex[which(share_single_data_sex$region!="Western Asia"),],
                                  aes(x=year, y=est, group=sex, colour=sex, fill=sex)) +
  geom_line(aes(linetype=sex)) + 
  geom_ribbon(aes(x=year,ymin=est-qnorm(0.975)*se,ymax=est+qnorm(0.975)*se, fill=sex), colour=NA,alpha=0.2) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.2), breaks=seq(0,0.2, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  facet_wrap(~region,
             ncol=2,
             labeller=labeller(region=c(Eastern="(a) Eastern Europe",
                                        Northern="(b) Northern Europe",
                                        Southern="(b) Southern Europe",
                                        Western="(b) Western Europe"))) +
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
  scale_color_lancet() + scale_fill_lancet()


SHARE_scale_figure_sex
SHARE_single_figure_sex

ggsave(paste0(workdir,"Results/SHARE_linear_scale_by_sex.jpg"),
       SHARE_scale_figure_sex)
ggsave(paste0(workdir,"Results/SHARE_linear_single_by_sex.jpg"),
       SHARE_single_figure_sex)

israel_figure <- ggplot(share_scale_data_sex[which(share_scale_data_sex$region=="Western Asia"),],
                                 aes(x=year, y=est, group=sex, colour=sex, fill=sex)) +
  geom_line(aes(linetype=sex)) + 
  geom_ribbon(aes(x=year,ymin=est-qnorm(0.975)*se,ymax=est+qnorm(0.975)*se, fill=sex), colour=NA,alpha=0.2) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.4), breaks=seq(0,0.4, by = 0.1), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
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
  scale_color_lancet() + scale_fill_lancet()

ggsave(paste0(workdir,"Results/israel_scale.jpg"),
       israel_figure)