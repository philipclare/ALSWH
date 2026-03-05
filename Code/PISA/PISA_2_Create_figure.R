
# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("ggplot2","ggthemes","haven","data.table","readr","readxl","survey")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

# create function for modelled trend
get_trend <- function (svydes) {
  fit <- svyglm(lonely_b ~ year,svydes,family="binomial")
  
  newdata <- data.frame(sex="Overall",year=unique(fit$model$year))
  
  predictions <- data.frame(newdata,
                            predict(fit,type="response",newdata=newdata))
  colnames(predictions) <- c("sex","year","p","se")
  
  predictions
  
}

get_sex_trend <- function (svydes) {
  fit <- svyglm(lonely_b ~ year*sex,svydes,family="binomial")
  
  newdata <- rbind(data.frame(sex="Female",year=unique(fit$model$year)),
                data.frame(sex="Male",year=unique(fit$model$year)))
  
  predictions <- data.frame(newdata,
                            predict(fit,type="response",newdata=newdata))
  colnames(predictions) <- c("sex","year","p","se")
  
  predictions
  
}

get_sig <- function (svydes) {
  fit1 <- svyglm(lonely_b ~ year,svydes,family="binomial")
  fit2 <- svyglm(lonely_b ~ year*sex,svydes,family="binomial")
  
  lincom <- svycontrast(fit2,c("year"=1,"year:sexMale"=1))
  est <- lincom["contrast"]
  se <- SE(lincom)
  df <- summary(fit2)$df[2]
  p_lincom <- 2 * pt(-abs(as.numeric(est/se)),df)
  
  p <- c(coef(summary(fit1))[2,4],
         coef(summary(fit1))[2,4],
         p_lincom)

  p
  
}

workdir <- "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-PISA/"

pisa_data <- read_dta(paste0(workdir,"PISA data/Analysis data/Combined_data.dta"))

pisa_data <- zap_label(pisa_data)
pisa_data <- zap_labels(pisa_data)
pisa_data <- zap_formats(pisa_data)

pisa_data <- data.table(pisa_data)

region <- read_xlsx("C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-analysis of living arrangement/Data/UN Regions.xlsx")
names(region) <- c("country","code","cnt","region","region2")
region <- region[,c("cnt","region")]

pisa_data <- merge(pisa_data,region,by="cnt",all.x=TRUE)

pisa_data$sex <- factor(pisa_data$sex,
                    levels=c(1,2),
                    labels=c("Female","Male"))

svydes_1 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Australia and New Zealand")], strata = ~stratum)
svydes_2 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Central Asia")], strata = ~stratum)
svydes_3 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Eastern Asia")], strata = ~stratum)
svydes_4 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="South-eastern Asia")], strata = ~stratum)
svydes_5 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Western Asia")], strata = ~stratum)
svydes_6 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Eastern Europe")], strata = ~stratum)
svydes_7 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Northern Europe")], strata = ~stratum)
svydes_8 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Southern Europe")], strata = ~stratum)
svydes_9 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Western Europe")], strata = ~stratum)
svydes_10 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Latin America/Caribbean")], strata = ~stratum)
svydes_11 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="North America")], strata = ~stratum)
svydes_12 <- svydesign(ids = ~1, weights=~w_fstuwt, data=pisa_data[which(pisa_data$region=="Northern Africa")], strata = ~stratum)

options(survey.lonely.psu="remove")
svymeans <- rbind(svyby(~lonely_b, ~ cnt + year, svydes_1, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_2, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_3, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_4, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_5, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_6, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_7, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_8, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_9, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_10, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_11, svymean, na.rm=TRUE),
                  svyby(~lonely_b, ~ cnt + year, svydes_12, svymean, na.rm=TRUE))

svymeans_sex <- rbind(svyby(~lonely_b, ~ cnt + year + sex, svydes_1, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_2, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_3, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_4, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_5, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_6, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_7, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_8, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_9, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_10, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_11, svymean, na.rm=TRUE),
                      svyby(~lonely_b, ~ cnt + year + sex, svydes_12, svymean, na.rm=TRUE))

svymeans$sex <- "Overall"
svymeans <- rbind(svymeans,svymeans_sex)
write_csv(svymeans,paste0(workdir,"Results/country means.csv"))

reg_means <- rbind(cbind(reg=0,svyby(~lonely_b, ~ year, svydes_1, svymean, na.rm=TRUE)),
                   cbind(reg=1,svyby(~lonely_b, ~ year, svydes_2, svymean, na.rm=TRUE)),
                   cbind(reg=2,svyby(~lonely_b, ~ year, svydes_3, svymean, na.rm=TRUE)),
                   cbind(reg=3,svyby(~lonely_b, ~ year, svydes_4, svymean, na.rm=TRUE)),
                   cbind(reg=4,svyby(~lonely_b, ~ year, svydes_5, svymean, na.rm=TRUE)),
                   cbind(reg=5,svyby(~lonely_b, ~ year, svydes_6, svymean, na.rm=TRUE)),
                   cbind(reg=6,svyby(~lonely_b, ~ year, svydes_7, svymean, na.rm=TRUE)),
                   cbind(reg=7,svyby(~lonely_b, ~ year, svydes_8, svymean, na.rm=TRUE)),
                   cbind(reg=8,svyby(~lonely_b, ~ year, svydes_9, svymean, na.rm=TRUE)),
                   cbind(reg=9,svyby(~lonely_b, ~ year, svydes_10, svymean, na.rm=TRUE)),
                   cbind(reg=10,svyby(~lonely_b, ~ year, svydes_11, svymean, na.rm=TRUE)),
                   cbind(reg=11,svyby(~lonely_b, ~ year, svydes_12, svymean, na.rm=TRUE)))

reg_means_sex <- rbind(cbind(reg=0,svyby(~lonely_b, ~ year + sex, svydes_1, svymean, na.rm=TRUE)),
                       cbind(reg=1,svyby(~lonely_b, ~ year + sex, svydes_2, svymean, na.rm=TRUE)),
                       cbind(reg=2,svyby(~lonely_b, ~ year + sex, svydes_3, svymean, na.rm=TRUE)),
                       cbind(reg=3,svyby(~lonely_b, ~ year + sex, svydes_4, svymean, na.rm=TRUE)),
                       cbind(reg=4,svyby(~lonely_b, ~ year + sex, svydes_5, svymean, na.rm=TRUE)),
                       cbind(reg=5,svyby(~lonely_b, ~ year + sex, svydes_6, svymean, na.rm=TRUE)),
                       cbind(reg=6,svyby(~lonely_b, ~ year + sex, svydes_7, svymean, na.rm=TRUE)),
                       cbind(reg=7,svyby(~lonely_b, ~ year + sex, svydes_8, svymean, na.rm=TRUE)),
                       cbind(reg=8,svyby(~lonely_b, ~ year + sex, svydes_9, svymean, na.rm=TRUE)),
                       cbind(reg=9,svyby(~lonely_b, ~ year + sex, svydes_10, svymean, na.rm=TRUE)),
                       cbind(reg=10,svyby(~lonely_b, ~ year + sex, svydes_11, svymean, na.rm=TRUE)),
                       cbind(reg=11,svyby(~lonely_b, ~ year + sex, svydes_12, svymean, na.rm=TRUE)))

reg_means$sex <- "Overall"
reg_means_sum <- rbind(reg_means,reg_means_sex)
reg_means_sum$reg <- factor(reg_means_sum$reg,
                            labels=c("Australia and New Zealand","Central Asia","Eastern Asia","South-eastern Asia","Western Asia",
                                     "Eastern Europe","Northern Europe","Southern Europe","Western Europe","Latin America/Caribbean",
                                     "North America","Northern Africa"))
write_csv(reg_means_sum,paste0(workdir,"Results/regional means.csv"))

trend_sig <- rbind(c(reg=0,get_sig(svydes_1)),
                   c(reg=1,get_sig(svydes_2)),
                   c(reg=2,get_sig(svydes_3)),
                   c(reg=3,get_sig(svydes_4)),
                   c(reg=4,get_sig(svydes_5)),
                   c(reg=5,get_sig(svydes_6)),
                   c(reg=6,get_sig(svydes_7)),
                   c(reg=7,get_sig(svydes_8)),
                   c(reg=8,get_sig(svydes_9)),
                   c(reg=9,get_sig(svydes_10)),
                   c(reg=10,get_sig(svydes_11)),
                   c(reg=11,get_sig(svydes_12)))

trend <- rbind(cbind(reg=0,get_trend(svydes_1)),
               cbind(reg=1,get_trend(svydes_2)),
               cbind(reg=2,get_trend(svydes_3)),
               cbind(reg=3,get_trend(svydes_4)),
               cbind(reg=4,get_trend(svydes_5)),
               cbind(reg=5,get_trend(svydes_6)),
               cbind(reg=6,get_trend(svydes_7)),
               cbind(reg=7,get_trend(svydes_8)),
               cbind(reg=8,get_trend(svydes_9)),
               cbind(reg=9,get_trend(svydes_10)),
               cbind(reg=10,get_trend(svydes_11)),
               cbind(reg=11,get_trend(svydes_12)))

trend_sex <- rbind(cbind(reg=0,get_sex_trend(svydes_1)),
                   cbind(reg=1,get_sex_trend(svydes_2)),
                   cbind(reg=2,get_sex_trend(svydes_3)),
                   cbind(reg=3,get_sex_trend(svydes_4)),
                   cbind(reg=4,get_sex_trend(svydes_5)),
                   cbind(reg=5,get_sex_trend(svydes_6)),
                   cbind(reg=6,get_sex_trend(svydes_7)),
                   cbind(reg=7,get_sex_trend(svydes_8)),
                   cbind(reg=8,get_sex_trend(svydes_9)),
                   cbind(reg=9,get_sex_trend(svydes_10)),
                   cbind(reg=10,get_sex_trend(svydes_11)),
                   cbind(reg=11,get_sex_trend(svydes_12)))

svymeans <- merge(svymeans,region,by="cnt",all.x=TRUE)
reg_means$reg <- factor(reg_means$reg,
                        labels=c("Australia and New Zealand","Central Asia","Eastern Asia","South-eastern Asia","Western Asia",
                                 "Eastern Europe","Northern Europe","Southern Europe","Western Europe","Latin America/Caribbean",
                                 "North America","Northern Africa"))

svymeans$lonely_b <- ifelse(svymeans$lonely_b==0,NA,svymeans$lonely_b)
svymeans$se <- ifelse(svymeans$se==0,NA,svymeans$se)

trend <- rbind(trend,trend_sex)
trend$reg <- factor(trend$reg,
                        labels=c("Australia and New Zealand","Central Asia","Eastern Asia","South-eastern Asia","Western Asia",
                                 "Eastern Europe","Northern Europe","Southern Europe","Western Europe","Latin America/Caribbean",
                                 "North America","Northern Africa"))
trend$reg <- fct_relevel(trend$reg, "Central Asia","Eastern Asia","South-eastern Asia","Western Asia",
                         "Eastern Europe","Northern Europe","Southern Europe","Western Europe",
                         "Latin America/Caribbean","North America","Northern Africa","Australia and New Zealand")
trend$sex <- factor(trend$sex,
                    levels=c("Overall","Female","Male"),
                    labels=c("Overall","Female","Male"))

trend$reg2 <- trend$reg %>%
  recode_factor(
    `Australia and New Zealand` = "Oceania",
    `Central Asia` = "Asia",
    `Eastern Asia` = "Asia",
    `South-eastern Asia` = "Asia",
    `Western Asia` = "Asia",
    `Eastern Europe` = "Europe",
    `Northern Europe` = "Europe",
    `Southern Europe` = "Europe",
    `Western Europe` = "Europe",
    `Latin America/Caribbean` = "Americas",
    `North America` = "Americas",
    `Northern Africa` = "Africa"
  )

PISA_country_figure <- ggplot(data=svymeans,
                  aes(x=year, y=lonely_b, colour=region, label=cnt, fill=region, group=cnt)) +
  geom_point(shape = 21, alpha=0.4) +
  geom_line() +
  geom_text(vjust = 0.4, size=2, alpha=1, show.legend  = FALSE) +
  guides(colour=guide_legend(override.aes=list(shape=15,size=4))) + 
  scale_x_continuous(limits=c(2015,2022), breaks=seq(2015,2022, by = 1), expand = c(0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.4), breaks=seq(0,0.4, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  theme_classic(base_size = 9) +
  theme(legend.position="bottom",
        legend.title=element_blank())
PISA_country_figure

PISA_region_figure <- ggplot(data=reg_means,
                              aes(x=year, y=lonely_b, colour=reg, fill=reg, group=reg)) +
  geom_line() +
  geom_ribbon(aes(ymin=lonely_b-qnorm(0.975)*se,ymax=lonely_b+qnorm(0.975)*se),alpha=0.3,colour=NA) +
  scale_x_continuous(limits=c(2015,2022), breaks=seq(2015,2022, by = 1), expand = c(0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.35), breaks=seq(0,0.35, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  theme_classic(base_size = 9) +
  theme(legend.position="bottom",
        legend.title=element_blank())
PISA_region_figure

PISA_region_trend <- ggplot(data=trend,
                             aes(x=year, y=p, colour=sex, fill=sex, group=sex, linetype=sex)) +
  geom_line() +
  geom_ribbon(aes(ymin=p-qnorm(0.975)*se,ymax=p+qnorm(0.975)*se),alpha=0.3,colour=NA) +
  scale_x_continuous(limits=c(2015,2022), breaks=seq(2015,2022, by = 2), expand = c(0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.35), breaks=seq(0,0.35, by = 0.05), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  theme_classic(base_size = 7) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01))+
  facet_wrap(~reg,
             labeller=labeller(reg=c(`Central Asia` = "(a) Central Asia",
                                     `Eastern Asia` = "(b) Eastern Asia",
                                     `South-eastern Asia` = "(c) South-eastern Asia",
                                     `Western Asia` = "(d) Western Asia",
                                     `Eastern Europe` = "(e) Eastern Europe",
                                     `Northern Europe` = "(f) Northern Europe",
                                     `Southern Europe` = "(g) Southern Europe",
                                     `Western Europe` = "(h) Western Europe",
                                     `Latin America/Caribbean` = "(i) Latin America/Caribbean",
                                     `North America` = "(j) North America",
                                     `Northern Africa` = "(k) Northern Africa",
                                     `Australia and New Zealand` = "(l) Australia and New Zealand"))) +
  scale_color_lancet() + scale_fill_lancet()
PISA_region_trend


ggsave(paste0(workdir,"Results/PISA_by_country.jpg"),
       PISA_country_figure,
       width=15,
       height=10,
       units="cm")

ggsave(paste0(workdir,"Results/PISA_by_region.jpg"),
       PISA_region_figure,
       width=15,
       height=10,
       units="cm")

ggsave(paste0(workdir,"Results/PISA_regional_trends_by_sex.jpg"),
       PISA_region_trend,
       width=15,
       height=10,
       units="cm")