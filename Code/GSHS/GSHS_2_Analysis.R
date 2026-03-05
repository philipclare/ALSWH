

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("dplyr","expss","ggplot2","ggsci","ggthemes","haven","lme4","openxlsx","readxl","survey","texreg","tidyverse")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

workdir <- "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-GSHS/"

ldf <- readRDS(paste0(workdir,"Analysis data.rds"))
ldf <- ldf[which(ldf$region2!="Europe"),]

region <- read_xlsx("C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-analysis of living arrangement/Data/UN Regions.xlsx")
names(region) <- c("country","code","abbr","region","region2")

set <- svydesign(ids=~1, weights=~weight, data=ldf, strata=~stratum_new)

means <- svyby(~lonely_b, ~country*year, set, svymean)
means <- merge(means,region,by="country",all.x=TRUE)

means$year <- as.numeric(levels(means$year))[means$year]
means$sex <- 0

means_sex <- svyby(~lonely_b, ~country*year*sex, set, svymean)
means_sex <- merge(means_sex,region,by="country",all.x=TRUE)

means_sex$year <- as.numeric(levels(means_sex$year))[means_sex$year]

means_sex <- rbind(means,means_sex)
means_sex$sex <- factor(means_sex$sex,
                        levels=c(0,2,1),
                        labels=c("Overall","Female","Male"))

wb <- createWorkbook("Country means")
addWorksheet(wb, sheetName = "Means")
writeData(wb, sheet = "Means", x = means_sex)
saveWorkbook(wb, file = paste0(workdir,"Country means data.xlsx"), overwrite = TRUE)

ldf$year <- as.numeric(ldf$year)
ldf$year <- ifelse(ldf$year==18,19,ldf$year)

model_set <- svydesign(ids=~1, weights=~weight, data=ldf, strata=~stratum_new)

trendline <- svyglm(lonely_b ~ year,
                     model_set,
                    family="poisson")
trendline2 <- svyglm(lonely_b ~ year + I(year^2) + I(year^3),
                    model_set,
                    family="poisson")
trendline_sex <- svyglm(lonely_b ~ year*sex,
                    model_set,
                    family="poisson")
trendline_sex2 <- svyglm(lonely_b ~ year*sex + I(year^2)*sex + I(year^3)*sex,
                     model_set,
                     family="poisson")
trendline_region2 <- svyglm(lonely_b ~ year*region2 + I(year^2)*region2 + I(year^3)*region2,
                               model_set,
                               family="poisson")
trendline_sex_region2 <- svyglm(lonely_b ~ year*sex*region2 + I(year^2)*sex*region2 + I(year^3)*sex*region2,
                         model_set,
                         family="poisson")

summary(trendline_region2)
summary(trendline_sex_region2)

overall <- c(regTermTest(trendline_region2, ~year + I(year^2) + I(year^3))$p,
             regTermTest(trendline_region2, ~year + year:region2Americas + I(year^2) + region2Americas:I(year^2) + I(year^3) + region2Americas:I(year^3))$p,
             regTermTest(trendline_region2, ~year + year:region2Asia + I(year^2) + region2Asia:I(year^2) + I(year^3) + region2Asia:I(year^3))$p,
             regTermTest(trendline_region2, ~year + year:region2Oceania + I(year^2) + region2Oceania:I(year^2) + I(year^3) + region2Oceania:I(year^3))$p)
female <- c(regTermTest(trendline_region2, ~year + I(year^2) + I(year^3))$p,
            regTermTest(trendline_region2, ~year + year:region2Americas + I(year^2) + region2Americas:I(year^2) + I(year^3) + region2Americas:I(year^3))$p,
            regTermTest(trendline_region2, ~year + year:region2Asia + I(year^2) + region2Asia:I(year^2) + I(year^3) + region2Asia:I(year^3))$p,
            regTermTest(trendline_region2, ~year + year:region2Oceania + I(year^2) + region2Oceania:I(year^2) + I(year^3) + region2Oceania:I(year^3))$p)
male <- c(regTermTest(trendline_region2, ~year + year:sex + I(year^2) + sex:I(year^2) + I(year^3) + sex:I(year^3))$p,
          regTermTest(trendline_region2, ~year + year:sex + year:region2Americas + year:sex:region2Americas + I(year^2) + sex:I(year^2) + region2Americas:I(year^2) + sex:region2Americas:I(year^2) + I(year^3) + sex:I(year^3) + region2Americas:I(year^3) + sex:region2Americas:I(year^3))$p,
          regTermTest(trendline_region2, ~year + year:sex + year:region2Asia + year:sex:region2Asia + I(year^2) + sex:I(year^2) + region2Asia:I(year^2) + sex:region2Asia:I(year^2) + I(year^3) + sex:I(year^3) + region2Asia:I(year^3) + sex:region2Asia:I(year^3))$p,
          regTermTest(trendline_region2, ~year + year:sex + year:region2Oceania + year:sex:region2Oceania + I(year^2) + sex:I(year^2) + region2Oceania:I(year^2) + sex:region2Oceania:I(year^2) + I(year^3) + sex:I(year^3) + region2Oceania:I(year^3) + sex:region2Oceania:I(year^3))$p)

ldf_sub <- ldf[which(ldf$region=="Sub-Saharan Africa" | ldf$region=="Latin America/Caribbean" | ldf$region=="South-eastern Asia" | 
                       ldf$region=="Southern Asia" | ldf$region=="Western Asia" | ldf$region=="Pacific Islands"),]

region_model_set <- svydesign(ids=~1, weights=~weight, data=ldf_sub, strata=~stratum_new)

trendline_region <- svyglm(lonely_b ~ year*region + I(year^2)*region + I(year^3)*region,
                           region_model_set,
                           family="poisson")
trendline_sex_region <- svyglm(lonely_b ~ year*sex*region + I(year^2)*sex*region + I(year^3)*sex*region,
                               region_model_set,
                               family="poisson")

summary(trendline_region)
summary(trendline_sex_region)

overall <- c(regTermTest(trendline_region, ~year + I(year^2) + I(year^3))$p,
             regTermTest(trendline_region, ~year + `year:regionPacific Islands` + I(year^2) + `regionPacific Islands:I(year^2)` + I(year^3) + `regionPacific Islands:I(year^3)`)$p,
             regTermTest(trendline_region, ~year + `year:regionSouth-eastern Asia` + I(year^2) + `regionSouth-eastern Asia:I(year^2)` + I(year^3) + `regionSouth-eastern Asia:I(year^3)`)$p,
             regTermTest(trendline_region, ~year + `year:regionSouthern Asia` + I(year^2) + `regionSouthern Asia:I(year^2) + I(year^3)` + `regionSouthern Asia:I(year^3)`)$p,
             regTermTest(trendline_region, ~year + `year:regionSub-Saharan Africa` + I(year^2) + `regionSub-Saharan Africa:I(year^2)` + I(year^3) + `regionSub-Saharan Africa:I(year^3)`)$p,
             regTermTest(trendline_region, ~year + `year:regionWestern Asia` + I(year^2) + `regionWestern Asia:I(year^2) + I(year^3)` + `regionWestern Asia:I(year^3)`)$p)
female <- c(regTermTest(trendline_region, ~year + I(year^2) + I(year^3))$p,
            regTermTest(trendline_region, ~year + `year:regionPacific Islands` + I(year^2) + `regionPacific Islands:I(year^2)` + I(year^3) + `regionPacific Islands:I(year^3)`)$p,
            regTermTest(trendline_region, ~year + `year:regionSouth-eastern Asia` + I(year^2) + `regionSouth-eastern Asia:I(year^2)` + I(year^3) + `regionSouth-eastern Asia:I(year^3)`)$p,
            regTermTest(trendline_region, ~year + `year:regionSouthern Asia` + I(year^2) + `regionSouthern Asia:I(year^2) + I(year^3)` + `regionSouthern Asia:I(year^3)`)$p,
            regTermTest(trendline_region, ~year + `year:regionSub-Saharan Africa` + I(year^2) + `regionSub-Saharan Africa:I(year^2)` + I(year^3) + `regionSub-Saharan Africa:I(year^3)`)$p,
            regTermTest(trendline_region, ~year + `year:regionWestern Asia` + I(year^2) + `regionWestern Asia:I(year^2) + I(year^3)` + `regionWestern Asia:I(year^3)`)$p)
male <- c(regTermTest(trendline_region, ~year + year:sex + I(year^2) + sex:I(year^2) + I(year^3) + sex:I(year^3))$p,
          regTermTest(trendline_region, ~year + year:sex + `year:regionPacific Islands` + `year:sex:regionPacific Islands` + I(year^2) + sex:I(year^2) + `regionPacific Islands:I(year^2)` + `sex:regionPacific Islands:I(year^2)` + I(year^3) + sex:I(year^3) + `regionPacific Islands:I(year^3)` + `sex:regionPacific Islands:I(year^3)`)$p,
          regTermTest(trendline_region, ~year + year:sex + `year:regionSouth-eastern Asia` + `year:sex:regionSouth-eastern Asia` + I(year^2) + sex:I(year^2) + `regionSouth-eastern Asia:I(year^2)` + `sex:regionSouth-eastern Asia:I(year^2)` + I(year^3) + sex:I(year^3) + `regionSouth-eastern Asia:I(year^3)` + `sex:regionSouth-eastern Asia:I(year^3)`)$p,
          regTermTest(trendline_region, ~year + year:sex + `year:regionSouthern Asia` + `year:sex:regionSouthern Asia` + I(year^2) + sex:I(year^2) + `regionSouthern Asia:I(year^2)` + `sex:regionSouthern Asia:I(year^2)` + I(year^3) + sex:I(year^3) + `regionSouthern Asia:I(year^3)` + `sex:regionSouthern Asia:I(year^3)`)$p,
          regTermTest(trendline_region, ~year + year:sex + `year:regionSub-Saharan Africa` + `year:sex:regionSub-Saharan Africa` + I(year^2) + sex:I(year^2) + `regionSub-Saharan Africa:I(year^2)` + `sex:regionSub-Saharan Africa:I(year^2)` + I(year^3) + sex:I(year^3) + `regionSub-Saharan Africa:I(year^3)` + `sex:regionSub-Saharan Africa:I(year^3)`)$p,
          regTermTest(trendline_region, ~year + year:sex + `year:regionWestern Asia` + `year:sex:regionWestern Asia` + I(year^2) + sex:I(year^2) + `regionWestern Asia:I(year^2)` + `sex:regionWestern Asia:I(year^2)` + I(year^3) + sex:I(year^3) + `regionWestern Asia:I(year^3)` + `sex:regionWestern Asia:I(year^3)`)$p)


pvalues <- cbind(overall,female,male)

min_values_by_category <- ldf %>%
  group_by(region) %>%
  summarize(Min_Value = min(year, na.rm = TRUE)) # na.rm=TRUE removes NA values
max_values_by_category <- ldf %>%
  group_by(region) %>%
  summarize(Max_Value = max(year, na.rm = TRUE)) # na.rm=TRUE removes NA values

min_values_by_category <- ldf %>%
  group_by(region2) %>%
  summarize(Min_Value = min(year, na.rm = TRUE)) # na.rm=TRUE removes NA values
max_values_by_category <- ldf %>%
  group_by(region2) %>%
  summarize(Max_Value = max(year, na.rm = TRUE)) # na.rm=TRUE removes NA values

newdata <- data.frame(year=seq(1,17))
newdata_sex <- rbind(data.frame(sex=1,year=seq(1,17)),
                 data.frame(sex=2,year=seq(1,17)))
newdata_region <- rbind(data.frame(region="Sub-Saharan Africa",year=seq(1,15)),
                     data.frame(region="Latin America/Caribbean",year=seq(2,17)),
                     data.frame(region="South-eastern Asia",year=seq(1,19)),
                     data.frame(region="Southern Asia",year=seq(5,14)),
                     data.frame(region="Western Asia",year=seq(2,15)),
                     data.frame(region="Pacific Islands",year=seq(8,17)))
newdata_sexregion <- rbind(data.frame(region="Sub-Saharan Africa",sex=1,year=seq(1,15)),
                        data.frame(region="Latin America/Caribbean",sex=1,year=seq(2,17)),
                        data.frame(region="South-eastern Asia",sex=1,year=seq(1,19)),
                        data.frame(region="Southern Asia",sex=1,year=seq(5,14)),
                        data.frame(region="Western Asia",sex=1,year=seq(2,15)),
                        data.frame(region="Pacific Islands",sex=1,year=seq(8,17)),
                        data.frame(region="Sub-Saharan Africa",sex=2,year=seq(1,15)),
                        data.frame(region="Latin America/Caribbean",sex=2,year=seq(2,17)),
                        data.frame(region="South-eastern Asia",sex=2,year=seq(1,19)),
                        data.frame(region="Southern Asia",sex=2,year=seq(5,14)),
                        data.frame(region="Western Asia",sex=2,year=seq(2,15)),
                        data.frame(region="Pacific Islands",sex=2,year=seq(8,17)))

newdata_region2 <- rbind(data.frame(region2="Africa",year=seq(1,15)),
                        data.frame(region2="Americas",year=seq(2,17)),
                        data.frame(region2="Asia",year=seq(1,17)),
                        data.frame(region2="Oceania",year=seq(8,17)))
newdata_sexregion2 <- rbind(data.frame(region2="Africa",sex=1,year=seq(1,15)),
                           data.frame(region2="Americas",sex=1,year=seq(2,17)),
                           data.frame(region2="Asia",sex=1,year=seq(1,17)),
                           data.frame(region2="Oceania",sex=1,year=seq(8,17)),
                           data.frame(region2="Africa",sex=2,year=seq(1,15)),
                           data.frame(region2="Americas",sex=2,year=seq(2,17)),
                           data.frame(region2="Asia",sex=2,year=seq(1,17)),
                           data.frame(region2="Oceania",sex=2,year=seq(8,17)))

overall_trend <- data.frame(predict(trendline,
                                    newdata=newdata,
                                    type="response"))
overall_trend$year <- seq(1,17)
overall_trend$year <- ifelse(overall_trend$year==18,19,overall_trend$year)
overall_trend$year <- overall_trend$year+2002

overall_trend2 <- data.frame(predict(trendline2,
                                    newdata=newdata,
                                    type="response"))
overall_trend2$year <- seq(1,17)
overall_trend2$year <- ifelse(overall_trend2$year==18,19,overall_trend2$year)
overall_trend2$year <- overall_trend2$year+2002
overall_trend2$sex <- 0

overall_trend_sex <- cbind(newdata_sex,data.frame(predict(trendline_sex,
                                                      newdata=newdata_sex,
                                                      type="response")))
overall_trend_sex$year <- ifelse(overall_trend_sex$year==18,19,overall_trend_sex$year)
overall_trend_sex$year <- overall_trend_sex$year+2002

overall_trend_sex2 <- cbind(newdata_sex,data.frame(predict(trendline_sex2,
                                                       newdata=newdata_sex,
                                                       type="response")))
overall_trend_sex2$year <- ifelse(overall_trend_sex2$year==18,19,overall_trend_sex2$year)
overall_trend_sex2$year <- overall_trend_sex2$year+2002

overall_trend_sex2 <- rbind(overall_trend2,overall_trend_sex2)
overall_trend_sex2$sex <- factor(overall_trend_sex2$sex,
                                 levels=c(0,2,1),
                                 labels=c("Overall","Female","Male"))

overall_trend_region <- cbind(newdata_region,data.frame(predict(trendline_region,
                                                           newdata=newdata_region,
                                                           type="response")))
overall_trend_region$year <- ifelse(overall_trend_region$year==18,19,overall_trend_region$year)
overall_trend_region$year <- overall_trend_region$year+2002
overall_trend_region$sex <- 0
overall_trend_sexregion <- cbind(newdata_sexregion,data.frame(predict(trendline_sex_region,
                                                           newdata=newdata_sexregion,
                                                           type="response")))
overall_trend_sexregion$year <- ifelse(overall_trend_sexregion$year==18,19,overall_trend_sexregion$year)
overall_trend_sexregion$year <- overall_trend_sexregion$year+2002
overall_trend_sexregion <- rbind(overall_trend_region,overall_trend_sexregion)
overall_trend_sexregion$sex <- factor(overall_trend_sexregion$sex,
                                      levels=c(0,2,1),
                                      labels=c("Overall","Female","Male"))

overall_trend_region2 <- cbind(newdata_region2,data.frame(predict(trendline_region2,
                                                                 newdata=newdata_region2,
                                                                 type="response")))
overall_trend_region2$year <- ifelse(overall_trend_region2$year==18,19,overall_trend_region2$year)
overall_trend_region2$year <- overall_trend_region2$year+2002
overall_trend_region2$sex <- 0
overall_trend_sexregion2 <- cbind(newdata_sexregion2,data.frame(predict(trendline_sex_region2,
                                                                        newdata=newdata_sexregion2,
                                                                        type="response")))
overall_trend_sexregion2$year <- ifelse(overall_trend_sexregion2$year==18,19,overall_trend_sexregion2$year)
overall_trend_sexregion2$year <- overall_trend_sexregion2$year+2002
overall_trend_sexregion2 <- rbind(overall_trend_region2,overall_trend_sexregion2)
overall_trend_sexregion2$sex <- factor(overall_trend_sexregion2$sex,
                                       levels=c(0,2,1),
                                       labels=c("Overall","Female","Male"))

# figure1 <- ggplot() +
#   geom_point(data=means,
#              aes(x=year, y=lonely_b, colour=region2, label=abbr, fill=region2), size = 8, shape = 21, alpha=0.4) +
#   geom_text(data=means,
#             aes(x=year, y=lonely_b, colour=region2, label=abbr, fill=region2), vjust = 0.4, size=2, alpha=1, show_guide  = FALSE) +
#   geom_line(data=overall_trend, aes(x=year,y=response)) +
#   geom_ribbon(data=overall_trend, aes(x=year,ymin=response-qnorm(0.975)*SE,ymax=response+qnorm(0.975)*SE), alpha=0.1) +
#   guides(colour=guide_legend(override.aes=list(shape=15,size=4))) + 
#   scale_y_continuous(labels = scales::percent, limits=c(0,0.3), breaks=seq(0,0.3, by = 0.05), expand = c(0, 0)) +
#   scale_x_continuous(limits=c(2002,2020), breaks=seq(2003,2019, by = 4), expand = c(0, 0)) +
#   ylab("Loneliness %") + 
#   xlab("Year") +
#   theme_classic() +
#   theme(legend.key=element_blank(),
#         legend.key.size = unit(1,"line"),
#         legend.title=element_blank(),
#         legend.position="bottom") +
#   scale_color_lancet() + scale_fill_lancet()
# 
# figure1
# ggsave(paste0(workdir,"All countries.jpg"),
#        figure1,
#        width=15,
#        height=13,
#        units="cm")

figure1b <- ggplot() +
  geom_point(data=means,
             aes(x=year, y=lonely_b, colour=region2, label=abbr, fill=region2), size = 8, shape = 21, alpha=0.4) +
  geom_text(data=means,
            aes(x=year, y=lonely_b, colour=region2, label=abbr, fill=region2), vjust = 0.4, size=2, alpha=1, show.legend  = FALSE) +
  geom_line(data=overall_trend_sex2, aes(x=year,y=response, linetype=sex)) +
  geom_ribbon(data=overall_trend_sex2, aes(x=year, linetype=sex, ymin=response-qnorm(0.975)*SE,ymax=response+qnorm(0.975)*SE), alpha=0.1) +
  guides(colour=guide_legend(override.aes=list(shape=15,size=4))) + 
  scale_y_continuous(labels = scales::percent, limits=c(0,0.15), breaks=seq(0,0.15, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(limits=c(2002,2020), breaks=seq(2003,2019, by = 4), expand = c(0, 0)) +
  ylab("Loneliness %") + 
  xlab("Year") +
  theme_classic() +
  theme(legend.key=element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title=element_blank(),
        legend.position="bottom") +
  scale_color_lancet() + scale_fill_lancet()

figure1b

ggsave(paste0(workdir,"All countries with polynomial by sex 20260206.jpg"),
       figure1b,
       width=15,
       height=13,
       units="cm")

# means <- means %>% 
#   group_by(country) %>% 
#   mutate(obs = row_number())%>% 
#   mutate(max = max(obs))
# 
# means_map <- means
# 
# means <- means[which(means$max>1),]
# means <- means[,-c(9,10)]
# 
# figure2 <- ggplot(data=means,
#                   aes(x=year, y=lonely_b, colour=country, label=abbr, fill=country)) +
#   geom_point(size = 8, shape = 21, alpha=0.4) +
#   geom_line() +
#   geom_text(vjust = 0.4, size=2, alpha=1, show_guide  = FALSE) +
#   guides(colour=guide_legend(override.aes=list(shape=15,size=4))) + 
#   scale_y_continuous(labels = scales::percent, limits=c(0,0.3), breaks=seq(0,0.3, by = 0.05), expand = c(0, 0)) +
#   scale_x_continuous(limits=c(2002,2020), breaks=seq(2003,2019, by = 4), expand = c(0, 0)) +
#   ylab("Loneliness %") + 
#   xlab("Year") +
#   theme_classic() +
#   theme(legend.position="none") +
#   scale_color_lancet() + scale_fill_lancet()
# 
# figure2
# ggsave(paste0(workdir,"Change Figure.jpg"),
#        figure2,
#        width=15,
#        height=13,
#        units="cm")
# 
# figure3 <- ggplot(data=means,
#                   aes(x=year, y=lonely_b, colour=country, label=abbr, fill=country)) +
#   geom_ribbon(data=means,aes(ymin=lonely_b-qnorm(0.975)*se,ymax=lonely_b+qnorm(0.975)*se),alpha=0.1, colour = NA) +
#   geom_text(vjust = 0.4, size=2, show_guide  = FALSE) +
#   geom_line() +
#   guides(colour=guide_legend(override.aes=list(shape=15,size=4))) + 
#   scale_y_continuous(labels = scales::percent, limits=c(0,0.3), breaks=seq(0,0.3, by = 0.05), expand = c(0, 0)) +
#   scale_x_continuous(limits=c(2002,2020), breaks=seq(2003,2019, by = 4), expand = c(0, 0)) +
#   ylab("Loneliness %") + 
#   xlab("Year") +
#   theme_classic() +
#   theme(legend.position="none") +
#   scale_color_lancet() + scale_fill_lancet()
# 
# figure3
# ggsave(paste0(workdir,"Change Figure with CI ribbon.jpg"),
#        figure3,
#        width=15,
#        height=13,
#        units="cm")
# 
# figure4 <- ggplot(data=means,
#                   aes(x=year, y=lonely_b, colour=country, label=abbr, fill=country)) +
#   geom_errorbar(data=means,aes(ymin=lonely_b-qnorm(0.975)*se,ymax=lonely_b+qnorm(0.975)*se), width=.2) +
#   geom_text(vjust = 0.4, size=2, show_guide  = FALSE) +
#   geom_line() +
#   guides(colour=guide_legend(override.aes=list(shape=15,size=4))) + 
#   scale_y_continuous(labels = scales::percent, limits=c(0,0.3), breaks=seq(0,0.3, by = 0.05), expand = c(0, 0)) +
#   scale_x_continuous(limits=c(2002,2020), breaks=seq(2003,2019, by = 4), expand = c(0, 0)) +
#   ylab("Loneliness %") + 
#   xlab("Year") +
#   theme_classic() +
#   theme(legend.position="none") +
#   scale_color_lancet() + scale_fill_lancet()
# 
# figure4
# ggsave(paste0(workdir,"Change Figure with CI bars.jpg"),
#        figure4,
#        width=15,
#        height=13,
#        units="cm")
# means_map <- means_map[which(means_map$obs==means_map$max),]
# means_map$lonely_c <- cut(means_map$lonely_b, 
#                             breaks=c(-Inf, 0.08700625, 0.13035463 , Inf), 
#                             labels=c("Bottom","Middle","Top"))
# 
# map.data <- map_data(map="world")
# map.data <- map.data[which(map.data$region!="Antarctica"),]
# country.map <- merge(map.data,means_map[,c("country","lonely_b","lonely_c")],by.x="region",by.y="country",all.x=TRUE)
# country.map <- country.map[which(!is.na(country.map$lonely_b)),]
# 
# country.map <- country.map[order(country.map$group, country.map$order),]
# country.map <- apply_labels(country.map,
#                            lonely_c = "Loneliness tertile")
#                       
# map <- ggplot(data = map.data, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "white", color = "black", linewidth = 0.3) + 
#   geom_polygon(data = country.map[which(!is.na(country.map$lonely_b)),], aes(fill=lonely_b)) + 
#   scale_fill_gradient(limits=c(0, 0.25), low = "lightgreen", high = "maroon")
# map
# 
# map <- ggplot(data = map.data, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "white", color = "black", linewidth = 0.2) + 
#   geom_polygon(data = country.map[which(!is.na(country.map$lonely_b)),], aes(fill=lonely_c)) +
#   scale_fill_discrete(name = "Loneliness tertile") +
#   theme_void()
# map
# 
# world_map = map_data("world")
# world_map <- world_map[which(world_map$region!="Antarctica"),]
# 
# map.data = world_map %>% 
#   distinct(region) %>% 
#   rowid_to_column()
# map.data <- merge(map.data,means_map[,c("country","lonely_b")],by.x="region",by.y="country",all.x=TRUE)
# 
# grad_map <- map.data %>% 
#   ggplot(aes(fill = lonely_b, map_id = region)) +
#   geom_map(map = world_map) +
#   expand_limits(x = world_map$long, y = world_map$lat) +
#   theme_map() +
#   scale_fill_continuous(na.value="white",
#                         limits=c(0, 0.25), low = "grey90", high = "grey20",
#                         name = "Loneliness") + 
#   theme(panel.background = element_rect(colour="lightblue",fill = "lightblue"),
#         legend.box.background = element_blank(),
#         legend.background = element_rect(fill = "lightblue"))
# grad_map
# 
# ggsave(paste0(workdir,"Map.jpg"),
#        grad_map,
#        width=16.43664,
#        height=8,
#        units="cm")




##############################################################################

figure1b_sex_continent <- ggplot() +
  geom_point(data=means_sex,
             aes(x=year, y=lonely_b, colour=sex), size = 4, shape = 21, alpha=0.4, show.legend = FALSE) +
  geom_text(data=means_sex,
            aes(x=year, y=lonely_b, colour=sex, label=abbr), vjust = 0.4, size=1, alpha=1, show.legend  = FALSE) +
  geom_line(data=overall_trend_sexregion2, aes(x=year,y=response, colour=sex, linetype=sex)) +
  geom_ribbon(data=overall_trend_sexregion2, aes(x=year, fill=sex, ymin=response-qnorm(0.975)*SE,ymax=response+qnorm(0.975)*SE), alpha=0.1) +
  guides(colour=guide_legend(override.aes=list(shape=15,size=2))) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.25), breaks=seq(0,0.25, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(limits=c(2002,2020), breaks=seq(2003,2019, by = 4), expand = c(0, 0)) +
  ylab("Loneliness %") +
  xlab("Year") +
  theme_classic(base_size = 9) +
  theme(legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size=5),
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01)) +
  facet_wrap(~region2,
             labeller=labeller(region2=c(`Africa` = "(a) Africa",
                                         `Americas` = "(b) Americas",
                                         `Asia` = "(c) Asia",
                                         `Oceania` = "(d) Oceania"))) +
  scale_color_lancet() + scale_fill_lancet()

figure1b_sex_continent

ggsave(paste0(workdir,"Figure by continent and sex 20260206.jpg"),
       figure1b_sex_continent,
       width=15,
       height=13,
       units="cm")

figure1b_sex_region <- ggplot() +
  geom_point(data=means_sex[which(means_sex$region=="Sub-Saharan Africa" | means_sex$region=="Latin America/Caribbean" | means_sex$region=="South-eastern Asia" | 
                                    means_sex$region=="Southern Asia" | means_sex$region=="Western Asia" | means_sex$region=="Pacific Islands"),],
             aes(x=year, y=lonely_b, colour=sex), size = 4, shape = 21, alpha=0.4, show.legend = FALSE) +
  geom_text(data=means_sex[which(means_sex$region=="Sub-Saharan Africa" | means_sex$region=="Latin America/Caribbean" | means_sex$region=="South-eastern Asia" | 
                                   means_sex$region=="Southern Asia" | means_sex$region=="Western Asia" | means_sex$region=="Pacific Islands"),],
            aes(x=year, y=lonely_b, colour=sex, label=abbr), vjust = 0.4, size=1, alpha=1, show.legend  = FALSE) +
  geom_line(data=overall_trend_sexregion, aes(x=year,y=response, colour=sex, linetype=sex)) +
  geom_ribbon(data=overall_trend_sexregion, aes(x=year, fill=sex, ymin=response-qnorm(0.975)*SE,ymax=response+qnorm(0.975)*SE), alpha=0.1) +
  guides(colour=guide_legend(override.aes=list(shape=15,size=2))) +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.25), breaks=seq(0,0.25, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(limits=c(2002,2020), breaks=seq(2003,2019, by = 4), expand = c(0, 0)) +
  ylab("Loneliness %") +
  xlab("Year") +
  theme_classic(base_size = 9) +
  theme(legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size=5),
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01)) +
  facet_wrap(~region,
             ncol=2,
             labeller=labeller(region=c(`Latin America/Caribbean` = "(a) Latin America/Caribbean",
                                        `Pacific Islands` = "(b) Pacific Islands",
                                        `South-eastern Asia` = "(c) South-eastern Asia",
                                        `Southern Asia` = "(d) Southern Asia",
                                        `Sub-Saharan Africa` = "(e) Sub-Saharan Africa",
                                        `Western Asia` = "(f) Western Asia"))) +
  scale_color_lancet() + scale_fill_lancet()

figure1b_sex_region

ggsave(paste0(workdir,"Figure by region and sex 20260206.jpg"),
       figure1b_sex_region,
       width=15,
       height=13,
       units="cm")


           
####################################################################
## Country-specific linear trends

# Argentina
set_arg <- svydesign(ids=~1, weights=~weight, data=ldf[which(ldf$country=="Argentina"),], strata=~stratum_new)
arg_fit_l <- svyglm(design = set_arg,
                    formula = lonely_b ~ year,
                    family="poisson")
arg_fit_l_sex <- svyglm(design = set_arg,
                        formula = lonely_b ~ year*sex,
                        family="poisson")

# Uruguay
set_ury <- svydesign(ids=~1, weights=~weight, data=ldf[which(ldf$country=="Uruguay"),], strata=~stratum_new)
ury_fit_l <- svyglm(design = set_ury,
                    formula = lonely_b ~ year,
                    family="poisson")
ury_fit_l_sex <- svyglm(design = set_ury,
                        formula = lonely_b ~ year*sex,
                        family="poisson")

# Morocco
set_mar <- svydesign(ids=~1, weights=~weight, data=ldf[which(ldf$country=="Morocco"),], strata=~stratum_new)
mar_fit_l <- svyglm(design = set_mar,
                    formula = lonely_b ~ year,
                    family="poisson")
mar_fit_l_sex <- svyglm(design = set_mar,
                        formula = lonely_b ~ year*sex,
                        family="poisson")

# UAE
set_are <- svydesign(ids=~1, weights=~weight, data=ldf[which(ldf$country=="United Arab Emirates"),], strata=~stratum_new)
are_fit_l <- svyglm(design = set_are,
                    formula = lonely_b ~ year,
                    family="poisson")
are_fit_l_sex <- svyglm(design = set_are,
                        formula = lonely_b ~ year*sex,
                        family="poisson")

# Thailand
set_tha <- svydesign(ids=~1, weights=~weight, data=ldf[which(ldf$country=="Thailand"),], strata=~stratum_new)
tha_fit_l <- svyglm(design = set_tha,
                    formula = lonely_b ~ year,
                    family="poisson")
tha_fit_l_sex <- svyglm(design = set_tha,
                        formula = lonely_b ~ year*sex,
                        family="poisson")

# Philippines
set_phl <- svydesign(ids=~1, weights=~weight, data=ldf[which(ldf$country=="Philippines"),], strata=~stratum_new)
phl_fit_l <- svyglm(design = set_phl,
                    formula = lonely_b ~ year,
                    family="poisson")
phl_fit_l_sex <- svyglm(design = set_phl,
                        formula = lonely_b ~ year*sex,
                        family="poisson")

# Trinidad and Tobago
set_tto <- svydesign(ids=~1, weights=~weight, data=ldf[which(ldf$country=="Trinidad and Tobago"),], strata=~stratum_new)
tto_fit_l <- svyglm(design = set_tto,
                    formula = lonely_b ~ year,
                    family="poisson")
tto_fit_l_sex <- svyglm(design = set_tto,
                        formula = lonely_b ~ year*sex,
                        family="poisson")

2 * pt(-abs(coef(summary(tto_fit_l_sex))[2,1]/coef(summary(tto_fit_l_sex))[2,2]),summary(tto_fit_l_sex)$df[2])
2 * pt(-abs(coef(summary(tto_fit_l_sex))[3,1]/coef(summary(tto_fit_l_sex))[3,2]),summary(tto_fit_l_sex)$df[2])
2 * pt(-abs(coef(summary(tto_fit_l_sex))[4,1]/coef(summary(tto_fit_l_sex))[4,2]),summary(tto_fit_l_sex)$df[2])

overall <- data.frame(rbind(coef(summary(arg_fit_l))[2,1:2],
                            coef(summary(ury_fit_l))[2,1:2],
                            coef(summary(mar_fit_l))[2,1:2],
                            coef(summary(are_fit_l))[2,1:2],
                            coef(summary(tha_fit_l))[2,1:2],
                            coef(summary(phl_fit_l))[2,1:2],
                            coef(summary(tto_fit_l))[2,1:2]))

female <- data.frame(rbind(coef(summary(arg_fit_l_sex))[2,1:2],
                           coef(summary(ury_fit_l_sex))[2,1:2],
                           coef(summary(mar_fit_l_sex))[2,1:2],           
                           coef(summary(are_fit_l_sex))[2,1:2],            
                           coef(summary(tha_fit_l_sex))[2,1:2],
                           coef(summary(phl_fit_l_sex))[2,1:2],
                           coef(summary(tto_fit_l_sex))[2,1:2]))

male <- data.frame(rbind(print(svycontrast(arg_fit_l_sex,c("year"=1,"year:sex"=1))),
                         print(svycontrast(ury_fit_l_sex,c("year"=1,"year:sex"=1))),
                         print(svycontrast(mar_fit_l_sex,c("year"=1,"year:sex"=1))),
                         print(svycontrast(are_fit_l_sex,c("year"=1,"year:sex"=1))),
                         print(svycontrast(tha_fit_l_sex,c("year"=1,"year:sex"=1))),
                         print(svycontrast(phl_fit_l_sex,c("year"=1,"year:sex"=1))),
                         print(svycontrast(tto_fit_l_sex,c("year"=1,"year:sex"=1)))))

colnames(overall) <- colnames(female) <- colnames(male) <- c("est","se")

overall$t <- overall$est/overall$se
overall$df <- c(summary(arg_fit_l)$df[2],
                summary(ury_fit_l)$df[2],
                summary(mar_fit_l)$df[2],
                summary(are_fit_l)$df[2],
                summary(tha_fit_l)$df[2],
                summary(phl_fit_l)$df[2],
                summary(tto_fit_l)$df[2])
overall$p <- 2 * pt(-abs(overall$t),overall$df)

female$t <- female$est/female$se
female$df <- c(summary(arg_fit_l_sex)$df[2],
               summary(ury_fit_l_sex)$df[2],
               summary(mar_fit_l_sex)$df[2],
               summary(are_fit_l_sex)$df[2],
               summary(tha_fit_l_sex)$df[2],
               summary(phl_fit_l_sex)$df[2],
               summary(tto_fit_l_sex)$df[2])
female$p <- 2 * pt(-abs(female$t),female$df)

male$t <- male$est/male$se
male$df <- c(summary(arg_fit_l_sex)$df[2],
             summary(ury_fit_l_sex)$df[2],
             summary(mar_fit_l_sex)$df[2],
             summary(are_fit_l_sex)$df[2],
             summary(tha_fit_l_sex)$df[2],
             summary(phl_fit_l_sex)$df[2],
             summary(tto_fit_l_sex)$df[2])
male$p <- 2 * pt(-abs(male$t),male$df)


