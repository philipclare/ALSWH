######################################################################################
##   
## Project: Trends in loneliness and isolation in Australia
## Program: C2 - Missing Data.R
## Purpose: Create plots from results of LTMLE analyses
## Author: Philip Clare
## Date: 24 October 2023
## OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

# workdir <- "Y:/PRJ-hilda_data/Loneliness trends/"
workdir <- "Y:/PRJ-hilda_data/Loneliness trends/"

libs <- c("Amelia","ggplot2","tidyr","ggpubr","openxlsx","readxl","stringr")
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
mod_demog_1 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled_bydemog.xlsx"), sheet = "lonely")
mod_demog_1 <- separate(mod_demog_1, demog, c("dem", "level", "est"), "_")
mod_demog_1 <- mod_demog_1 %>%
  pivot_longer(c=4:26,names_to = c("year")) %>%
  pivot_wider(names_from = est,values_from = value) %>% 
  dplyr::rename(
    lonely_point_e = e,
    lonely_point_l = ll,
    lonely_point_u = ul)

mod_demog_2 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled_bydemog.xlsx"), sheet = "lonely_chronic")
mod_demog_2 <- separate(mod_demog_2, demog, c("dem", "level", "est"), "_")
mod_demog_2 <- mod_demog_2 %>%
  pivot_longer(c=4:25,names_to = c("year")) %>%
  pivot_wider(names_from = est,values_from = value) %>% 
  dplyr::rename(
    lonely_chronic_e = e,
    lonely_chronic_l = ll,
    lonely_chronic_u = ul)

mod_demog_3 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled_bydemog.xlsx"), sheet = "support")
mod_demog_3 <- separate(mod_demog_3, demog, c("dem", "level", "est"), "_")
mod_demog_3 <- mod_demog_3 %>%
  pivot_longer(c=4:26,names_to = c("year")) %>%
  pivot_wider(names_from = est,values_from = value) %>% 
  dplyr::rename(
    isolation_point_e = e,
    isolation_point_l = ll,
    isolation_point_u = ul)

mod_demog_4 <- read_xlsx(paste0(workdir,"Results/Raw output/modeled_bydemog.xlsx"), sheet = "support_chronic")
mod_demog_4 <- separate(mod_demog_4, demog, c("dem", "level", "est"), "_")
mod_demog_4 <- mod_demog_4 %>%
  pivot_longer(c=4:25,names_to = c("year")) %>%
  pivot_wider(names_from = est,values_from = value) %>% 
  dplyr::rename(
    isolation_chronic_e = e,
    isolation_chronic_l = ll,
    isolation_chronic_u = ul)

mod_bydemog <- merge(merge(merge(mod_demog_1,mod_demog_2,by=c("dem","level","year"), all=TRUE),
                           mod_demog_3,by=c("dem","level","year"), all=TRUE),
                     mod_demog_4,by=c("dem","level","year"), all=TRUE)

mod_bydemog_longer <- mod_bydemog %>%
  pivot_longer(c=4:15,
               names_to = c("outcome","type", ".value"),
               names_pattern = "(.+)_(.+)_(.+)"
  )

mod_bydemog_longer$outcome <- factor(mod_bydemog_longer$outcome,levels=c("lonely","isolation"),labels=c("Loneliness","Isolation"))
mod_bydemog_longer$year <- as.numeric(mod_bydemog_longer$year)

mod_age <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="age"),c(2:8)]
mod_age <- mod_age %>% dplyr::rename(age = level)
mod_age$age <- factor(mod_age$age,levels=c("16-29","30-64","65+"),labels=c("16-29","30-64","65+"))

mod_sex <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="sex"),c(2:8)]
mod_sex <- mod_sex %>% dplyr::rename(sex = level)
mod_sex$sex <- factor(mod_sex$sex,levels=c("male","female"),labels=c("Male","Female"))

mod_educ <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="educ"),c(2:8)]
mod_educ <- mod_educ %>% dplyr::rename(educ = level)
mod_educ$educ <- factor(mod_educ$educ,levels=c("hs","trade","uni"),labels=c("High school or less","Trade/diploma","University"))

mod_emp <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="emp"),c(2:8)]
mod_emp <- mod_emp %>% dplyr::rename(emp = level)
mod_emp$emp <- factor(mod_emp$emp,levels=c("employed","unempl","niw"),labels=c("Employed","Unemployed","Not in the workforce"))

mod_living <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="living"),c(2:8)]
mod_living <- mod_living %>% dplyr::rename(living = level)
mod_living$living <- factor(mod_living$living,levels=c("alone","others"),labels=c("Live alone","Live with others"))

mod_marital <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="marital"),c(2:8)]
mod_marital <- mod_marital %>% dplyr::rename(marital = level)
mod_marital$marital <- factor(mod_marital$marital,levels=c("part","single","widowed"),labels=c("Married/partnered","Single","Widowed"))

mod_cob <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="cob"),c(2:8)]
mod_cob <- mod_cob %>% dplyr::rename(cob = level)
mod_cob$cob <- factor(mod_cob$cob,levels=c("aus","eng","noneng"),labels=c("Australia","Other English speaking","Non-English speaking"))

mod_lang <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="lang"),c(2:8)]
mod_lang <- mod_lang %>% dplyr::rename(lang = level)
mod_lang$lang <- factor(mod_lang$lang,levels=c("eng","noneng"),labels=c("English","Language other than English"))

mod_seifa <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="seifa"),c(2:8)]
mod_seifa <- mod_seifa %>% dplyr::rename(seifa = level)
mod_seifa$seifa <- factor(mod_seifa$seifa,levels=c("bottom3","mid4","top3"),labels=c("Bottom three deciles","Middle four deciles","Top three deciles"))

mod_aria <- mod_bydemog_longer[which(mod_bydemog_longer$dem=="aria"),c(2:8)]
mod_aria <- mod_aria %>% dplyr::rename(aria = level)
mod_aria$aria <- factor(mod_aria$aria,levels=c("city","reg1","other"),labels=c("Metropolitan","Inner regional","Outer regional/remote"))

dem_data <- list(mod_age,mod_sex,mod_educ,mod_emp,mod_living,mod_marital,mod_cob,mod_lang,mod_seifa,mod_aria)
dem_list <- c("age","sex","educ","emp","living","marital","cob","lang","seifa","aria")
dem_label <- c("(a) Age category","(b) Sex",
               "(c) Education","(d) Employment status",
               "(e) Living arrangement","(f) Marital status",
               "(g) Country of birth","(h) First language",
               "(i) Area-level disadvantage (IRSD)","(j) Remoteness (ARIA+)")

######################################################################################
# 3. Define theme and common properties
#-------------------------------------------------------------------------------------

pd <- position_dodge(0.1)

# 3.1 Define theme
dem_theme <- theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
        text = element_text(size = 8),
        axis.line = element_line(colour = 'grey80', linewidth = 0.3),
        axis.ticks = element_line(colour = "grey80", linewidth = 0.3),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=6),
        plot.title = element_text(size=9),
        legend.key.size = unit(0.25, 'cm'))

# 3.2 Custom function to create each figure, allowing lapply
fig <- function(data,dem,title,type) {
  
  ymax <- ifelse(plyr::round_any(max(data$u,na.rm=TRUE),0.05,ceiling)>=0.5,
             plyr::round_any(max(data$u,na.rm=TRUE),0.1,ceiling),
             plyr::round_any(max(data$u,na.rm=TRUE),0.05,ceiling))
    
  incr <- ifelse(ymax>=0.5,0.1,0.05)
  
  xmin <- ifelse(type=="point",2001,2002)
  xbound <- xmin-1
  
  fig <- ggplot(data,
                aes(x=year, y=e, color=.data[[dem]])) +
    geom_ribbon(aes(ymin=l, ymax=u, fill = .data[[dem]]), position=pd, size=0.2, show.legend = F, alpha=0.2) +
    geom_line(aes(color=.data[[dem]])) +
    geom_point(size=2, shape=18, aes(color=.data[[dem]])) +
    xlab("Year") +
    expand_limits(y=c(0, ymax),x=c(xbound,2024)) +
    scale_y_continuous(breaks=seq(0, ymax, by = incr), expand = c(0, 0)) +
    scale_x_continuous(breaks=seq(xmin, 2023, by = 2), expand = c(0, 0)) +
    dem_theme +
    scale_color_manual(values=c("darkred", "darkgreen", "darkblue"),na.translate = F) + 
    scale_fill_manual(values=c("darkred", "darkgreen", "darkblue"),na.translate = F) + 
    ggtitle(title)
  
  fig
}

######################################################################################
# 4. Primary Analysis Plots
#-------------------------------------------------------------------------------------

# 4.1 Point loneliness
point_loneliness <- lapply(seq(1,10), function (x) {
  fig(as.data.frame(dem_data[[x]][which(mod_bydemog_longer$outcome=="Loneliness" & mod_bydemog_longer$type=="point"),]),dem_list[x],dem_label[x],"point") +
    ylab("Prevalence of loneliness")
})

point_loneliness_final <- ggarrange(point_loneliness[[1]],point_loneliness[[2]],
                                    point_loneliness[[3]],point_loneliness[[4]],
                                    point_loneliness[[5]],point_loneliness[[6]],
                                    point_loneliness[[7]],point_loneliness[[8]],
                                    point_loneliness[[9]],point_loneliness[[10]],ncol=2,nrow=5)
ggsave(paste0(workdir,"Results/F5 - point - loneliness by demog 20250515.tiff"),
       point_loneliness_final,
       width = 165,
       height = 247,
       units = "mm")

# 4.2 Point isolation
point_isolation <- lapply(seq(1,10), function (x) {
  fig(as.data.frame(dem_data[[x]][which(mod_bydemog_longer$outcome=="Isolation" & mod_bydemog_longer$type=="point"),]),dem_list[x],dem_label[x],"point") +
    ylab("Prevalence of isolation")
})

point_isolation_final <- ggarrange(point_isolation[[1]],point_isolation[[2]],
                                    point_isolation[[3]],point_isolation[[4]],
                                    point_isolation[[5]],point_isolation[[6]],
                                    point_isolation[[7]],point_isolation[[8]],
                                    point_isolation[[9]],point_isolation[[10]],ncol=2,nrow=5)
ggsave(paste0(workdir,"Results/F6 - point - isolation by demog 20250515.tiff"),
       point_isolation_final,
       width = 165,
       height = 247,
       units = "mm")

# 4.3 Chronic loneliness
chronic_loneliness <- lapply(seq(1,10), function (x) {
  fig(as.data.frame(dem_data[[x]][which(mod_bydemog_longer$outcome=="Loneliness" & mod_bydemog_longer$type=="chronic"),]),dem_list[x],dem_label[x],"chronic") +
    ylab("Prevalence of loneliness")
})

chronic_loneliness_final <- ggarrange(chronic_loneliness[[1]],chronic_loneliness[[2]],
                                    chronic_loneliness[[3]],chronic_loneliness[[4]],
                                    chronic_loneliness[[5]],chronic_loneliness[[6]],
                                    chronic_loneliness[[7]],chronic_loneliness[[8]],
                                    chronic_loneliness[[9]],chronic_loneliness[[10]],ncol=2,nrow=5)
ggsave(paste0(workdir,"Results/F7 - chronic - loneliness by demog 20250515.tiff"),
       chronic_loneliness_final,
       width = 165,
       height = 247,
       units = "mm")

# 4.4 Chronic isolation
chronic_isolation <- lapply(seq(1,10), function (x) {
  fig(as.data.frame(dem_data[[x]][which(mod_bydemog_longer$outcome=="Isolation" & mod_bydemog_longer$type=="chronic"),]),dem_list[x],dem_label[x],"chronic") +
    ylab("Prevalence of isolation")
})

chronic_isolation_final <- ggarrange(chronic_isolation[[1]],chronic_isolation[[2]],
                                   chronic_isolation[[3]],chronic_isolation[[4]],
                                   chronic_isolation[[5]],chronic_isolation[[6]],
                                   chronic_isolation[[7]],chronic_isolation[[8]],
                                   chronic_isolation[[9]],chronic_isolation[[10]],ncol=2,nrow=5)
ggsave(paste0(workdir,"Results/F8 - chronic - isolation by demog 20250515.tiff"),
       chronic_isolation_final,
       width = 165,
       height = 247,
       units = "mm")