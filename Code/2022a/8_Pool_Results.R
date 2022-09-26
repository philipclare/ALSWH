######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Extract results from LTMLE MSM fits and pool using Rubin's rules
## Date: 16 September 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/Physical activity trajectories/"

libs <- c("ltmle","parallel","threadr","Amelia","ggplot2")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2. Load model fit objects 
#-----------------------------------------------------------------------------

fit_list <- list(pcsa_fit = read_rdata(file=paste0(workdir,"Results/results-pcsa9.RData")),
                 mcsa_fit = read_rdata(file=paste0(workdir,"Results/results-mcsa9.RData")),
                 pf_fit = read_rdata(file=paste0(workdir,"Results/results-pf9.RData")),
                 rp_fit = read_rdata(file=paste0(workdir,"Results/results-rp9.RData")),
                 bp_fit = read_rdata(file=paste0(workdir,"Results/results-bp9.RData")),
                 gh_fit = read_rdata(file=paste0(workdir,"Results/results-gh9.RData")),
                 vt_fit = read_rdata(file=paste0(workdir,"Results/results-vt9.RData")),
                 sf_fit = read_rdata(file=paste0(workdir,"Results/results-sf9.RData")),
                 re_fit = read_rdata(file=paste0(workdir,"Results/results-re9.RData")),
                 mh_fit = read_rdata(file=paste0(workdir,"Results/results-mh9.RData")))

##############################################################################
# 3. Extract means and SEs from model fits
#-----------------------------------------------------------------------------

res_all <- lapply(fit_list, function (y) {

  res <- lapply(y,function (x) {

    res <- c(c(plogis(summary(x)$cmat[1,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1]),
               plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])))
    
    cov.mat <- var(x$IC)
    
    gradient <- list(c(res[1]*(1-res[1]), 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[2]*(1-res[2]), res[2]*(1-res[2]), 0, 0, 0, 0, 0, 0, 0, 0),
                     c(res[3]*(1-res[3]), 0, res[3]*(1-res[3]), 0, 0, 0, 0, 0, 0, 0),
                     c(res[4]*(1-res[4]), 0, 0, res[4]*(1-res[4]), 0, 0, 0, 0, 0, 0),
                     c(res[5]*(1-res[5]), 0, 0, 0, res[5]*(1-res[5]), 0, 0, 0, 0, 0),
                     c(res[6]*(1-res[6]), 0, 0, 0, 0, res[6]*(1-res[6]), 0, 0, 0, 0),
                     c(res[7]*(1-res[7]), 0, 0, 0, 0, 0, res[7]*(1-res[7]), 0, 0, 0),
                     c(res[8]*(1-res[8]), 0, 0, 0, 0, 0, 0, res[8]*(1-res[8]), 0, 0),
                     c(res[9]*(1-res[9]), 0, 0, 0, 0, 0, 0, 0, res[9]*(1-res[9]), 0),
                     c(res[10]*(1-res[10]), 0, 0, 0, 0, 0, 0, 0, 0, res[10]*(1-res[10])))
    
    v <- do.call(rbind,lapply(gradient, function(x) {
      v <- t(x) %*% cov.mat %*% x
      std.dev <- sqrt(v[1, 1] / dim(ltmle_data)[1])*100
    }))
    
    coef <- c(plogis(summary(x)$cmat[1,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[2,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[3,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[4,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[5,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[6,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[7,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[8,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[9,1])*100,
              plogis(summary(x)$cmat[1,1]+summary(x)$cmat[10,1])*100)
    
    fin_res <- matrix(cbind(coef,v),nrow=10)
    
  })
  
})

##############################################################################
# 4. Pool MI results using Rubin's rules
#-----------------------------------------------------------------------------

mi_res <- do.call(cbind,lapply(res_all, function (y) {

  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
}))

colnames(mi_res) <- c("pcsa9_mean","pcsa9_se","mcsa9_mean","mcsa9_se",
                      "pf9_mean","pf9_se","rp9_mean","rp9_se",
                      "bp9_mean","bp9_se","gh9_mean","gh9_se",
                      "vt9_mean","vt9_se","sf9_mean","sf9_se",
                      "re9_mean","re9_se","mh9_mean","mh9_se")

mi_res_sust <- as.data.frame(mi_res[1:6,])
mi_res_init <- as.data.frame(mi_res[c(6:10,1),])

mi_res_sust$result <- c("stop_45","stop_50","stop_55","stop_60","stop_65","stop_70")
mi_res_init$result <- c("start_45","start_50","start_55","start_60","start_65","start_70")

##############################################################################
# 5. Plot results
#-----------------------------------------------------------------------------

pd <- position_dodge(0.1)

figure_theme <- theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", size = 0.2),
        text = element_text(size = 5),
        axis.line = element_line(colour = 'grey80', size = 0.2),
        axis.ticks = element_line(colour = "grey80", size = 0.2))

pcsa_sust_fig <- ggplot(mi_res_sust, aes(x=result, y=pcsa9_mean)) + 
  geom_errorbar(aes(ymin=pcsa9_mean-(1.96*pcsa9_se), ymax=pcsa9_mean+(1.96*pcsa9_se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age stopped meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) +                        # Expand y range
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +         # Set tick every 4
  scale_x_discrete(labels=c("45","50","55","60","65","70")) +
  figure_theme


ggsave(paste0(workdir,"Results/pcsa-sustained.tiff"),
       plot = pcsa_sust_fig,
       width = 1000,
       height = 500,
       units = "px")

pcsa_init_fig <- ggplot(mi_res_init, aes(x=result, y=pcsa9_mean)) + 
  geom_errorbar(aes(ymin=pcsa9_mean-(1.96*pcsa9_se), ymax=pcsa9_mean+(1.96*pcsa9_se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age started meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) +                        # Expand y range
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +         # Set tick every 4
  scale_x_discrete(labels=c("45","50","55","60","65","70")) +
  figure_theme

ggsave(paste0(workdir,"Results/pcsa-initiated.tiff"),
       plot = pcsa_init_fig,
       width = 1000,
       height = 500,
       units = "px")

mcsa_sust_fig <- ggplot(mi_res_sust, aes(x=result, y=mcsa9_mean)) + 
  geom_errorbar(aes(ymin=mcsa9_mean-(1.96*mcsa9_se), ymax=mcsa9_mean+(1.96*mcsa9_se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age stopped meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) +                        # Expand y range
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +         # Set tick every 4
  scale_x_discrete(labels=c("45","50","55","60","65","70")) +
  figure_theme


ggsave(paste0(workdir,"Results/mcsa-sustained.tiff"),
       plot = mcsa_sust_fig,
       width = 1000,
       height = 500,
       units = "px")

mcsa_init_fig <- ggplot(mi_res_init, aes(x=result, y=mcsa9_mean)) + 
  geom_errorbar(aes(ymin=mcsa9_mean-(1.96*mcsa9_se), ymax=mcsa9_mean+(1.96*mcsa9_se)), width=.1, position=pd, color="darkred", size=0.2) +
  geom_line(position=pd) +
  geom_point(position=pd, size=0.5, shape=20, color="darkred") +
  xlab("Age started meeting guidelines") +
  ylab("Expected mean") +
  expand_limits(y=25:75) +                        # Expand y range
  scale_y_continuous(breaks=seq(25, 75, by = 5), expand = c(0, 0)) +         # Set tick every 4
  scale_x_discrete(labels=c("45","50","55","60","65","70")) +
  figure_theme

ggsave(paste0(workdir,"Results/mcsa-initiated.tiff"),
       plot = mcsa_init_fig,
       width = 1000,
       height = 500,
       units = "px")
    