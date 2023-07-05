######################################################################################
##   
## Effects of physical activity on health-related quality of life
## Extract difference results and calculate E-Values
## Date: 4 October 2022
## OSF Registration: https://osf.io/6zkcw
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "Y:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"
datadir <- "D:/Dropbox (Sydney Uni)/ALSWH/Model fit/"

libs <- c("Amelia","EValue")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

read_rdata <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"])
}

names <- c("pcsa_diff","pcsa_se","mcsa_diff","mcsa_se",
           "pf_diff","pf_se","rp_diff","rp_se",
           "bp_diff","bp_se","gh_diff","gh_se",
           "vt_diff","vt_se","sf_diff","sf_se",
           "re_diff","re_se","mh_diff","mh_se")

######################################################################################
# 2. Summarise mean difference
#-------------------------------------------------------------------------------------

# 2.1. Load model fit objects 
p_pcsa_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-pcsa9-1.RData")),
                    read_rdata(file=paste0(datadir,"primary-differences-pcsa9-2.RData")),
                    read_rdata(file=paste0(datadir,"primary-differences-pcsa9-3.RData")),
                    read_rdata(file=paste0(datadir,"primary-differences-pcsa9-4.RData")))
p_mcsa_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-mcsa9-1.RData")),
                    read_rdata(file=paste0(datadir,"primary-differences-mcsa9-2.RData")),
                    read_rdata(file=paste0(datadir,"primary-differences-mcsa9-3.RData")),
                    read_rdata(file=paste0(datadir,"primary-differences-mcsa9-4.RData")))
p_pf_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-pf9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-pf9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-pf9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-pf9-4.RData")))
p_rp_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-rp9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-rp9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-rp9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-rp9-4.RData")))
p_bp_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-bp9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-bp9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-bp9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-bp9-4.RData")))
p_gh_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-gh9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-gh9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-gh9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-gh9-4.RData")))
p_vt_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-vt9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-vt9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-vt9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-vt9-4.RData")))
p_sf_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-sf9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-sf9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-sf9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-sf9-4.RData")))
p_re_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-re9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-re9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-re9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-re9-4.RData")))
p_mh_res <- rbind(read_rdata(file=paste0(datadir,"primary-differences-mh9-1.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-mh9-2.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-mh9-3.RData")),
                  read_rdata(file=paste0(datadir,"primary-differences-mh9-4.RData")))

# 2.2. Combine into single list of results
primary_res <- list(p_pcsa_res,p_mcsa_res,
                    p_pf_res,p_rp_res,p_bp_res,p_gh_res,
                    p_vt_res,p_sf_res,p_re_res,p_mh_res)

# 2.3. Pool MI results using Rubin's rules
primary_diff_res <- do.call(cbind,lapply(primary_res, function (y) {
  
  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
}))

# 2.3 Assign column names and separate out sustained and inititiation results
colnames(primary_diff_res) <- names
primary_diff_sust <- as.data.frame(primary_diff_res[1:6,])
primary_diff_init <- as.data.frame(primary_diff_res[c(6:10,1),])
primary_diff_sust$result <- c("stop_50","stop_55","stop_60","stop_65","stop_70")
primary_diff_init$result <- c("start_45","start_50","start_55","start_60","start_65")

######################################################################################
# 3. Calculate E-Values
#-------------------------------------------------------------------------------------

evalues_sust <- do.call(rbind,lapply(seq(1,6), function (x) {
  
  c(evalues.MD(primary_diff_sust[x,1],primary_diff_sust[x,2])[2,1],
    evalues.MD(primary_diff_sust[x,3],primary_diff_sust[x,4])[2,1],
    evalues.MD(primary_diff_sust[x,5],primary_diff_sust[x,6])[2,1],
    evalues.MD(primary_diff_sust[x,7],primary_diff_sust[x,8])[2,1],
    evalues.MD(primary_diff_sust[x,9],primary_diff_sust[x,10])[2,1],
    evalues.MD(primary_diff_sust[x,11],primary_diff_sust[x,12])[2,1],
    evalues.MD(primary_diff_sust[x,13],primary_diff_sust[x,14])[2,1],
    evalues.MD(primary_diff_sust[x,15],primary_diff_sust[x,16])[2,1],
    evalues.MD(primary_diff_sust[x,17],primary_diff_sust[x,18])[2,1],
    evalues.MD(primary_diff_sust[x,19],primary_diff_sust[x,20])[2,1])
  
}))

evalues_init <- do.call(rbind,lapply(seq(1,6), function (x) {
  
  c(evalues.MD(primary_diff_init[x,1],primary_diff_init[x,2])[2,1],
    evalues.MD(primary_diff_init[x,3],primary_diff_init[x,4])[2,1],
    evalues.MD(primary_diff_init[x,5],primary_diff_init[x,6])[2,1],
    evalues.MD(primary_diff_init[x,7],primary_diff_init[x,8])[2,1],
    evalues.MD(primary_diff_init[x,9],primary_diff_init[x,10])[2,1],
    evalues.MD(primary_diff_init[x,11],primary_diff_init[x,12])[2,1],
    evalues.MD(primary_diff_init[x,13],primary_diff_init[x,14])[2,1],
    evalues.MD(primary_diff_init[x,15],primary_diff_init[x,16])[2,1],
    evalues.MD(primary_diff_init[x,17],primary_diff_init[x,18])[2,1],
    evalues.MD(primary_diff_init[x,19],primary_diff_init[x,20])[2,1])
  
}))

######################################################################################
# 4. Create data summary matrix
#-------------------------------------------------------------------------------------

sum_sust <- cbind(primary_diff_sust[,1:2],evalues_sust[,1],
                  primary_diff_sust[,3:4],evalues_sust[,2],
                  primary_diff_sust[,5:6],evalues_sust[,3],
                  primary_diff_sust[,7:8],evalues_sust[,4],
                  primary_diff_sust[,9:10],evalues_sust[,5],
                  primary_diff_sust[,11:12],evalues_sust[,6],
                  primary_diff_sust[,13:14],evalues_sust[,7],
                  primary_diff_sust[,15:16],evalues_sust[,8],
                  primary_diff_sust[,17:18],evalues_sust[,9],
                  primary_diff_sust[,19:20],evalues_sust[,10])

sum_init <- cbind(primary_diff_init[,1:2],evalues_init[,1],
                  primary_diff_init[,3:4],evalues_init[,2],
                  primary_diff_init[,5:6],evalues_init[,3],
                  primary_diff_init[,7:8],evalues_init[,4],
                  primary_diff_init[,9:10],evalues_init[,5],
                  primary_diff_init[,11:12],evalues_init[,6],
                  primary_diff_init[,13:14],evalues_init[,7],
                  primary_diff_init[,15:16],evalues_init[,8],
                  primary_diff_init[,17:18],evalues_init[,9],
                  primary_diff_init[,19:20],evalues_init[,10])
