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

# 1.1. Specify paths to Katana/windows PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/alswh/"
} else { # Manually defined for PC
  workdir <- "R:/PRJ-prc_alswh/Paper 1 - Health-related quality of life/"
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("Amelia","EValue","xlsx")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

names <- c("pcsa_diff","pcsa_se","mcsa_diff","mcsa_se",
           "gh_diff","gh_se","pf_diff","pf_se",
           "rp_diff","rp_se","bp_diff","bp_se",
           "vt_diff","vt_se","sf_diff","sf_se",
           "re_diff","re_se","mh_diff","mh_se")

######################################################################################
# 2. Load analysis data and get outcome means
#-------------------------------------------------------------------------------------

# 2.1. Load data
load(paste0(workdir,"Data/primary analysis data - wide form 20230605.RData"))
ltmle_data <- imp_primary
rm(imp_primary)

# 2.2. Get mean of each outcome in each imputation
outcomes <- c("pcsa9","mcsa9","gh9","pf9","rp9","bp9","vt9","sf9","re9","mh9")
sds <- do.call(rbind,lapply(ltmle_data, function (x,outcomes) {
  apply(x[,outcomes], 2, sd)
},outcomes=outcomes))

######################################################################################
# 2. Summarise mean difference
#-------------------------------------------------------------------------------------

# 2.1. Load model fit objects 
p_pcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pcsa9-1.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pcsa9-2.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pcsa9-3.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pcsa9-4.rds")))
p_mcsa_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mcsa9-1.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mcsa9-2.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mcsa9-3.rds")),
                    readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mcsa9-4.rds")))
p_pf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pf9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pf9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pf9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-pf9-4.rds")))
p_rp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-rp9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-rp9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-rp9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-rp9-4.rds")))
p_bp_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-bp9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-bp9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-bp9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-bp9-4.rds")))
p_gh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-gh9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-gh9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-gh9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-gh9-4.rds")))
p_vt_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-vt9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-vt9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-vt9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-vt9-4.rds")))
p_sf_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-sf9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-sf9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-sf9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-sf9-4.rds")))
p_re_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-re9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-re9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-re9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-re9-4.rds")))
p_mh_res <- rbind(readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mh9-1.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mh9-2.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mh9-3.rds")),
                  readRDS(file=paste0(workdir,"Results/Katana output/primary-differences-mh9-4.rds")))

# 2.2. Combine into single list of results
primary_res <- list(p_pcsa_res,p_mcsa_res,
                    p_gh_res,p_pf_res,p_rp_res,p_bp_res,
                    p_vt_res,p_sf_res,p_re_res,p_mh_res)

# 2.3. Pool MI results using Rubin's rules
primary_diff_res <- do.call(cbind,lapply(seq(1,10), function (z,sds) {
  
  y <- primary_res[[z]]
  
  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
}))

standard_diff_res <- do.call(cbind,lapply(seq(1,10), function (z,sds) {
  
  y <- primary_res[[z]]
  sd <- sds[,z]
  
  coef <- do.call(rbind,lapply(y,function (x) {
    x[,1]
  }))/sd
  se <- do.call(rbind,lapply(y,function (x) {
    x[,2]
  }))/sd
  
  res <- t(do.call(rbind,mi.meld(q=coef,se=se)))
  
},sds=sds))

# 2.3 Assign column names and separate out sustained and inititiation results
colnames(primary_diff_res) <- names
primary_diff_sust <- as.data.frame(primary_diff_res[1:5,])
primary_diff_init <- as.data.frame(primary_diff_res[5:9,])
primary_diff_sust$result <- c("stop_50","stop_55","stop_60","stop_65","stop_70")
primary_diff_init$result <- c("start_45","start_50","start_55","start_60","start_65")

colnames(standard_diff_res) <- names
standard_diff_sust <- as.data.frame(standard_diff_res[1:5,])
standard_diff_init <- as.data.frame(standard_diff_res[5:9,])
standard_diff_sust$result <- c("stop_50","stop_55","stop_60","stop_65","stop_70")
standard_diff_init$result <- c("start_45","start_50","start_55","start_60","start_65")

######################################################################################
# 3. Calculate E-Values
#-------------------------------------------------------------------------------------

evalues_sust <- do.call(rbind,lapply(seq(1,5), function (x) {
  
  c(evalues.MD(standard_diff_sust[x,1],standard_diff_sust[x,2])[2,1],
    evalues.MD(standard_diff_sust[x,3],standard_diff_sust[x,4])[2,1],
    evalues.MD(standard_diff_sust[x,5],standard_diff_sust[x,6])[2,1],
    evalues.MD(standard_diff_sust[x,7],standard_diff_sust[x,8])[2,1],
    evalues.MD(standard_diff_sust[x,9],standard_diff_sust[x,10])[2,1],
    evalues.MD(standard_diff_sust[x,11],standard_diff_sust[x,12])[2,1],
    evalues.MD(standard_diff_sust[x,13],standard_diff_sust[x,14])[2,1],
    evalues.MD(standard_diff_sust[x,15],standard_diff_sust[x,16])[2,1],
    evalues.MD(standard_diff_sust[x,17],standard_diff_sust[x,18])[2,1],
    evalues.MD(standard_diff_sust[x,19],standard_diff_sust[x,20])[2,1])
  
}))

evalues_init <- do.call(rbind,lapply(seq(1,5), function (x) {
  
  c(evalues.MD(standard_diff_init[x,1],standard_diff_init[x,2])[2,1],
    evalues.MD(standard_diff_init[x,3],standard_diff_init[x,4])[2,1],
    evalues.MD(standard_diff_init[x,5],standard_diff_init[x,6])[2,1],
    evalues.MD(standard_diff_init[x,7],standard_diff_init[x,8])[2,1],
    evalues.MD(standard_diff_init[x,9],standard_diff_init[x,10])[2,1],
    evalues.MD(standard_diff_init[x,11],standard_diff_init[x,12])[2,1],
    evalues.MD(standard_diff_init[x,13],standard_diff_init[x,14])[2,1],
    evalues.MD(standard_diff_init[x,15],standard_diff_init[x,16])[2,1],
    evalues.MD(standard_diff_init[x,17],standard_diff_init[x,18])[2,1],
    evalues.MD(standard_diff_init[x,19],standard_diff_init[x,20])[2,1])
  
}))

######################################################################################
# 4. Create data summary matrix
#-------------------------------------------------------------------------------------

evalue_summary <- rbind(cbind(primary_diff_sust[,1:2],evalue=evalues_sust[,1],
                              primary_diff_sust[,3:4],evalue=evalues_sust[,2],
                              primary_diff_sust[,5:6],evalue=evalues_sust[,3],
                              primary_diff_sust[,7:8],evalue=evalues_sust[,4],
                              primary_diff_sust[,9:10],evalue=evalues_sust[,5],
                              primary_diff_sust[,11:12],evalue=evalues_sust[,6],
                              primary_diff_sust[,13:14],evalue=evalues_sust[,7],
                              primary_diff_sust[,15:16],evalue=evalues_sust[,8],
                              primary_diff_sust[,17:18],evalue=evalues_sust[,9],
                              primary_diff_sust[,19:20],evalue=evalues_sust[,10]),
                        cbind(primary_diff_init[,1:2],evalue=evalues_init[,1],
                              primary_diff_init[,3:4],evalue=evalues_init[,2],
                              primary_diff_init[,5:6],evalue=evalues_init[,3],
                              primary_diff_init[,7:8],evalue=evalues_init[,4],
                              primary_diff_init[,9:10],evalue=evalues_init[,5],
                              primary_diff_init[,11:12],evalue=evalues_init[,6],
                              primary_diff_init[,13:14],evalue=evalues_init[,7],
                              primary_diff_init[,15:16],evalue=evalues_init[,8],
                              primary_diff_init[,17:18],evalue=evalues_init[,9],
                              primary_diff_sust[,19:20],evalue=evalues_sust[,10]))

######################################################################################
# 5. Update excel file with evalue results
#-------------------------------------------------------------------------------------

wb <- loadWorkbook(paste0(workdir,"Results/Tables.xlsx"))
sheet <- getSheets(wb)$EValueResults
addDataFrame(evalue_summary,
                   sheet)
saveWorkbook(wb, paste0(workdir,"Results/Tables.xlsx"))
