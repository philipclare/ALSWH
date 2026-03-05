# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("dplyr","expss","haven","readxl","tidyverse")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

workdir <- "D:/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-GSHS/"

filenames <- list.files(paste0(workdir,"GSHS"), 
                        pattern="*.dta", 
                        full.names=TRUE)

ldf <- do.call(rbind,lapply(filenames[-c(15,45,126)], function (x) {
  x <- read_dta(x)
  names(x) <- tolower(names(x))
  x <- x[,c("q1","q2","q22","country","year","weight","psu_new","stratum_new")]
  x <- x %>%
    rename(age = "q1",
           sex = "q2",
           lonely = "q22")
  x
}))

ldf <- data.frame(ldf[complete.cases(ldf),])
ldf$country <- trimws(ldf$country,"both", whitespace = "[\\h\\v]")

region <- read_xlsx("D:/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-analysis of living arrangement/Data/UN Regions.xlsx")
names(region) <- c("country","code","abbr","region","region2")

ldf <- merge(ldf,region,by="country",all.x=TRUE)

ldf$country <- factor(ldf$country)
ldf$year <- factor(ldf$year)

ldf$lonely_b <- ifelse(ldf$lonely<=4,0,1)

saveRDS(ldf,paste0(workdir,"Analysis data.rds"))