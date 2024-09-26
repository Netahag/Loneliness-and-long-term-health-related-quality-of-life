######################################################################################
##   
## Loneliness Latent Class Analysis
## Finalise data after imputation and format for analysis
## Date: 6 October 2022
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

workdir <- "R:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/"

libs <- c("plyr","dplyr","ltmle","gtools","haven")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load imputed data
#-------------------------------------------------------------------------------------

load(file=paste0(workdir,"Data/imputed data.RData"))
load(paste0(workdir,"Data/wide data.RData"))

######################################################################################
# 3. Create computed/derived variables
#-------------------------------------------------------------------------------------

# 3.1 Add variable identifying which imputation, and merge in the unimputed data

imp_data$imp <- 0
for (i in seq(1,m)) {
  imp[[i]]$imp <- i
}
imp[[21]] <- imp_data

# 3.2 Create derived variables and drop originals

imp <- lapply(imp,function (x) {
  x$b_alcliferisk <- ifelse(x$b_alcfq>10,1,0)
  x$b_alcepisrisk <- ifelse(x$b_alcbng>1,1,0)
  x$b_alcrisk <- ifelse(x$b_alcliferisk==1 | x$b_alcepisrisk==1,1,0)
  
  
  x[,c("b_alcliferisk","b_alcepisrisk","b_alcrisk")] <- lapply(x[,c("b_alcliferisk","b_alcepisrisk","b_alcrisk")], factor, labels=c("No","Yes"))
  x <- subset(x, select = -c(b_alcfq,b_alcbng,inarea))
  x
})  
  
  
  
  