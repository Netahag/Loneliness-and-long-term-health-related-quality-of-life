######################################################################################
##   
## Australian Longitudinal Study on Women's Health: ALSWH
## Multiple imputation using random forests
## Date: September 2023
##
######################################################################################
# 1. Setup Environment
#-------------------------------------------------------------------------------------

##workdir <- "Y:/Paper 2. Loneliness LCA/Data/"

workdir <- "R:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/"

libs <- c("mice","miceadds","VIM","UpSetR","ggplot2","naniar", "dplyr", "haven")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}

lapply(libs, library, character.only = TRUE)

set.seed(966495)

######################################################################################
# 2. Load and process data
#-------------------------------------------------------------------------------------

load(paste0(workdir,"wide_data.RData"))


######################################################################################
# 3. Process data and assess missingness
#-------------------------------------------------------------------------------------

# 3.1 Sort data from most to least missing, saving order to return data to original order if needed
res<-summary(aggr(wide_data))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataimp <- wide_data[,res$Variable]

# 3.2 Assess amount and patterns of missing data
mean(is.na(dataimp))
mean(complete.cases(dataimp))

gg_miss_upset(dataimp,
              nintersects = 15)


library(naniar)
library(misty)

#Figure of missing data
vis_miss(wide_data)
gg_miss_var(wide_data) + theme_bw()

#Littles test 
mcar_test(wide_data) 
#small p-value indicates not missing completely at random


######################################################################################
# 5. Define Imputation Parameters
#-------------------------------------------------------------------------------------

m <- 20 # number of imputations
n <- 4 # number of cores for parlmice to use
nimpcore <- m/n
maxit <- 50; # Number of mice iterations
default <- c("rf","rf","rf","rf") # Manually defined list of methods for each variable type



######################################################################################
# 6. Run multiple imputation using MICE with random forests
#-------------------------------------------------------------------------------------

imp_mice <- parlmice(data=dataimp,
                     m=m,
                     n.core=n,
                     n.imp.core=nimpcore, 
                     maxit=maxit,
                     defaultMethod=default,
                     clusterseed=564110)
end <- Sys.time()

imp <- mids2datlist(imp_mice)

pm <- imp_mice$predictorMatrix

######################################################################################
# 6. Add variable identifying which imputation, and merge in the unimputed data
#-------------------------------------------------------------------------------------

wide_data$imp <- 0
for (i in seq(1,m)) {
  imp[[i]]$imp <- i
}
imp[[21]] <- wide_data

# 7 Create derived variables and drop originals
imp <- lapply(imp,function (x) {
  x$b_alcliferisk <- ifelse(x$b_alcfq>10,1,0)
  x$b_alcepisrisk <- ifelse(x$b_alcbng>1,1,0)
  x$b_alcrisk <- ifelse(x$b_alcliferisk==1 | x$b_alcepisrisk==1,1,0)
  
  x[,c("b_alcliferisk","b_alcepisrisk","b_alcrisk")] <- lapply(x[,c("b_alcliferisk","b_alcepisrisk","b_alcrisk")], factor, labels=c("No","Yes"))
  x <- subset(x, select = -c(b_alcfq,b_alcbng,b_inarea))
  x
})  


######################################################################################
# 8. Save data
#-------------------------------------------------------------------------------------

save(imp,file=paste0(workdir,"imputed data.RData"))

# 8.1 Save R data as lists
save(imp,file=paste0(workdir,"primary imputed data.RData"))

# 8.2 Save Stata data as dataframes
imp_primary_stata <- do.call(rbind,imp)
write_dta(imp_primary_stata,path=paste0(workdir,"primary imputed data.dta"))





