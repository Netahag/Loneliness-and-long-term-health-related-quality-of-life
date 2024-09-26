
######################################################################################
##   
## Lonelyness and QoL Latent Class Analysis
## Convert R data to Stata 
## Date: Spetember 2023
## Authors: Kat Owen

######################################################################################


workdir <- "X:/Paper 2. Loneliness LCA/Data/"
load(file=paste0(workdir,"wide data.RData"))

library("haven")

write_dta(wide_data,path=paste0(workdir,"wide data.dta"))