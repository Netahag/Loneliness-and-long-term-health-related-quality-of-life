######################################################################################
##   
## Lonelyness and QoL Latent Class Analysis
## Extract key variables from separate wave datasets
## Date: 30 August 2023
## Authors: Philip Clare and Neta Hagani
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

######################################################################################
# Setup Environment----------

workdir <- "R:/PRJ-Loneliness_ALSWH/"

libs <- c("haven","plyr","dplyr","tidyr","summarytools", "foreign")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 1. Wave 1 Data----------------
w1data <- read_sav(paste0(workdir,"Data/wha1midb.sav"))
w1data <- zap_label(w1data)
w1data <- zap_labels(w1data)
w1data <- zap_formats(w1data)

w1data <- w1data %>%
  select(idproj, m1wtarea, inarea, m1ariapgp, m1mnstrs, m1smokst, m1age, m1cobcat, m1ariapgp, m1q90, m1q77a, m1labf, m1lngcat, m1q76a, m1whobmigroup, m1q43, m1q44, m1q45)

w1data <- w1data %>% 
  rename_all(~stringr::str_replace(.,"^m1",""))

# static variables
# education
w1data <- w1data %>% mutate(b_educ = recode(q90, 
                                            `1` = 0,
                                            `2` = 0,
                                            `3` = 0,
                                            `4` = 1,
                                            `5` = 1,
                                            `6` = 2,
                                            `7` = 2))
w1data$b_educ <- factor(w1data$b_educ,labels=c("Less than high school","Trade/apprentice/certificate/diploma","University"))

# country of birth
w1data <- w1data %>% mutate(b_country = recode(cobcat, 
                                               `1` = 0,
                                               `2` = 1,
                                               `3` = 1,
                                               `4` = 1,
                                               `5` = 1))
w1data$b_cobcat <- factor(w1data$b_cobcat,labels=c("Australia","Other"))

# rename
w1data <- w1data %>% 
  rename(b_wtarea = wtarea)
w1data <- w1data %>% 
  rename(b_inarea = inarea)

# recoding language spoken at home 1 = English, 2 = European, 3 = Asian, 4 = other
w1data <- w1data %>%
  mutate(b_language = case_when(lngcat >=1 & lngcat <=2 ~ 1,
                                lngcat == 3 ~ 2,
                                lngcat == 4 ~ 3,
                                lngcat == 5 ~ 4))
w1data$b_language <- factor(w1data$b_language,labels=c("English","European","Asian","other"))

#w1check <- subset(w1data, select = c(activity_time, q63, q64, q65))
#%>% 
#select(country, cobcat) %>% 
#View()

w1data <- subset(w1data, select = c(idproj, b_educ, b_wtarea, b_inarea, b_language, b_country))

save(w1data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w1.RData"))

######################################################################################
# 2. Wave 2 Data-------------------
w2data <- read_sav(paste0(workdir,"Data/wha2midb.sav"))
w2data <- zap_label(w2data)
w2data <- zap_labels(w2data)
w2data <- zap_formats(w2data)

w2data <- w2data %>%
  select(idproj, m2I_disAd, m2mnsocsup6, m2smokst, m2q20q, m2q20p, m2q29az, m2q29aaa, m2age, m2ariapgp, m2marital, m2q54mn, m2q54sc, m2q74a, m2q74b, m2q74c, m2q74d, m2q74e, m2q74f, m2q74g, m2whobmigroup, m2mnstrs, m2q39, m2q40, m2q41, m2q82a, m2q82b, m2q82c, m2q82d, m2q82e, m2q82f, m2q82g, m2q82h, m2q82i, m2q82j, m2q82k, m2q82l, m2q82m, m2q82n, m2q82o, m2q82p, m2q82q, m2q82r, m2q82s, m2cesd10, m2q31a, m2q31b, m2q31c, m2q31d, m2q31e, m2q31f, m2q31g, m2q31h, m2q31j, m2pf,m2mh,m2gh,m2re,m2rp,m2sf,m2bp,m2vt,m2pcsa,m2mcsa
  )

# removing m2 initial
w2data <- w2data %>% 
  rename_all(~stringr::str_replace(.,"^m2",""))

# renaming Seifa Index of Disadvantage
w2data <- w2data %>% 
  rename(b_seifadis = I_disAd)

# renaming MOS social support 6-items- higher score- higher social support
w2data <- w2data %>% 
  rename(b_mos_short = mnsocsup6)

# creating mean score for all of the MOS social support items- higher score- higher social support
w2data <- w2data %>%
  rowwise() %>%
  mutate(b_mos_long = mean(c(q82a, q82b, q82c, q82d, q82e, q82f, q82g, q82h, q82i, q82j, q82k, q82l, q82m, q82n, q82o, q82p, q82q, q82r, q82s)))

# Recoding marital status 
# 1 = married/partnered, 2= separated/ divorced/ never married/single, 3= widowed
w2data <- w2data %>%
  mutate(b_mstat = case_when((marital >= 1 & marital <= 2) ~ 1, 
                             (marital >= 3 & marital <= 4) | marital == 6 ~ 2,
                             marital == 5 ~ 3)) 
w2data$mstat <- factor(w2data$b_mstat,labels=c("married/partnered","separated/ divorced/ never married/single","widowed"))

# living situation- if either of the items are larger than 1, then live with someone = 1, if it = 1 then not live with someone = 0
w2data$b_live_alone <- ifelse(w2data$q74a>1 | w2data$q74b>1 | w2data$q74c>1| w2data$q74d>1
                              | w2data$q74e>1| w2data$q74f>1| w2data$q74g>1,
                              1,0)
w2data$b_live_alone <- factor(w2data$b_live_alone,labels=c("live alone","live with someone"))

#w2check <- subset(w2data, select = c(b_live_alone,q74a,q74b,q74c,q74d,q74e,q74f,q74g))


# main and secondary occupations- recode to not employed/employed 
w2data$q54sc <- ifelse(is.na(w2data$q54sc),0,w2data$q54sc)
w2data$b_employ <- ifelse(((w2data$q54mn<1 | w2data$q54mn>3) & (w2data$q54sc<1 | w2data$q54sc>3)),
                          0,1)
w2data$b_employ <- factor(w2data$b_employ,labels=c("Not employed","Employed"))

w2data <- w2data %>% mutate(b_alcfre = recode(q39, 
                                              `1` = 0,
                                              `2` = 0,
                                              `3` = 0,
                                              `4` = 1.5,
                                              `5` = 3.5,
                                              `6` = 5.5,
                                              `7` = 7))
w2data <- w2data %>% mutate(b_alcqnt = recode(q40, 
                                              `1` = 1.5,
                                              `2` = 3.5,
                                              `3` = 6.5,
                                              `4` = 9))
w2data <- w2data %>% 
  rename(b_alcbng = q41)

w2data$b_alcfq <- w2data$b_alcfre * w2data$b_alcqnt


# anxiety variable:
# if was ever diagnosed and and had symptoms sometimes-often, than had anxiety
# if one of the variables is missing than the total value is missing

w2data$b_anxiety <- ifelse(w2data$q29aaa>2 | w2data$q20q>0,
                           1,0)
w2data$b_anxiety <- ifelse(is.na(w2data$q29aaa) | is.na(w2data$q20q), NA, w2data$b_anxiety)

w2data$b_anxiety <- factor(w2data$b_anxiety,labels=c("no","yes"))
#w2check <- subset(w2data, select = c(b_anxiety, q20q, q29aaa))
#View(w2check)
#freq(w2check$b_anxiety)


# depression variable:
# if was ever diagnosed and and had symptoms sometimes-often, than had depression
# if one of the variables is missing than the total value is missing

w2data$b_depression <- ifelse(w2data$q29az>2 | w2data$q20p>0,
                           1,0)
w2data$b_depression <- ifelse(is.na(w2data$q29az) | is.na(w2data$q20p), NA, w2data$b_depression)

w2data$b_depression <- factor(w2data$b_depression,labels=c("no","yes"))

#w2check <- subset(w2data, select = c(b_depression, q20p, q29az))
#View(w2check)
#freq(w2check$b_depression)

w2data <- w2data %>% 
  rename(b_age = age,
         b_ariapgp = ariapgp,
         b_bmigp = whobmigroup,
         b_smokst = smokst,
         b_stress = mnstrs,
         b_cesd = cesd10)

w2data$b_bmigp <- factor(w2data$b_bmigp, labels=c("Underweight","Healthy","Overweight","Obese"))

w2data <- w2data %>% mutate(b_ariapgp = recode(b_ariapgp, 
                                               `1` = 0,
                                               `2` = 1,
                                               `3` = 1,
                                               `4` = 2,
                                               `5` = 2,
                                               `6` = NA_real_))
w2data$b_ariapgp <- factor(w2data$b_ariapgp,labels=c("Major city","Regional","Remote"))

w2data <- w2data %>% mutate(b_smokst = recode(b_smokst, 
                                              `1` = 0,
                                              `2` = 1,
                                              `3` = 2,
                                              `4` = 2,
                                              `5` = 2,
                                              `6` = NA_real_))
w2data$b_smokst <- factor(w2data$b_smokst,labels=c("Never smoker","Ex smoker","Current smoker"))

# cesd-d 10 score (The extra item (k), 'I felt terrific', was added for the Mid age and Older cohorts so that the scale finished with a positive item) not included in the 10 items.
# ces-d sum score excluding loneliness

# positive mood items (e and h) reversed
#w2data <- w2data %>% mutate(q31e_rec = recode(q31e, 
#                                              `0` = 3,
#                                              `1` = 2,
#                                              `2` = 1,
#                                              `3` = 0))
#
#w2data <- w2data %>% mutate(q31h_rec = recode(q31h, 
 #                                             `0` = 3,
 #                                             `1` = 2,
  #                                            `2` = 1,
  #                                            `3` = 0))

# ces-d sum score
# if all are missing- then the sum is N/A
#w2data <- w2data %>%
#  rowwise() %>%
 # mutate(b_cesd_no_lonely = {
#    selected_vars <- c(q31a, q31b, q31c, q31d, q31f, q31g, q31j, q31e_rec, q31h_rec)
#    if (all(is.na(selected_vars))) {
 #     NA
 #   } else {
 #     sum(selected_vars, na.rm = TRUE)
 #   }
#  })

#w2check <- subset(w2data, select = c(cesd_no_lonely, q31a, q31b, q31c, q31d, q31f, q31g, q31j, q31e_rec, q31h_rec))
#View(w2check)

w2data <- w2data %>% 
  rename(b_pcsa = pcsa,
         b_mcsa = mcsa,
         b_pf = pf,
         b_mh = mh,
         b_gh = gh,
         b_re = re,
         b_rp = rp,
         b_sf = sf,
         b_bp = bp,
         b_vt = vt)

w2data <- subset(w2data, select = c(idproj, b_seifadis, b_mos_short, b_mos_long, b_smokst, b_age, b_ariapgp, b_mstat, b_live_alone, b_employ, b_bmigp, b_stress, b_alcfq, b_alcbng, b_anxiety, b_depression, b_cesd, b_pcsa, b_mcsa, b_pf, b_mh, b_gh, b_re, b_rp, b_sf, b_bp, b_vt))

save(w2data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w2.RData"))
 
######################################################################################
# 3. Wave 3 Data---------------
w3data <- read_sav(paste0(workdir,"Data/wha3midb.sav"))
w3data <- zap_label(w3data)
w3data <- zap_labels(w3data)
w3data <- zap_formats(w3data)

w3data <- w3data %>%
  select(idproj, m3q44i)

w3data$wave <- 3
w3data <- w3data %>% 
  rename_all(~stringr::str_replace(.,"^m3",""))

# recoding loneliness binary rarely-some or a little = 0 , occasionally - most = 1
w3data$lonely_binary <- ifelse(w3data$q44i >= 2, 1,0)

# loneliness as category 
w3data <- w3data %>% 
  rename(lonely_category = q44i)

w3data <- subset(w3data, select = c(idproj, wave, lonely_binary, lonely_category))

save(w3data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w3.RData"))

######################################################################################
# 4. Wave 4 Data---------------
w4data <- read_sav(paste0(workdir,"Data/wha4midb.sav"))
w4data <- zap_label(w4data)
w4data <- zap_labels(w4data)
w4data <- zap_formats(w4data)

w4data <- w4data %>%
  select(idproj, m4q52i)

w4data$wave <- 4
w4data <- w4data %>% 
  rename_all(~stringr::str_replace(.,"^m4",""))

# recodig loneliness binary rarely-some or a little = 0 , occasionally - most = 1
w4data$lonely_binary <- ifelse(w4data$q52i >= 2, 1,0)

# loneliness as category 
w4data <- w4data %>% 
  rename(lonely_category = q52i)

w4data <- subset(w4data, select = c(idproj, wave, lonely_binary, lonely_category))
save(w4data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w4.RData"))

######################################################################################
# 5. Wave 5 Data---------------
w5data <- read_sav(paste0(workdir,"Data/wha5midb.sav"))
w5data <- zap_label(w5data)
w5data <- zap_labels(w5data)
w5data <- zap_formats(w5data)

w5data <- w5data %>%
  select(idproj, m5q52i)

w5data$wave <- 5
w5data <- w5data %>% 
  rename_all(~stringr::str_replace(.,"^m5",""))

# recodig loneliness binary rarely-some or a little = 0 , occasionally - most = 1
w5data$lonely_binary <- ifelse(w5data$q52i >= 2, 1,0)

# loneliness as category 
w5data <- w5data %>% 
  rename(lonely_category = q52i)

w5data <- subset(w5data, select = c(idproj, wave, lonely_binary, lonely_category))
save(w5data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w5.RData"))

######################################################################################
# 6. Wave 6 Data---------------
w6data <- read_sav(paste0(workdir,"Data/wha6midb.sav"))
w6data <- zap_label(w6data)
w6data <- zap_labels(w6data)
w6data <- zap_formats(w6data)

w6data <- w6data %>%
  select(idproj, m6Q52i)

w6data$wave <- 6
w6data <- w6data %>% 
  rename_all(~stringr::str_replace(.,"^m6",""))

# recodig loneliness binary rarely-some or a little = 0 , occasionally - most = 1
w6data$lonely_binary <- ifelse(w6data$Q52i >= 2, 1,0)

# loneliness as category 
w6data <- w6data %>% 
  rename(lonely_category = Q52i)

w6data <- subset(w6data, select = c(idproj, wave, lonely_binary, lonely_category))
save(w6data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w6.RData"))

######################################################################################
# 7. Wave 7 Data---------------
w7data <- read_sav(paste0(workdir,"Data/wha7midb.sav"))
w7data <- zap_label(w7data)
w7data <- zap_labels(w7data)
w7data <- zap_formats(w7data)

w7data <- w7data %>%
  select(idproj, m7q44i)

w7data$wave <- 7
w7data <- w7data %>% 
  rename_all(~stringr::str_replace(.,"^m7",""))

# recodig loneliness binary rarely-some or a little = 0 , occasionally - most = 1
w7data$lonely_binary <- ifelse(w7data$q44i >= 2, 1,0)

# loneliness as category 
w7data <- w7data %>% 
  rename(lonely_category = q44i)

w7data <- subset(w7data, select = c(idproj, wave, lonely_binary, lonely_category))
save(w7data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w7.RData"))

######################################################################################
# 8. Wave 8 Data---------------
w8data <- read_sav(paste0(workdir,"Data/wha8midb.sav"))
w8data <- zap_label(w8data)
w8data <- zap_labels(w8data)
w8data <- zap_formats(w8data)

w8data <- w8data %>%
  select(idproj, M8Q43i)

w8data$wave <- 8
w8data <- w8data %>% 
  rename_all(~stringr::str_replace(.,"^M8",""))

# recodig loneliness binary rarely-some or a little = 0 , occasionally - most = 1
w8data$lonely_binary <- ifelse(w8data$Q43i >= 2, 1,0)

# loneliness as category 
w8data <- w8data %>% 
  rename(lonely_category = Q43i)

w8data <- subset(w8data, select = c(idproj, wave, lonely_binary, lonely_category))
save(w8data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w8.RData"))

######################################################################################
# 9. Wave 9 Data---------------
w9data <- read_sav(paste0(workdir,"Data/wha9midb.sav"))
w9data <- zap_label(w9data)
w9data <- zap_labels(w9data)
w9data <- zap_formats(w9data)

w9data <- w9data %>%
  select(idproj, m9bp, m9gh, m9mh, m9pf, m9re, m9rp, m9sf, m9vt, m9pcs_abs, m9mcs_abs)

w9data <- w9data %>% 
  rename_all(~stringr::str_replace(.,"^m9",""))

w9data <- w9data %>% 
  rename(pcsa = pcs_abs,
         mcsa = mcs_abs)

w9data <- subset(w9data, select = c(idproj, pf, mh, gh, re, rp, sf, bp, vt, pcsa, mcsa))
save(w9data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w9.RData"))

######################################################################################
# 10. Merge data.
#-------------------------------------------------------------------------------------

# Load individual wave data and merge into long form
#-------------------------------------------------------------------------------------
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w1.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w2.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w3.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w4.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w5.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w6.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w7.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w8.RData"))
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/loneliness and qol w9.RData"))

long_data <- rbind.fill(w3data,w4data,w5data,w6data,w7data,w8data)

wide_data <- reshape(long_data,
                     timevar=c("wave"), 
                     idvar=c("idproj"),
                     v.names=c("lonely_binary", "lonely_category"),
                     sep = "",
                     dir="wide")

base_data <- merge(w1data,w2data,by="idproj",
                   all.x=TRUE,
                   all.y=TRUE)
wide_data <- merge(base_data,wide_data,by="idproj",
                   all.x=TRUE,
                   all.y=TRUE)
wide_data <- merge(wide_data,w9data,by="idproj",
                   all.x=TRUE,
                   all.y=TRUE)

save(wide_data,file=paste0(workdir,"Paper 2. Loneliness LCA/Data/wide data.RData"))



# demografic table using the wide_data before imputations
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/wide_data.RData"))


subset_data <- wide_data[, c("idproj", "b_country", "b_educ", "b_language", "b_mstat", "b_depression", "b_anxiety", "b_mos_long", "b_age", "b_employ", "b_ariapgp", "b_stress", "b_smokst", "b_live_alone", "b_bmigp", "b_cesd", "b_pcsa", "b_mcsa", "b_gh", "b_pf","b_re","b_rp","b_vt","b_mh","b_bp","b_sf", "lonely_binary3")]


view(dfSummary(subset_data)) 

comp_data <- wide_data[complete.cases(wide_data),] 

# demografic table using the imp data with imputations
load(file=paste0(workdir,"Paper 2. Loneliness LCA/Data/imputed data.RData"))

subset_data <- imp[[1]][, c("idproj", "b_country", "b_educ", "b_language", "b_mstat", "b_depression", "b_anxiety", "b_mos_long", "b_age", "b_employ", "b_ariapgp", "b_stress", "b_smokst", "b_live_alone", "b_bmigp", "b_cesd", "b_pcsa", "b_mcsa", "b_gh", "b_pf","b_re","b_rp","b_vt","b_mh","b_bp","b_sf", "lonely_binary3")]

view(dfSummary(subset_data)) 

subset_data$b_age
library(skimr)
skim(subset_data$b_age)
median_age <- median(subset_data$b_age)
median_age
