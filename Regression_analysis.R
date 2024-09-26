# loneliness and quality of life Latent Class Analysis
# Run latent class regresion and regression on distal outcomes using BCH
# Date: 18 September 2023
# Author: Philip J Clare and Neta HaGani
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

######################################################################################
# Setup Environment----------

workdir <- "R:/PRJ-Loneliness_ALSWH/"

libs <- c("haven","plyr","dplyr","tidyr","summarytools", "foreign", "nnet", "mice", "survey")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

############################################
# 1. Load data and start log
#----------------------------------------------------------------------------
completed_data <- read_dta("R:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/primary analysis long data - with weights.dta")
View(completed_data)


# Create a subset with only entries where '_mi_' is equal to 1
subset_data <- completed_data[completed_data$`_mi_m` == 1, ]



# 2. Latent class regression on baseline covariates

# Define survey design using inverse probability weights and clustering
design <- svydesign(ids = ~idproj, weights = ~ bchw, data = subset_data)

# Create a formula for the multinomial logistic regression
formula <- modclass ~ b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa

# Fit the survey-weighted multinomial logistic regression model
model <- svyglm(formula, design = design)

# Get summary statistics of the model
summary(model)


# 3. Run regression on primary distal outcomes

# Define survey design using inverse probability weights
design <- svydesign(ids = ~1, weights = ~ bchw, data = subset_data, nest = TRUE, id = ~idproj)

# Fit the survey-weighted regression model
model1 <- svyglm(pcsa ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                design = design)

model2 <- svyglm(mcsa ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                design = design)


# 4. Run regression on secondary distal outcomes

model3 <- svyglm(pf ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)



model4 <- svyglm(rp ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)


model5 <- svyglm(bp ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)


model6 <- svyglm(gh ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)


model7 <- svyglm(vt ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)


model8 <- svyglm(sf ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)


model9 <- svyglm(re ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)


model10 <- svyglm(mh ~ modclass + b_mos_long + b_bmigp + b_alcfq + b_cesd + b_live_alone + b_depression + b_anxiety + b_smokst + b_stress + b_employ + b_mstat + b_seifadis + b_ariapgp +  b_age + b_language + b_country + b_educ + b_pcsa + b_mcsa,
                 design = design)




# Get summary statistics of the model
summary(model)







