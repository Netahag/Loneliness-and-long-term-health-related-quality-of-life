/****************************************************************************/
//
// Loneliness and QoL Latent Class Analysis
// Run latent class regresion and regression on distal outcomes using BCH
// Date: 17 October 2028
// Author: Philip J Clare & Neta HaGani
//
/****************************************************************************/
// 1. start log
//----------------------------------------------------------------------------

log using "Y:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/distal regressions 20231710.smcl", replace


/****************************************************************************/
// 2. Latent class regression on baseline covariates
//----------------------------------------------------------------------------

// load wide format data
use "Y:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/wide data - with weights.dta", clear

//main analysis
mi estimate, dots esampvaryok errorok rrr: mlogit modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long ///
, base(2)


mean  lonely_binary3 lonely_binary4 lonely_binary5 lonely_binary6 lonely_binary7 lonely_binary8, over(modclass)
mean classpost1 classpost2 classpost3 classpost4, over(modclass)

// obtain mean and sd of QoL outcomes
summarize pcsa mcsa pf rp bp gh vt sf re mh  

// creat mean varibable of loneliness accross all waves and than get the mean for each class
egen mean_loneliness = rowmean(lonely_binary3 lonely_binary4 lonely_binary5 lonely_binary6 lonely_binary7 lonely_binary8)
mean mean_loneliness, over(modclass)

mean  lonely_binary3 , over(modclass)

mean  lonely_binary8

/****************************************************************************/
// 3. Run regression on primary distal outcomes
//----------------------------------------------------------------------------

// load long format data

use "R:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/long data - with weights.dta", clear

// pcsa
// unadjusted model
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// mcsa
// unadjusted model
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


/****************************************************************************/
// 4. Run regression on secondary distal outcomes
//----------------------------------------------------------------------------

// pf
// unadjusted model
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// rp
// unadjusted model
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// bp

// unadjusted model
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// gh
// unadjusted model
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// vt
// unadjusted model
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


// sf
// unadjusted model
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// re
// unadjusted model
mi estimate, dots esampvaryok errorok: regress re ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress re ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress re ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// mh
// unadjusted model
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


/*************************************************************
/***********************************************************
//sensitivity analysis- using loneliness different cutpoints (lowest level vs. the three highest)
/-----------------------------------------------------------
// load wide format data for sensitivity analysis
use "Y:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/wide data - with weights_sens.dta", clear

mi estimate, dots esampvaryok errorok rrr: mlogit modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long ///
, base(2)

/****************************************************************************/
// 3. Run regression on primary distal outcomes
//----------------------------------------------------------------------------

// load long format data

use "Y:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/long data - with weights_sens.dta", clear

// pcsa
// unadjusted model
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// mcsa
// unadjusted model
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


/****************************************************************************/
// 4. Run regression on secondary distal outcomes
//----------------------------------------------------------------------------

// pf
// unadjusted model
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// rp
// unadjusted model
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// bp

// unadjusted model
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// gh
// unadjusted model
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// vt
// unadjusted model
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


// sf
// unadjusted model
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// re
// unadjusted model
mi estimate, dots esampvaryok errorok: regress re ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress re ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress re ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// mh
// unadjusted model
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)




/*************************************************************
/***********************************************************
//sensitivity analysis- using probit data
/-----------------------------------------------------------
// load probit wide format data for sensitivity analysis
use "Y:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/wide data - with weights_sens_prob.dta", clear

mi estimate, dots esampvaryok errorok rrr: mlogit modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long ///
, base(2)

/****************************************************************************/
// 3. Run regression on primary distal outcomes
//----------------------------------------------------------------------------

// load long format data

use "Y:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/Data/long data - with weights_sens_prob.dta", clear

// pcsa
// unadjusted model
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress pcsa ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// mcsa
// unadjusted model
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress mcsa ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


/****************************************************************************/
// 4. Run regression on secondary distal outcomes
//----------------------------------------------------------------------------

// pf
// unadjusted model
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress pf ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// rp
// unadjusted model
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress rp ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// bp

// unadjusted model
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress bp ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// gh
// unadjusted model
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress gh ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// vt
// unadjusted model
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress vt ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


// sf
// unadjusted model
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress sf ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// re
// unadjusted model
mi estimate, dots esampvaryok errorok: regress re ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress re ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress re ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)

// mh
// unadjusted model
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass /// 
[iweight=bchw], vce(cluster idproj)

// model adjusted for sociodemographics
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone /// 
[iweight=bchw], vce(cluster idproj)

// fully adjusted model
mi estimate, dots esampvaryok errorok: regress mh ib2.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_depression i.b_anxiety i.b_language ///
c.b_age c.b_seifadis i.b_mstat i.b_ariapgp i.b_employ i.b_country i.b_educ i.b_live_alone c.b_mos_long /// 
[iweight=bchw], vce(cluster idproj)


























log close