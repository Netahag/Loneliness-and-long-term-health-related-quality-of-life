/****************************************************************************/
//
// Syntax File 1
// ALSWH Loneliness Latent Class Analysis
// Latent class model fit
// Date: 13 July 2023
// Author: Philip J Clare
//
/****************************************************************************/
// 1. Load data and start log
//----------------------------------------------------------------------------

//log using "C:/Users/pcla5984/Dropbox (Sydney Uni)/ALSWH/Loneliness LCA/model fit 20230713.smcl", replace

//use "C:/Users/pcla5984/Dropbox (Sydney Uni)/ALSWH/Loneliness LCA/LCA_data.dta", clear

use "Y:/PRJ-Loneliness_ALSWH/Paper 2. Loneliness LCA/LCA_data.dta", clear

// generating new loneliness variable for sensitivity analysis with rarely lonely vs sometimes, occasionally and most-all of the time (0 vs 1-3)
recode lonely_category (0 = 0) (1 = 1) (2 = 1) (3 = 1), generate(lonely_category_low)

reshape wide lonely_binary lonely_category lonely_category_low, i(idproj) j(wave)

/****************************************************************************/
// 2. Estimate latent class models on complete data and generate fit statistics
//----------------------------------------------------------------------------

matrix summ=J(6,9,.)

forvalues i=1/6 {
	gsem (lonely_binary2 lonely_binary3 lonely_binary4 lonely_binary5 lonely_binary6 lonely_binary7 lonely_binary8 <- ), logit lclass(class `i') emopts(iterate(100)) iter(1000)
	estat ic
	matrix temp=r(S)
	matrix summ[`i',1]=temp[1,5..6]
	matrix summ[`i',3]= -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
	estat lcprob
	matrix temp=r(b)
	matrix summ[`i',4]=temp[1,1..`i']
}
matrix list summ


//Sensitivity analysis
/****************************************************************************/
// 2.1 Estimate latent class models on complete data and generate fit statistics
//----------------------------------------------------------------------------

matrix summ=J(6,9,.)

forvalues i=1/6 {
	gsem (lonely_category_low2 lonely_category_low3 lonely_category_low4 lonely_category_low5 lonely_category_low6 lonely_category_low7 lonely_category_low8 <- ), logit lclass(class `i') emopts(iterate(100)) iter(1000)
	estat ic
	matrix temp=r(S)
	matrix summ[`i',1]=temp[1,5..6]
	matrix summ[`i',3]= -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
	estat lcprob
	matrix temp=r(b)
	matrix summ[`i',4]=temp[1,1..`i']
}
matrix list summ


/****************************************************************************/
// 3. Based on model fit, best performing LCA is a 4-class solution. Refit to generate posterior probs
//----------------------------------------------------------------------------

local nclass=4

svyset [pweight=b_wtarea]

qui svy: gsem (lonely_category_low2 lonely_category_low3 lonely_category_low4 lonely_category_low5 lonely_category_low6 lonely_category_low7 lonely_category_low8 <- ), logit lclass(class `nclass') nolog emopts(iterate(100)) listwise
predict classpost*, classposteriorpr
estat lcmean

matrix b=r(b)
matrix V=r(V)

egen modclass = rowmax(classpost*)
forvalues i=1/`nclass' {
	replace modclass=`i' if modclass==classpost`i' & classpost`i'!=.
}

matrix H=J(`nclass',`nclass',.)
forvalues i=1/`nclass' {
	forvalues j=1/`nclass' {
		qui su classpost`j' if modclass==`i'
		matrix H[`i',`j']=r(mean)
	}
}

mean lonely_binary2 lonely_binary3 lonely_binary4 lonely_binary5 lonely_binary6 lonely_binary7 lonely_binary8, over(modclass)


