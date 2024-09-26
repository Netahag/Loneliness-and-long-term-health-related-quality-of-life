
/****************************************************************************/
//
// Loneliness Latent Class Analysis
// Estimate BCH weights and save for analysis
// Waves 3-7 
// Date: September 2023
// 
//
/****************************************************************************/
// 1. Setup Environment 
//----------------------------------------------------------------------------


capture program drop bchweight 
program bchweight, eclass properties(mi)
local nclass="`1'"


svyset [pweight=b_wtarea]

svy: gsem (lonely_category3_low lonely_category4_low lonely_category5_low lonely_category6_low lonely_category7_low lonely_category8_low <- ), logit lclass(class `nclass') emopts(iterate(100)) listwise
predict classpost*, classposteriorpr

egen modclass = rowmax(classpost*)

forvalues i=1/`nclass' {
	replace modclass=`i' if modclass==classpost`i' & modclass!=.
}

matrix H=J(`nclass',`nclass',.)
forvalues i=1/`nclass' {
	forvalues j=1/`nclass' {
		qui prop modclass
		local m=_b[`i'.modclass]
		gen temp=(((modclass==`i')*b_wtarea)*classpost`j')
		qui su temp
		matrix H[`i',`j']=r(mean)/`m'
		drop temp
	}
}
matrix Hstar=inv(H)

replace bchw1=Hstar[modclass,1]
replace bchw2=Hstar[modclass,2]
replace bchw3=Hstar[modclass,3]
replace bchw4=Hstar[modclass,4]


end

/****************************************************************************/
// 2. Load data
//----------------------------------------------------------------------------

use "X:/Paper 2. Loneliness LCA/Data/primary imputed data.dta", clear

mi import flong, m(imp) id(idproj) imputed(lonely_category3_low lonely_category4_low lonely_category5_low lonely_category6_low lonely_category7_low lonely_category8_low) clear



 
/****************************************************************************/
// 3. Run program to generate weights, then expand to multiple record data
//----------------------------------------------------------------------------

gen bchw1=.
gen bchw2=.
gen bchw3=.
gen bchw4=.
mi xeq: bchweight 4 // argument passed to the program is the number of classes to use



save "X:/Paper 2. Loneliness LCA/Data/wide data - with weights_sens.dta", replace


// load wide format data
use "X:/Paper 2. Loneliness LCA/Data/wide data - with weights_sens.dta", clear


drop modclass // drop 'modal' class

mi reshape long bchw, i(idproj) j(modclass)


/****************************************************************************/
// 4. Save data with BCH weights for distal analysis
//----------------------------------------------------------------------------

save "X:/Paper 2. Loneliness LCA/Data/long data - with weights_sens.dta", replace





