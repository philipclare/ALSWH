/****************************************************************************/
//
// Syntax File 7
// Physical Activity Latent Class Analysis
// Estimate BCH weights and save for analysis
// Date: 6 October 2022
// Author: Philip J Clare
//
/****************************************************************************/
// 1. Setup Environment and start log
//----------------------------------------------------------------------------

log using "Y:\PRJ-prc_alswh\Paper 0 - Latent Class Analysis\Results\weight creation 20221007.smcl", replace

capture program drop bchweight 
program bchweight, eclass properties(mi)
local nclass="`1'"

svyset [pweight=b_wtarea]

svy: gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class `nclass') emopts(iterate(100)) listwise
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

use "Y:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary analysis data.dta", clear

/****************************************************************************/
// 3. Run program to generate weights, then expand to multiple record data
//----------------------------------------------------------------------------

gen bchw1=.
gen bchw2=.
gen bchw3=.
gen bchw4=.
mi xeq: bchweight 4 // argument passed to the program is the number of classes to use

drop modclass // drop 'modal' class
mi reshape long bchw, i(idproj) j(modclass)

/****************************************************************************/
// 4. Save data with BCH weights for distal analysis
//----------------------------------------------------------------------------

save "Y:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary analysis data - with weights.dta", replace

log close
