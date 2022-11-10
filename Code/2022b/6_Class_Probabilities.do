/****************************************************************************/
//
// Syntax File 6
// Physical Activity Latent Class Analysis
// Estimate posterior class probabilities and patterns of exposure in classes
// Date: 6 October 2022
// Author: Philip J Clare
//
/****************************************************************************/
// 1. Setup Environment and start log
//----------------------------------------------------------------------------

log using "Y:\PRJ-prc_alswh\Paper 0 - Latent Class Analysis\Results\class characteristics 20221005.smcl", replace

capture program drop lcmeans 
program lcmeans, eclass properties(mi)
local nclass="`1'"
global m=$m+1

qui gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class `nclass') nolog emopts(iterate(100)) listwise
predict classpost*, classposteriorpr
qui estat lcmean

matrix b=r(b)
matrix V=r(V)

egen modclass = rowmax(classpost*)
forvalues i=1/`nclass' {
	replace modclass=`i' if modclass==classpost`i'
}

matrix H$m=J(`nclass',`nclass',.)
forvalues i=1/`nclass' {
	forvalues j=1/`nclass' {
		qui su classpost`j' if modclass==`i'
		matrix H$m[`i',`j']=r(mean)
	}
}

ereturn post b V
ereturn local cmd="estat_lcmean"

end 

/****************************************************************************/
// 2. Load data
//----------------------------------------------------------------------------

use "Y:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary analysis data.dta", clear

/****************************************************************************/
// 3. Run program to generate posterior mean probabilities etc
//----------------------------------------------------------------------------

global m=0
mi estimate, dots: lcmeans 4

forvalues i=1/2 {
	matrix H`i'=H`i'[1,1..4],H`i'[2,1..4],H`i'[3,1..4],H`i'[4,1..4]
}
matrix H=H1\H2\H3\H4\H5\H6\H7\H8\H9\H10\H11\H12\H13\H14\H15\H16\H17\H18\H19\H20
mata
: h = st_matrix("H")
: m = mean(h)
: p = m[1,1..4]\m[1,5..8]\m[1,9..12]\m[1,13..16]
: st_matrix("M",p)
end

matrix list M

log close