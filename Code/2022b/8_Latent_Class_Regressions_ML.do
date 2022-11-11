/****************************************************************************/
//
// Syntax File 8
// Physical Activity Latent Class Analysis
// Latent Class Regression on Baseline Covariates using ML
// Date: 10 October 2022
// Author: Philip J Clare
//
/****************************************************************************/
// 1. Setup Environment and start log
//----------------------------------------------------------------------------

log using "Y:\PRJ-prc_alswh\Paper 0 - Latent Class Analysis\Results\baseline regression 20221011.smcl", replace

capture program drop mlregress 
program mlregress, eclass properties(mi)
local nclass="`1'"

gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class `nclass') emopts(iterate(20)) listwise
predict classpost*, classposteriorpr

matrix pmatrix = J(`nclass',`nclass',.)
matrix npmatrix = J(`nclass',`nclass',.)
matrix qmatrix = J(`nclass',`nclass',.)
matrix lq = J(`nclass',`nclass',.)

egen modclass = rowmax(classpost*)
forvalues i=1/`nclass' {
	replace modclass=`i' if modclass==classpost`i'
}

forvalues i=1/`nclass' {
	forvalues j=1/`nclass' {
		su classpost`j' if modclass==`i'
		matrix pmatrix[`i',`j'] = r(mean)*r(N)
		matrix npmatrix[`i',`j'] = r(mean)*r(N)*r(N)
	}
}

matrix coltotal = J(1, rowsof(npmatrix), 1) * npmatrix

forvalues i=1/`nclass' {
  forvalues j=1/`nclass' {
    matrix qmatrix[`j',`i'] = npmatrix[`i',`j']/coltotal[1,`j']
  }
}

forvalues i=1/`nclass' {
  forvalues j=1/`nclass' {
    matrix lq[`i',`j'] = log(qmatrix[`i',`j']/qmatrix[`i',1])
  }
}

local L_12=lq[1,2]
local L_13=lq[1,3]
local L_14=lq[1,4]
local L_22=lq[2,2]
local L_23=lq[2,3]
local L_24=lq[2,4]
local L_32=lq[3,2]
local L_33=lq[3,3]
local L_34=lq[3,4]
local L_42=lq[4,2]
local L_43=lq[4,3]
local L_44=lq[4,4]

capture noisily gsem ///
(1: 2.modclass<-_cons@`L_12')(1: 3.modclass<-_cons@`L_13')(1: 4.modclass<-_cons@`L_14') ///
(2: 2.modclass<-_cons@`L_22')(2: 3.modclass<-_cons@`L_23')(2: 4.modclass<-_cons@`L_24') ///
(3: 2.modclass<-_cons@`L_32')(3: 3.modclass<-_cons@`L_33')(3: 4.modclass<-_cons@`L_34') ///
(4: 2.modclass<-_cons@`L_42')(4: 3.modclass<-_cons@`L_43')(4: 4.modclass<-_cons@`L_44') ///
(CLASS <- i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age c.b_seifadis i.b_marital i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18) ///
, vce(robust) lclass(CLASS `nclass') emopts(iterate(20))

end

/****************************************************************************/
// 2. Load data
//----------------------------------------------------------------------------

use "R:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary analysis data.dta", clear

/****************************************************************************/
// 3. Run program via mi estimate to get mi regression estimates
//----------------------------------------------------------------------------

mi estimate, noisily: mlregress 4

log close
