
global workdir "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-SHARE"

use "$workdir/Data/share_longitudinal_combined.dta", clear

bysort mergeid: egen wgtsum=sum(cciw)
bysort mergeid: gen n=_N
gen weight=cciw*(n/wgtsum)
drop n wgtsum cciw

gen all=1

replace stratum1="DEX" if country==12 & stratum1==""
replace stratum1="SEX" if country==13 & stratum1==""
replace stratum1="FRX" if country==17 & stratum1==""
replace stratum1="DMX" if country==18 & stratum1==""
replace stratum1="BEX" if country==23 & stratum1==""
replace stratum1="ILX" if country==25 & stratum1==""
replace stratum1="HUX" if country==32 & stratum1==""
replace stratum1="PTX" if country==33 & stratum1==""
replace stratum1="EEX" if country==35 & stratum1==""

drop if lonely_scale==. | wave==. | country==. | stratum1=="" | weight==.

merge m:1 country using "$workdir/Data/regions.dta", keep(match master) nogen
drop if region=="Western Asia"
encode region, gen(reg)
drop region
rename reg region
order region, before(region2)

foreach i in 51 35 32 57 29 61 63 34 47 28 48 18 55 13 19 16 33 15 53 59 11 23 17 12 25 20 31 {
	preserve

	keep if country==`i'
	
	qui svyset mergeid, strata(stratum1) || _n, weight(weight)
	
	qui svydescribe, generate(single)
	drop if single==1

	svy: mean lonely_scale, over(wave)
	matrix r`i'=r(table)'
	matrix n`i'=e(_N)'
	local dim (`= rowsof(r`i')') 
	matrix rtemp=J(`dim',1,`i')
	matrix r`i'=r`i',rtemp,n`i'
	restore
}

matrix region1=r51 \ r35 \ r32 \ r57 \ r29 \ r61 \ r63 \ r34 \ r47 \ r28 \ r48
matrix region2=r18 \ r55 \ r13 
matrix region3=r19 \ r16 \ r33 \ r15 \ r53 \ r59
matrix region4=r11 \ r23 \ r17 \ r12 \ r25 \ r20 \ r31

preserve
clear
svmat region1, names(col)
local names : rownames region1
gen names=""
forvalues i=1/`: word count `names'' {
  replace names=`"`: word `i' of `names''"' in `i'
}
keep b se c1 r1 names
export delimited "$workdir/Results/scale_region0.csv", replace
restore

preserve
clear
svmat region2, names(col)
local names : rownames region2
gen names=""
forvalues i=1/`: word count `names'' {
  replace names=`"`: word `i' of `names''"' in `i'
}
keep b se c1 r1 names
export delimited "$workdir/Results/scale_region1.csv", replace
restore

preserve
clear
svmat region3, names(col)
local names : rownames region3
gen names=""
forvalues i=1/`: word count `names'' {
  replace names=`"`: word `i' of `names''"' in `i'
}
keep b se c1 r1 names
export delimited "$workdir/Results/scale_region2.csv", replace
restore

preserve
clear
svmat region4, names(col)
local names : rownames region4
gen names=""
forvalues i=1/`: word count `names'' {
  replace names=`"`: word `i' of `names''"' in `i'
}
keep b se c1 r1 names
export delimited "$workdir/Results/scale_region3.csv", replace
restore