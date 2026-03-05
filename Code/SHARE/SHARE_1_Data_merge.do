
global workdir "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-SHARE"

use mergeid country cciw_w5 cchw_w5 stratum1 stratum2 psu ssu using "$workdir/Data/wave 5/sharew5_rel9-0-0_gv_weights.dta", clear
rename cciw_w5 cciw
rename cchw_w5 cchw
gen wave=5
merge 1:1 mergeid using "$workdir/Data/wave 5/sharew5_rel9-0-0_mh.dta", keepusing(mh034_ mh035_ mh036_ mh037_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 5/sharew5_rel9-0-0_dn.dta", keepusing(dn042_ dn003_ dn014_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 5/sharew5_rel9-0-0_sp.dta", keepusing(sp002_ sp003_* sp005_* sp007_* sp008_ sp009_* sp011_* sp013_* sp014_ sp015* sp016_* sp018_ sp019* sp020_ sp021*) keep(match) nogen
save "$workdir/Data/w5_combined.dta", replace

use mergeid country cciw_w6 cchw_w6 stratum1 stratum2 psu ssu using "$workdir/Data/wave 6/sharew6_rel9-0-0_gv_weights.dta", clear
rename cciw_w6 cciw
rename cchw_w6 cchw
gen wave=6
merge 1:1 mergeid using "$workdir/Data/wave 6/sharew6_rel9-0-0_mh.dta", keepusing(mh034_ mh035_ mh036_ mh037_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 6/sharew6_rel9-0-0_dn.dta", keepusing(dn042_ dn003_ dn014_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 6/sharew6_rel9-0-0_sp.dta", keepusing(sp002_ sp003_* sp005_* sp007_* sp008_ sp009_* sp011_* sp013_* sp014_ sp015* sp016_* sp018_ sp019* sp020_ sp021*) keep(match) nogen
save "$workdir/Data/w6_combined.dta", replace

use mergeid country cciw_w7 cchw_w7 stratum1 stratum2 psu ssu using "$workdir/Data/wave 7/sharew7_rel9-0-0_gv_weights.dta", clear
rename cciw_w7 cciw
rename cchw_w7 cchw
gen wave=7
merge 1:1 mergeid using "$workdir/Data/wave 7/sharew7_rel9-0-0_mh.dta", keepusing(mh034_ mh035_ mh036_ mh037_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 7/sharew7_rel9-0-0_dn.dta", keepusing(dn042_ dn003_ dn014_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 7/sharew7_rel9-0-0_sp.dta", keepusing(sp002_ sp003_* sp005_* sp007_* sp008_ sp009_* sp011_* sp013_* sp014_ sp015* sp016_* sp018_ sp019* sp020_ sp021*) keep(match) nogen
save "$workdir/Data/w7_combined.dta", replace

use mergeid country cciw_w8_main cchw_w8_main stratum1 stratum2 psu ssu using "$workdir/Data/wave 8/sharew8_rel9-0-0_gv_weights.dta", clear
rename cciw_w8_main cciw
rename cchw_w8_main cchw
gen wave=8
merge 1:1 mergeid using "$workdir/Data/wave 8/sharew8_rel9-0-0_mh.dta", keepusing(mh034_ mh035_ mh036_ mh037_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 8/sharew8_rel9-0-0_dn.dta", keepusing(dn042_ dn003_ dn014_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 8/sharew8_rel9-0-0_sp.dta", keepusing(sp002_ sp003_* sp005_* sp007_* sp008_ sp009_* sp011_* sp013_* sp014_ sp015* sp016_* sp018_ sp019* sp020_ sp021*) keep(match) nogen
save "$workdir/Data/w8_combined.dta", replace

use mergeid country cciw_w9 cchw_w9 stratum1 stratum2 psu ssu using "$workdir/Data/wave 9/sharew9_rel9-0-0_gv_weights.dta", clear
rename cciw_w9 cciw
rename cchw_w9 cchw
gen wave=9
merge 1:1 mergeid using "$workdir/Data/wave 9/sharew9_rel9-0-0_mh.dta", keepusing(mh034_ mh035_ mh036_ mh037_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 9/sharew9_rel9-0-0_dn.dta", keepusing(dn042_ dn003_ dn014_) keep(match) nogen
merge 1:1 mergeid using "$workdir/Data/wave 9/sharew9_rel9-0-0_sp.dta", keepusing(sp002_ sp003_* sp005_* sp007_* sp008_ sp009_* sp011_* sp013_* sp014_ sp015* sp016_* sp018_ sp019* sp020_ sp021*) keep(match) nogen
save "$workdir/Data/w9_combined.dta", replace

use "$workdir/Data/w5_combined.dta", clear
append using "$workdir/Data/w6_combined.dta"
append using "$workdir/Data/w7_combined.dta"
append using "$workdir/Data/w8_combined.dta"
append using "$workdir/Data/w9_combined.dta"

replace mh034_=. if mh034_<0
replace mh034_=4-mh034_
replace mh035_=. if mh035_<0
replace mh035_=4-mh035_
replace mh036_=. if mh036_<0
replace mh036_=4-mh036_
replace mh037_=. if mh037_<0
replace mh037_=4-mh037_

gen lonely_scale=mh034_ + mh035_ + mh036_
replace lonely_scale=0 if lonely_scale<6 
replace lonely_scale=1 if lonely_scale>=6 & lonely_scale!=.
label define noyes 0 "No" 1 "Yes"
label values lonely_scale noyes

label define howoften_ 1 "Hardly ever or never" 3 "Often", modify
rename mh037_ lonely_single

gen lonely_single_b=0 if lonely_single<=2
replace lonely_single_b=1 if lonely_single==3
label values lonely_single_b noyes

tab lonely_scale
tab lonely_single
tab lonely_single_b

save "$workdir/Data/share_longitudinal_combined.dta", replace

