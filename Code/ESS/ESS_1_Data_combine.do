global workdir="D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-ESS"

use "$workdir\Data\ESS_2010.dta", clear

keep idno cntry anweight fltlnla gndr agea
gen year=2010
rename fltlnla fltlnl

merge 1:1 idno cntry using "$workdir\Data\ESS_2010_sampling.dta", keepusing(psu stratum) nogen keep(match master)

save "$workdir\Data\Analysis data\ESS_2010.dta", replace

use "$workdir\Data\ESS_2012.dta", clear

keep idno cntry anweight fltlnl gndr agea
gen year=2012

merge 1:1 idno cntry using "$workdir\Data\ESS_2012_sampling.dta", keepusing(psu stratum) nogen keep(match master)

save "$workdir\Data\Analysis data\ESS_2012.dta", replace

use "$workdir\Data\ESS_2014.dta", clear

keep idno cntry anweight fltlnl gndr agea
gen year=2014

merge 1:1 idno cntry using "$workdir\Data\ESS_2014_sampling.dta", keepusing(psu stratum) nogen keep(match master)

tostring stratum, replace
replace stratum=cntry+"_2014_"+stratum
tostring psu, replace
replace psu=cntry+"_2023_"+psu

save "$workdir\Data\Analysis data\ESS_2014.dta", replace

use "$workdir\Data\ESS_2023.dta", clear

keep idno cntry psu stratum anweight fltlnl gndr agea
gen year=2023

tostring stratum, replace
replace stratum=cntry+"_2023_"+stratum
tostring psu, replace
replace psu=cntry+"_2023_"+psu

save "$workdir\Data\Analysis data\ESS_2023.dta", replace

use "$workdir\Data\Analysis data\ESS_2010.dta", clear
append using "$workdir\Data\Analysis data\ESS_2012.dta"
append using "$workdir\Data\Analysis data\ESS_2014.dta"
append using "$workdir\Data\Analysis data\ESS_2023.dta"
merge m:1 cntry using "$workdir\Data\Regions.dta", nogen keep(match master)

tostring idno, replace
replace idno=cntry+"_"+idno

bysort idno: egen sum_w = total(anweight)
bysort idno: gen group_n =_N
gen weight_a = anweight * (group_n / sum_w)

order weight_a, after(anweight)
drop anweight
rename weight_a anweight

save "$workdir\Data\Analysis data\Combined analysis data.dta", replace
