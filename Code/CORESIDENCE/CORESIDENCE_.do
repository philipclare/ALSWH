
global workdir="C:/Users/pcla5984/Dropbox (Sydney Uni)/Lancet series data analysis/Paper 1-analysis of living arrangement"

use "$workdir/Data/average_size.dta", clear

drop if T1>2019

replace T1=T1-1990
bysort Region: egen min_year=min(T1)
bysort Region: egen max_year=max(T1)
replace T1=T1+1990

bysort C1 T1: gen n=_N
bysort C1 T1: drop if n>1 & S2!=4
drop n S2

reshape wide HS17 HS01 P1 P4 D1 D6 hdi pweights_a pweights_b, i(C1) j(T1)
reshape long

replace T1=T1-1990

misstable summ C1 T1 HS17 HS01 P1 P4 D1 D6 Region hdi, all

mi set flong
mi set M=20


xi i.Region

gen T_sq=T1^2
gen T_cu=T1^3

gen _IRegion_2_t=_IRegion_2*T1
gen _IRegion_3_t=_IRegion_3*T1
gen _IRegion_4_t=_IRegion_4*T1
gen _IRegion_5_t=_IRegion_5*T1
gen _IRegion_6_t=_IRegion_6*T1
gen _IRegion_7_t=_IRegion_7*T1
gen _IRegion_8_t=_IRegion_8*T1
gen _IRegion_9_t=_IRegion_9*T1
gen _IRegion_10_t=_IRegion_10*T1
gen _IRegion_11_t=_IRegion_11*T1
gen _IRegion_12_t=_IRegion_12*T1
gen _IRegion_13_t=_IRegion_13*T1
gen _IRegion_14_t=_IRegion_14*T1
gen _IRegion_15_t=_IRegion_15*T1

gen _IRegion_2_t2=_IRegion_2*T_sq
gen _IRegion_3_t2=_IRegion_3*T_sq
gen _IRegion_4_t2=_IRegion_4*T_sq
gen _IRegion_5_t2=_IRegion_5*T_sq
gen _IRegion_6_t2=_IRegion_6*T_sq
gen _IRegion_7_t2=_IRegion_7*T_sq
gen _IRegion_8_t2=_IRegion_8*T_sq
gen _IRegion_9_t2=_IRegion_9*T_sq
gen _IRegion_10_t2=_IRegion_10*T_sq
gen _IRegion_11_t2=_IRegion_11*T_sq
gen _IRegion_12_t2=_IRegion_12*T_sq
gen _IRegion_13_t2=_IRegion_13*T_sq
gen _IRegion_14_t2=_IRegion_14*T_sq
gen _IRegion_15_t2=_IRegion_15*T_sq

gen hdi_t1=hdi*T1
gen hdi_t2=hdi*T_sq
gen hdi_t3=hdi*T_cu

mi register imputed HS17 HS01 P1 P4 D1 D6 hdi hdi_t1 hdi_t2 hdi_t3

mi imput chained ///
(regress) HS17 HS01 P1 P4 D1 D6 hdi hdi_t1 hdi_t2 hdi_t3 ///
= i.C1 c.T1 T_sq i.Region _IRegion_2_t _IRegion_3_t _IRegion_4_t _IRegion_5_t _IRegion_6_t _IRegion_7_t _IRegion_8_t _IRegion_9_t _IRegion_10_t _IRegion_11_t _IRegion_12_t _IRegion_13_t _IRegion_14_t _IRegion_15_t _IRegion_2_t2 _IRegion_3_t2 _IRegion_4_t2 _IRegion_5_t2 _IRegion_6_t2 _IRegion_7_t2 _IRegion_8_t2 _IRegion_9_t2 _IRegion_10_t2 _IRegion_11_t2 _IRegion_12_t2 _IRegion_13_t2 _IRegion_14_t2 _IRegion_15_t2, replace rseed(56816)

drop if T1<0
drop if T1<min_year | T1>max_year

gen hdi_cat=0 if hdi<0.550
replace hdi_cat=1 if hdi>=0.550 & hdi<0.700
replace hdi_cat=2 if hdi>=0.700 & hdi<0.800
replace hdi_cat=3 if hdi>=0.800 & hdi!=.

save "$workdir/Data/imputed_data.dta", replace


