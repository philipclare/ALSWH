//##################################################################################//
//   
// Project: Trends in loneliness and isolation in Australia
// Program: C1 - Data Combine.R
// Purpose: Combine data from all waves and create analysis variables
// Author: Philip Clare
// Date: 24 October 2023
// OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
//
//##################################################################################//
// 1. Load data from each wave one by one
//----------------------------------------------------------------------------------//

local wave="a b c d e f g h i j k l m n o p q r s t u v w"

forvalues j=1/23 {
	
	local i `: word `j' of `wave''
	
	use xwaveid xhhstrat xhhraid `i'hhwtsc `i'lssuppv `i'lssupnh `i'lssupvl `i'mrcurr `i'lsclub `i'lssocal ///
	`i'hgage `i'hgsex1 `i'edhigh1 `i'fiprosp `i'hhda10 `i'hhra `i'hhsra ///
	`i'anbcob `i'anengf `i'esdtl `i'hh0_4 `i'hh5_9 `i'hh10_14 `i'hhadult ///
	using "Y:\PRJ-hilda_data\Release 23\Stata Data\Combined Data\Combined_`i'230c.dta", clear
	
	rename `i'* *
	
	gen wave=`j'
	
	save "Y:\PRJ-hilda_data\Loneliness trends\Data\Wave `j'.dta", replace
}

//##################################################################################//
// 2. Combine all waves into a single dataset
//----------------------------------------------------------------------------------//

use "Y:\PRJ-hilda_data\Loneliness trends\Data\Wave 1.dta", clear

forvalues j=2/23 {
	
	append using "Y:\PRJ-hilda_data\Loneliness trends\Data\Wave `j'.dta"
	
}

//##################################################################################//
// 3. Clean data and create analysis variables
//----------------------------------------------------------------------------------//

destring xwaveid, replace
destring xhhraid, replace

drop if lssuppv==-10
drop if hgage<16

replace anengf=1 if anbcob==1

foreach i in hgsex1 hhra hhda10 hhsra hhda10 anengf anbcob esdtl mrcurr lsclub lssocal lssuppv lssupnh lssupvl fiprosp edhigh1 {
	replace `i'=. if `i'<0
}

gen household_size=hh0_4 + hh5_9 + hh10_14 + hhadult

egen lonely = rowmedian(lssuppv lssupnh lssupvl)
replace lonely=0 if lonely<=4
replace lonely=1 if lonely>4 & lonely!=.

gen lonely_sens=0 if lssupvl<=4
replace lonely_sens=1 if lssupvl>4 & lssupvl!=.

gen sup_1=1 if (mrcurr>2 & mrcurr!=.) & household_size==1 
gen sup_2=1 if lssocal>5 & lssocal!=.
gen sup_3=1 if lsclub==2 & lsclub!=.
egen support=rowtotal(sup_1 sup_2 sup_3), m
replace support=0 if support<2
replace support=1 if support>=2 & support!=.
drop sup_1 sup_2 sup_3

drop lssuppv lssupnh lssupvl lssocal lsclub

xtset xwaveid wave
gen lonely_chronic=0 if (lonely==0 | l1.lonely==0) & lonely!=. & l1.lonely!=.
replace lonely_chronic=1 if lonely==1 & l1.lonely==1
gen support_chronic=0 if (support==0 | l1.support==0) & support!=. & l1.support!=.
replace support_chronic=1 if support==1 & l1.support==1

bysort xwaveid: egen wgtsum=sum(hhwtsc)
bysort xwaveid: gen n=_N
gen weight=hhwtsc*(n/wgtsum)
drop n wgtsum hhwtsc

gen agecat=0 if hgage<30
replace agecat=1 if hgage>=30 & hgage<65
replace agecat=2 if hgage>=65

drop hgage

rename hgsex1 sex

gen educ=0 if edhigh1==10 | edhigh1==9 | edhigh1==8
replace educ=1 if edhigh1==5 | edhigh1==4
replace educ=2 if edhigh1==3 | edhigh1==2 | edhigh1==1

drop edhigh1

rename anbcob cob

rename anengf language

gen marital=0 if mrcurr==1 | mrcurr==2
replace marital=1 if mrcurr==3 | mrcurr==4 | mrcurr==6
replace marital=2 if mrcurr==5

egen living=rowtotal(hh0_4 hh5_9 hh10_14 hhadult)
replace living=2 if living>2 & living!=.

drop hh0_4 hh5_9 hh10_14 hhadult

gen employ=0 if esdtl==1 | esdtl==2
replace employ=1 if esdtl==3 | esdtl==4
replace employ=2 if esdtl==5 | esdtl==6

drop esdtl

gen aria=0 if hhsra==0
replace aria=1 if hhsra==1
replace aria=2 if hhsra==2 | hhsra==3 | hhsra==4

drop hhsra

gen seifa=0 if hhda10==1 | hhda10==2 | hhda10==3
replace seifa=1 if hhda10==4 | hhda10==5 | hhda10==6 | hhda10==7
replace seifa=2 if hhda10==8 | hhda10==9 | hhda10==10

drop hhda10

drop if weight==0

//##################################################################################//
// 4. Save final dataset
//----------------------------------------------------------------------------------//

save "Y:\PRJ-hilda_data\Loneliness trends\Data\Combined data.dta", replace