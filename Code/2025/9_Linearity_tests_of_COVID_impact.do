//##################################################################################//
//   
// Project: Trends in loneliness and isolation in Australia
// Program: C3 - Linearity Tests.R
// Purpose: Combine data from all waves and create analysis variables
// Author: Philip Clare
// Date: 24 October 2023
// OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
//
//##################################################################################//
// 1. Start log file
//----------------------------------------------------------------------------------//

capture log close
log using "Y:/PRJ-hilda_data/Loneliness trends/logfiles/linearity tests 19 20231024.smcl", replace

//##################################################################################//
// 2. Load and finalise data
//----------------------------------------------------------------------------------//

// 2.1. Set working directory
cd "Y:/PRJ-hilda_data/Loneliness trends"
local workdir "Y:/PRJ-hilda_data/Loneliness trends"

// 2.2. Load analysis data
use "`workdir'/Data/combined data.dta", clear

// 2.3. Create log-time for models
gen lnwave=ln(wave)

gen covid=0
replace covid=1 if wave>=20

gen post_covid=wave-20
replace post_covid=0 if post_covid<0

gen lnpost_covid=ln(post_covid)

//##################################################################################//
// 3. Estimate models and save fit parameters in a matrix for comparison
//----------------------------------------------------------------------------------//

matrix ic_sum=J(9,8,.)

// Loneliness
melogit lonely c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[1,1]=temp[1,5..6]
melogit lonely c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[2,1]=temp[1,5..6]
melogit lonely c.wave##c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[3,1]=temp[1,5..6]
	
melogit lonely c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[4,1]=temp[1,5..6]
melogit lonely c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[5,1]=temp[1,5..6]
melogit lonely c.wave##c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[6,1]=temp[1,5..6]

melogit lonely c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[7,1]=temp[1,5..6]
melogit lonely c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[8,1]=temp[1,5..6]
melogit lonely c.wave##c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[9,1]=temp[1,5..6]

// Chronic loneliness
melogit lonely_chronic c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[1,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[2,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[3,3]=temp[1,5..6]
	
melogit lonely_chronic c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[4,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[5,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[6,3]=temp[1,5..6]

melogit lonely_chronic c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[7,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[8,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[9,3]=temp[1,5..6]
	
// Support
melogit support c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[1,5]=temp[1,5..6]
melogit support c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[2,5]=temp[1,5..6]
melogit support c.wave##c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[3,5]=temp[1,5..6]
	
melogit support c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[4,5]=temp[1,5..6]
melogit support c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[5,5]=temp[1,5..6]
melogit support c.wave##c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[6,5]=temp[1,5..6]

melogit support c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[7,5]=temp[1,5..6]
melogit support c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[8,5]=temp[1,5..6]
melogit support c.wave##c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[9,5]=temp[1,5..6]

// Chronic lack of support
melogit support_chronic c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[1,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[2,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave##c.wave c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[3,7]=temp[1,5..6]
	
melogit support_chronic c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[4,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[5,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave##c.wave c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[6,7]=temp[1,5..6]

melogit support_chronic c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[7,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[8,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave##c.wave c.post_covid##c.post_covid##c.post_covid [pweight=weight] || xhhraid: || xwaveid:
	estat ic
	matrix temp=r(S)
	matrix ic_sum[9,7]=temp[1,5..6]
	
matrix list ic_sum

log close