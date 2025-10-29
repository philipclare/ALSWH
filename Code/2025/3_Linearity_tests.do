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

log using "Y:/PRJ-hilda_data/Loneliness trends/logfiles/linearity tests 20250515.smcl", replace

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

//##################################################################################//
// 3. Estimate models and save fit parameters in a matrix for comparison
//----------------------------------------------------------------------------------//

matrix ic_sum=J(4,8,.)

melogit lonely c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[1,1]=temp[1,5..6]
melogit lonely c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[2,1]=temp[1,5..6]
melogit lonely c.wave##c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[3,1]=temp[1,5..6]
melogit lonely c.wave c.lnwave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[4,1]=temp[1,5..6]

melogit lonely_chronic c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[1,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[2,3]=temp[1,5..6]
melogit lonely_chronic c.wave##c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[3,3]=temp[1,5..6]
melogit lonely_chronic c.wave c.lnwave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[4,3]=temp[1,5..6]

melogit support c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[1,5]=temp[1,5..6]
melogit support c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[2,5]=temp[1,5..6]
melogit support c.wave##c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[3,5]=temp[1,5..6]
melogit support c.wave c.lnwave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[4,5]=temp[1,5..6]

melogit support_chronic c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[1,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[2,7]=temp[1,5..6]
melogit support_chronic c.wave##c.wave##c.wave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[3,7]=temp[1,5..6]
melogit support_chronic c.wave c.lnwave || xhhraid: || xwaveid:, intp(5)
estat ic
	matrix temp=r(S)
	matrix ic_sum[4,7]=temp[1,5..6]
	
matrix list ic_sum

log close