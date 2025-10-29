//##################################################################################//
//   
// Project: Trends in loneliness and isolation in Australia
// Program: C6 - Demographic interactions
// Purpose: Estimate trends in loneliness using mixed effects models
// Author: Philip Clare
// Date: 24 October 2023
// OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
//
//##################################################################################//
// 1. Start log file
//----------------------------------------------------------------------------------//

log using "Y:/PRJ-hilda_data/Loneliness trends/logfiles/interactions 20240223.smcl", replace

//##################################################################################//
// 2. Load and finalise data
//----------------------------------------------------------------------------------//

// 2.1. Set working directory
cd "Y:/PRJ-hilda_data/Loneliness trends"

// 2.2. Load analysis data
use "Y:/PRJ-hilda_data/Loneliness trends/Data/combined data.dta", clear

// 2.3. Create log-time for models
gen lnwave=ln(wave)

// 2.4. Recode variables so 0 is referent
recode cob 1=0 2=1 3=2
label define AANBCOB 0 "Australia" 1 "Main English Speaking" 2 "Other", modify

// 2.5. Set the survey structure to ensure correct SEs
svyset xhhraid, strata(xhhstrat) || xwaveid || _n, weight(weight)

//##################################################################################//
// 3. Estimate mean loneliness over time by subpopulation group
//----------------------------------------------------------------------------------//

// 4.1. Calculate means

matrix pta=J(3,8,.)
matrix pva=J(3,4,.)

local x=0
foreach i in sex language living {

	local x=`x'+1
	svy: melogit lonely c.wave##c.wave##c.wave##i.`i' || xhhraid: || xwaveid:, intp(15)
		
	test 2.`i'#wave 2.`i'#wave#wave 2.`i'#wave#wave#wave
	matrix pva[`x',1]=r(p)
	
	svy: melogit lonely_chronic c.wave##c.wave##i.`i' if wave>1 || xhhraid: || xwaveid:, intp(15)
	
	test 2.`i'#wave 2.`i'#wave#wave
	matrix pva[`x',2]=r(p)
	
	svy: melogit support c.wave##c.wave##i.`i' || xhhraid: || xwaveid:, intp(15)
		
	test 2.`i'#wave 2.`i'#wave#wave
	matrix pva[`x',3]=r(p)
	
	svy: melogit support_chronic c.wave##c.wave##i.`i' if wave>1 || xhhraid: || xwaveid:, intp(15)
		
	test 2.`i'#wave 2.`i'#wave#wave
	matrix pva[`x',4]=r(p)
	
}

matrix ptb=J(7,12,.)
matrix pvb=J(7,4,.)

local x=0
foreach i in i.agecat i.educ i.cob  i.marital  i.employ i.aria i.seifa {

	local x=`x'+1
	
	svy: melogit lonely c.wave##c.wave##c.wave##i.`i' || xhhraid: || xwaveid:, intp(15)
	
	test 1.`i'#wave 1.`i'#wave#wave 1.`i'#wave#wave#wave 2.`i'#wave 2.`i'#wave#wave 2.`i'#wave#wave#wave
	matrix pvb[`x',1]=r(p)
	
	svy: melogit lonely_chronic c.wave##c.wave##i.`i' if wave>1 || xhhraid: || xwaveid:, intp(15)
	
	test 1.`i'#wave 1.`i'#wave#wave 2.`i'#wave 2.`i'#wave#wave
	matrix pvb[`x',2]=r(p)
	
	svy: melogit support c.wave##c.wave##i.`i' || xhhraid: || xwaveid:, intp(15)
	
	test 1.`i'#wave 1.`i'#wave#wave 2.`i'#wave 2.`i'#wave#wave
	matrix pvb[`x',3]=r(p)
	
	svy: melogit support_chronic c.wave##c.wave##i.`i' if wave>1 || xhhraid: || xwaveid:, intp(15)
	
	test 1.`i'#wave 1.`i'#wave#wave 2.`i'#wave 2.`i'#wave#wave
	matrix pvb[`x',4]=r(p)
	
}

matrix pv=pva \ pvb

matrix list pv

log close
