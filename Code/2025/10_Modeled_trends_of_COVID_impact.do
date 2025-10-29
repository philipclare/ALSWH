//##################################################################################//
//   
// Project: Trends in loneliness and isolation in Australia
// Program: C5 - Modeled Trends
// Purpose: Estimate trends in loneliness using mixed effects models
// Author: Philip Clare
// Date: 24 October 2023
// OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
//
//##################################################################################//
// 1. Start log file
//----------------------------------------------------------------------------------//

log using "Y:/PRJ-hilda_data/Loneliness trends/logfiles/modeled trends covid 20231024.smcl", replace

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

gen post_covid=wave-19
replace post_covid=0 if post_covid<0

// 2.4. Set the survey structure to ensure correct SEs
svyset xhhraid, strata(xhhstrat) || xwaveid || _n, weight(weight)

//##################################################################################//
// 3. Estimate mean loneliness over time
//----------------------------------------------------------------------------------//

// 3.1. Calculate means
svy: melogit lonely c.wave##c.wave##c.wave c.post_covid##c.post_covid || xhhraid: || xwaveid:, intp(15)
test c.post_covid c.post_covid#c.post_covid
margins, at(wave=(1/23) post_covid=(0)) predict(mu)
	matrix r1a=r(table)
	matrix r1a=r1a[1,1..23]\r1a[5..6,1..23]
margins, at(wave=(20) post_covid=(1)) predict(mu)
	matrix r1b=r(table)
	matrix r1b=r1b[1,1]\r1b[5..6,1]
margins, at(wave=(21) post_covid=(2)) predict(mu)
	matrix r1c=r(table)
	matrix r1c=r1c[1,1]\r1c[5..6,1]
margins, at(wave=(22) post_covid=(3)) predict(mu)
	matrix r1d=r(table)
	matrix r1d=r1d[1,1]\r1d[5..6,1]
margins, at(wave=(23) post_covid=(4)) predict(mu)
	matrix r1e=r(table)
	matrix r1e=r1e[1,1]\r1e[5..6,1]
matrix r1 = r1a \ r1a[1..3,1..19] , r1b , r1c , r1d , r1e

svy: melogit lonely_chronic c.wave##c.wave c.post_covid##c.post_covid##c.post_covid if wave>1 || xhhraid: || xwaveid:, intp(15)
test c.post_covid c.post_covid#c.post_covid
margins, at(wave=(2/23) post_covid=(0)) predict(mu)
	matrix r2a=r(table)
	matrix r2a=r2a[1,1..22]\r2a[5..6,1..22]
margins, at(wave=(20) post_covid=(1)) predict(mu)
	matrix r2b=r(table)
	matrix r2b=r2b[1,1]\r2b[5..6,1]
margins, at(wave=(21) post_covid=(2)) predict(mu)
	matrix r2c=r(table)
	matrix r2c=r2c[1,1]\r2c[5..6,1]
margins, at(wave=(22) post_covid=(3)) predict(mu)
	matrix r2d=r(table)
	matrix r2d=r2d[1,1]\r2d[5..6,1]
margins, at(wave=(23) post_covid=(4)) predict(mu)
	matrix r2e=r(table)
	matrix r2e=r2e[1,1]\r2e[5..6,1]
matrix r2 = r2a \ r2a[1..3,1..18] , r2b , r2c , r2d , r2e
	
svy: melogit support c.wave##c.wave##c.wave c.post_covid##c.post_covid##c.post_covid || xhhraid: || xwaveid:, intp(15)
test c.post_covid
margins, at(wave=(1/23) post_covid=(0)) predict(mu)
	matrix r3a=r(table)
	matrix r3a=r3a[1,1..23]\r3a[5..6,1..23]
margins, at(wave=(20) post_covid=(1)) predict(mu)
	matrix r3b=r(table)
	matrix r3b=r3b[1,1]\r3b[5..6,1]
margins, at(wave=(21) post_covid=(2)) predict(mu)
	matrix r3c=r(table)
	matrix r3c=r3c[1,1]\r3c[5..6,1]
margins, at(wave=(22) post_covid=(3)) predict(mu)
	matrix r3d=r(table)
	matrix r3d=r3d[1,1]\r3d[5..6,1]
margins, at(wave=(23) post_covid=(4)) predict(mu)
	matrix r3e=r(table)
	matrix r3e=r3e[1,1]\r3e[5..6,1]
matrix r3 = r3a \ r3a[1..3,1..19] , r3b , r3c , r3d , r3e
	
svy: melogit support_chronic c.wave##c.wave##c.wave c.post_covid##c.post_covid##c.post_covid if wave>1 || xhhraid: || xwaveid:, intp(15)
test c.post_covid c.post_covid#c.post_covid
margins, at(wave=(2/23) post_covid=(0)) predict(mu)
	matrix r4a=r(table)
	matrix r4a=r4a[1,1..22]\r4a[5..6,1..22]
margins, at(wave=(20) post_covid=(1)) predict(mu)
	matrix r4b=r(table)
	matrix r4b=r4b[1,1]\r4b[5..6,1]
margins, at(wave=(21) post_covid=(2)) predict(mu)
	matrix r4c=r(table)
	matrix r4c=r4c[1,1]\r4c[5..6,1]
margins, at(wave=(22) post_covid=(3)) predict(mu)
	matrix r4d=r(table)
	matrix r4d=r4d[1,1]\r4d[5..6,1]
margins, at(wave=(23) post_covid=(4)) predict(mu)
	matrix r4e=r(table)
	matrix r4e=r4e[1,1]\r4e[5..6,1]
matrix r4 = r4a \ r4a[1..3,1..18] , r4b , r4c , r4d , r4e

// 3.2. Save results to excel
putexcel set "Y:/PRJ-hilda_data/Loneliness trends/Results/Raw output/modeled - covid interaction.xlsx", sheet(lonely) modify
putexcel A1 = matrix(r1), names
matrix list r1
putexcel set "Y:/PRJ-hilda_data/Loneliness trends/Results/Raw output/modeled - covid interaction.xlsx", sheet(lonely_chronic) modify
putexcel A1 = matrix(r2), names
matrix list r2
putexcel set "Y:/PRJ-hilda_data/Loneliness trends/Results/Raw output/modeled - covid interaction.xlsx", sheet(support) modify
putexcel A1 = matrix(r3), names
matrix list r3
putexcel set "Y:/PRJ-hilda_data/Loneliness trends/Results/Raw output/modeled - covid interaction.xlsx", sheet(support_chronic) modify
putexcel A1 = matrix(r4), names
matrix list r4	
