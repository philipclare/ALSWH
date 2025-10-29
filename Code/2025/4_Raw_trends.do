//##################################################################################//
//   
// Project: Trends in loneliness and isolation in Australia
// Program: C4 - Raw Trends
// Purpose: Describe raw trends in loneliness
// Author: Philip Clare
// Date: 24 October 2023
// OSF Registration: https://doi.org/10.17605/OSF.IO/CPTZF
//
//##################################################################################//
// 1. Start log file
//----------------------------------------------------------------------------------//

log using "Y:/PRJ-hilda_data/Loneliness trends/logfiles/raw trends 20230919.smcl", replace

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

// 2.4. Set the survey structure to ensure correct SEs
svyset xhhraid, strata(xhhstrat) || xwaveid || _n, weight(weight)

// 2.5 Create local macro with labels
local rows "age_16-29 age_30-64 age_65+ sex_male sex_female educ_hs educ_trade educ_uni cob_aus cob_eng cob_noneng lang_eng lang_noneng marital_part marital_single marital_widowed living_alone living_others emp_employed emp_unempl emp_niw aria_city aria_reg1 aria_other seifa_bottom3 seifa_mid4 seifa_top3"
local years1 "2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023"
local years2 "2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023"

//##################################################################################//
// 3. Estimate mean loneliness over time
//----------------------------------------------------------------------------------//

// 3.1. Calculate means
svy: mean lonely, over(wave)
	matrix r1=r(table)
	matrix r1=r1[1,1..23]
	matrix colnames r1 = `years1'

svy: mean lonely_chronic if wave>1, over(wave)
	matrix r2=r(table)
	matrix r2=r2[1,1..22]
	matrix colnames r2 = `years2'

svy: mean support, over(wave)
	matrix r3=r(table)
	matrix r3=r3[1,1..23]
	matrix colnames r3 = `years1'

svy: mean support_chronic if wave>1, over(wave)
	matrix r4=r(table)
	matrix r4=r4[1,1..22]
	matrix colnames r4 = `years2'

// 3.2. Save results to excel
putexcel set "`workdir'/Results/Raw output/raw.xlsx", sheet(lonely) modify
putexcel A1 = matrix(r1), names
matrix list r1	
putexcel set "`workdir'/Results/Raw output/raw.xlsx", sheet(lonely_chronic) modify
putexcel A1 = matrix(r2), names
matrix list r2
putexcel set "`workdir'/Results/Raw output/raw.xlsx", sheet(support) modify
putexcel A1 = matrix(r3), names
matrix list r3
putexcel set "`workdir'/Results/Raw output/raw.xlsx", sheet(support_chronic) modify
putexcel A1 = matrix(r4), names
matrix list r4

//##################################################################################//
// 4. Estimate mean loneliness over time by subpopulation group
//----------------------------------------------------------------------------------//

// 4.1. Calculate means
xi i.agecat i.sex i.educ i.cob i.language i.marital i.living i.employ i.aria i.seifa, noomit

foreach i in _Iagecat_0 _Iagecat_1 _Iagecat_2 _Isex_1 _Isex_2 _Ieduc_0 _Ieduc_1 _Ieduc_2 _Icob_1 _Icob_2 _Icob_3 _Ilanguage_1 _Ilanguage_2 _Imarital_0 _Imarital_1 _Imarital_2 _Iliving_1 _Iliving_2 _Iemploy_0 _Iemploy_1 _Iemploy_2 _Iaria_0 _Iaria_1 _Iaria_2 _Iseifa_0 _Iseifa_1 _Iseifa_2 {

	svy, subpop(`i'): mean lonely, over(wave)
		matrix `i'r1=r(table)
		matrix `i'r1=`i'r1[1,1..23]
	
	svy, subpop(`i' if wave>1): mean lonely_chronic, over(wave)
		matrix `i'r2=r(table)
		matrix `i'r2=`i'r2[1,1..22]
		
	svy, subpop(`i'): mean support, over(wave)
		matrix `i'r3=r(table)
		matrix `i'r3=`i'r3[1,1..23]
		
	svy, subpop(`i' if wave>1): mean support_chronic, over(wave)
		matrix `i'r4=r(table)
		matrix `i'r4=`i'r4[1,1..22]
}

// 4.2. Save results to excel
matrix r5=_Iagecat_0r1 \ _Iagecat_1r1 \ _Iagecat_2r1 \ _Isex_1r1 \ _Isex_2r1 \ _Ieduc_0r1 \ _Ieduc_1r1 \ _Ieduc_2r1 \ _Icob_1r1 \ _Icob_2r1 \ _Icob_3r1 \ _Ilanguage_1r1 \ _Ilanguage_2r1 \ _Imarital_0r1 \ _Imarital_1r1 \ _Imarital_2r1 \ _Iliving_1r1 \ _Iliving_2r1 \ _Iemploy_0r1 \ _Iemploy_1r1 \ _Iemploy_2r1 \ _Iaria_0r1 \ _Iaria_1r1 \ _Iaria_2r1 \ _Iseifa_0r1 \ _Iseifa_1r1 \ _Iseifa_2r1

matrix rownames r5 = `rows'
matrix colnames r5 = `years1'

matrix r6=_Iagecat_0r2 \ _Iagecat_1r2 \ _Iagecat_2r2 \ _Isex_1r2 \ _Isex_1r2 \ _Ieduc_0r2 \ _Ieduc_1r2 \ _Ieduc_2r2 \ _Icob_1r2 \ _Icob_2r2 \ _Icob_3r2 \ _Ilanguage_1r2 \ _Ilanguage_2r2 \ _Imarital_0r2 \ _Imarital_1r2 \ _Imarital_2r2 \ _Iliving_1r2 \ _Iliving_2r2 \ _Iemploy_0r2 \ _Iemploy_1r2 \ _Iemploy_2r2 \ _Iaria_0r2 \ _Iaria_1r2 \ _Iaria_2r2 \ _Iseifa_0r2 \ _Iseifa_1r2 \ _Iseifa_2r2

matrix rownames r6 = `rows'
matrix colnames r6 = `years2'

matrix r7=_Iagecat_0r3 \ _Iagecat_1r3 \ _Iagecat_2r3 \ _Isex_1r3 \ _Isex_2r3 \ _Ieduc_0r3 \ _Ieduc_1r3 \ _Ieduc_2r3 \ _Icob_1r3 \ _Icob_2r3 \ _Icob_3r3 \ _Ilanguage_1r3 \ _Ilanguage_2r3 \ _Imarital_0r3 \ _Imarital_1r3 \ _Imarital_2r3 \ _Iliving_1r3 \ _Iliving_2r3 \ _Iemploy_0r3 \ _Iemploy_1r3 \ _Iemploy_2r3 \ _Iaria_0r3 \ _Iaria_1r3 \ _Iaria_2r3 \ _Iseifa_0r3 \ _Iseifa_1r3 \ _Iseifa_2r3

matrix rownames r7 = `rows'
matrix colnames r7 = `years1'

matrix r8=_Iagecat_0r4 \ _Iagecat_1r4 \ _Iagecat_2r4 \ _Isex_1r4 \ _Isex_2r4 \ _Ieduc_0r4 \ _Ieduc_1r4 \ _Ieduc_2r4 \ _Icob_1r4 \ _Icob_2r4 \ _Icob_3r4 \ _Ilanguage_1r4 \ _Ilanguage_2r4 \ _Imarital_0r4 \ _Imarital_1r4 \ _Imarital_2r4 \ _Iliving_1r4 \ _Iliving_2r4 \ _Iemploy_0r4 \ _Iemploy_1r4 \ _Iemploy_2r4 \ _Iaria_0r4 \ _Iaria_1r4 \ _Iaria_2r4 \ _Iseifa_0r4 \ _Iseifa_1r4 \ _Iseifa_2r4

matrix rownames r8 = `rows'
matrix colnames r8 = `years2'

putexcel set "`workdir'/Results/Raw output/raw_bydemog.xlsx", sheet(lonely) modify
putexcel A1 = matrix(r5), names
matrix list r5

putexcel set "`workdir'/Results/Raw output/raw_bydemog.xlsx", sheet(lonely_chronic) modify
putexcel A1 = matrix(r6), names
matrix list r6

putexcel set "`workdir'/Results/Raw output/raw_bydemog.xlsx", sheet(support) modify
putexcel A1 = matrix(r7), names
matrix list r7

putexcel set "`workdir'/Results/Raw output/raw_bydemog.xlsx", sheet(support_chronic) modify
putexcel A1 = matrix(r8), names
matrix list r8

log close
