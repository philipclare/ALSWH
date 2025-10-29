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

log using "Y:/PRJ-hilda_data/Loneliness trends/logfiles/modeled trends 20250821.smcl", replace

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

gen lonely_sens_chronic=0 if (lonely_sens==0 | l1.lonely_sens==0) & lonely_sens!=. & l1.lonely_sens!=.
replace lonely_sens_chronic=1 if lonely_sens==1 & l1.lonely_sens==1

gen lonely_chronic_3=0 if (lonely==0 | l1.lonely==0 | l2.lonely==0) & lonely!=. & l1.lonely!=.
replace lonely_chronic_3=1 if lonely==1 & l1.lonely==1 & l2.lonely==1
gen support_chronic_3=0 if (support==0 | l1.support==0 | l2.support==0) & support!=. & l1.support!=.
replace support_chronic_3=1 if support==1 & l1.support==1 & l2.support==1

// 2.4. Set the survey structure to ensure correct SEs
svyset xhhraid, strata(xhhstrat) || xwaveid || _n, weight(weight)

// 2.5 Create local macro with labels
local row_names "age_16-29_e" "age_16-29_ll" "age_16-29_ul" "age_30-64_e" "age_30-64_ll" "age_30-64_ul" "age_65+_e" "age_65+_ll" "age_65+_ul" "sex_male_e" "sex_male_ll" "sex_male_ul" "sex_female_e" "sex_female_ll" "sex_female_ul" "educ_hs_e" "educ_hs_ll" "educ_hs_ul" "educ_trade_e" "educ_trade_ll" "educ_trade_ul" "educ_uni_e" "educ_uni_ll" "educ_uni_ul" "cob_aus_e" "cob_aus_ll" "cob_aus_ul" "cob_eng_e" "cob_eng_ll" "cob_eng_" "cob_noneng_e" "cob_noneng_ll" "cob_noneng_ul" "lang_eng_e" "lang_eng_ll" "lang_eng_ul" "lang_noneng_e" "lang_noneng_ll" "lang_noneng_ul" "marital_part_e" "marital_part_ll" "marital_part_ul" "marital_single_e" "marital_single_ll" "marital_single_ul" "marital_widowed_e" "marital_widowed_ll" "marital_widowed_ul" "living_alone_e" "living_alone_ll" "living_alone_ul" "living_others_e" "living_others_ll" "living_others_ul" "emp_employed_e" "emp_employed_ll" "emp_employed_ul" "emp_unempl_e" "emp_unempl_ll" "emp_unempl_ul" "emp_niw_e" "emp_niw_ll" "emp_niw_ul" "aria_city_e" "aria_city_ll" "aria_city_ul" "aria_reg1_e" "aria_reg1_ll" "aria_reg1_ul" "aria_other_e" "aria_other_ll" "aria_other_ul" "seifa_bottom3_e" "seifa_bottom3_ll" "seifa_bottom3_ul" "seifa_mid4_e" "seifa_mid4_ll" "seifa_mid4_ul" "seifa_top3_e" "seifa_top3_ll" "seifa_top3_ul"
local years1 "2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023"
local years2 "2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023"

//##################################################################################//
// 3. Estimate mean loneliness over time
//----------------------------------------------------------------------------------//

matrix p=J(1,4,.)
// 3.1. Calculate means
svy: melogit lonely c.wave##c.wave##c.wave || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave c.wave#c.wave#c.wave
matrix p[1,1]=r(p)
margins, at(wave=(1/23)) predict(mu) post
	matrix r1=r(table)
	matrix r1=r1[1,1..23]\r1[5..6,1..23]
	
svy: melogit lonely_chronic c.wave##c.wave if wave>1 || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave
matrix p[1,2]=r(p)
margins, at(wave=(2/23)) predict(mu) post
	matrix r2=r(table)
	matrix r2=r2[1,1..22]\r2[5..6,1..22]
	
svy: melogit support c.wave##c.wave || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave
matrix p[1,3]=r(p)
margins, at(wave=(1/23)) predict(mu) post
	matrix r3=r(table)
	matrix r3=r3[1,1..23]\r3[5..6,1..23]
	
svy: melogit support_chronic c.wave##c.wave if wave>1 || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave
matrix p[1,4]=r(p)
margins, at(wave=(2/23)) predict(mu) post
	matrix r4=r(table)
	matrix r4=r4[1,1..22]\r4[5..6,1..22]

// 3.2. Save results to excel
putexcel set "`workdir'/Results/Raw output/modeled.xlsx", sheet(lonely) modify
putexcel A1 = matrix(r1), names
matrix list r1
putexcel set "`workdir'/Results/Raw output/modeled.xlsx", sheet(lonely_chronic) modify
putexcel A1 = matrix(r2), names
matrix list r2
putexcel set "`workdir'/Results/Raw output/modeled.xlsx", sheet(support) modify
putexcel A1 = matrix(r3), names
matrix list r3
putexcel set "`workdir'/Results/Raw output/modeled.xlsx", sheet(support_chronic) modify
putexcel A1 = matrix(r4), names
matrix list r4	

//##################################################################################//
// 4. Estimate mean loneliness over time by subpopulation group
//----------------------------------------------------------------------------------//

// 4.1. Calculate means
xi i.agecat i.sex i.educ i.cob i.language i.marital i.living i.employ i.aria i.seifa, noomit

foreach i in _Iagecat_0 _Iagecat_1 _Iagecat_2 _Isex_1 _Isex_2 _Ieduc_0 _Ieduc_1 _Ieduc_2 _Icob_1 _Icob_2 _Icob_3 _Ilanguage_1 _Ilanguage_2 _Imarital_0 _Imarital_1 _Imarital_2 _Iliving_1 _Iliving_2 _Iemploy_0 _Iemploy_1 _Iemploy_2 _Iaria_0 _Iaria_1 _Iaria_2 _Iseifa_0 _Iseifa_1 _Iseifa_2 {

	matrix `i'p=J(1,4,.)
	
	svy, subpop(`i'): melogit lonely c.wave##c.wave##c.wave || xhhraid: || xwaveid:, intp(15)
	test c.wave c.wave#c.wave c.wave#c.wave#c.wave
	matrix `i'p[1,1]=r(p)
	margins, at(wave=(1/23)) predict(mu) post
		
		matrix `i'r1=r(table)
		matrix `i'r1=`i'r1[1,1..23] \ `i'r1[5..6,1..23]
		
	svy, subpop(`i'): melogit lonely_chronic c.wave##c.wave if wave>1 || xhhraid: || xwaveid:, intp(15)
	test c.wave c.wave#c.wave
	matrix `i'p[1,2]=r(p)
	margins, at(wave=(2/23)) predict(mu) post
		
		matrix `i'r2=r(table)
		matrix `i'r2=`i'r2[1,1..22] \ `i'r2[5..6,1..22]
		
	svy, subpop(`i'): melogit support c.wave##c.wave || xhhraid: || xwaveid:, intp(15)
	test c.wave c.wave#c.wave
	matrix `i'p[1,3]=r(p)
	margins, at(wave=(1/23)) predict(mu) post
		matrix `i'r3=r(table)
		matrix `i'r3=`i'r3[1,1..23] \ `i'r3[5..6,1..23]
		
	svy, subpop(`i'): melogit support_chronic c.wave##c.wave if wave>1 || xhhraid: || xwaveid:, intp(15)
	test c.wave c.wave#c.wave
	matrix `i'p[1,4]=r(p)
	margins, at(wave=(2/23)) predict(mu) post
		matrix `i'r4=r(table)
		matrix `i'r4=`i'r4[1,1..22] \ `i'r4[5..6,1..22]
		
}

// 4.2. Save results to excel
matrix r5=_Iagecat_0r1 \ _Iagecat_1r1 \ _Iagecat_2r1 \ _Isex_1r1 \ _Isex_2r1 \ _Ieduc_0r1 \ _Ieduc_1r1 \ _Ieduc_2r1 \ _Icob_1r1 \ _Icob_2r1 \ _Icob_3r1 \ _Ilanguage_1r1 \ _Ilanguage_2r1 \ _Imarital_0r1 \ _Imarital_1r1 \ _Imarital_2r1 \ _Iliving_1r1 \ _Iliving_2r1 \ _Iemploy_0r1 \ _Iemploy_1r1 \ _Iemploy_2r1 \ _Iaria_0r1 \ _Iaria_1r1 \ _Iaria_2r1 \ _Iseifa_0r1 \ _Iseifa_1r1 \ _Iseifa_2r1

matrix rownames r5 = `row_names'
matrix colnames r5 = `years1'

matrix r6=_Iagecat_0r2 \ _Iagecat_1r2 \ _Iagecat_2r2 \ _Isex_1r2 \ _Isex_1r2 \ _Ieduc_0r2 \ _Ieduc_1r2 \ _Ieduc_2r2 \ _Icob_1r2 \ _Icob_2r2 \ _Icob_3r2 \ _Ilanguage_1r2 \ _Ilanguage_2r2 \ _Imarital_0r2 \ _Imarital_1r2 \ _Imarital_2r2 \ _Iliving_1r2 \ _Iliving_2r2 \ _Iemploy_0r2 \ _Iemploy_1r2 \ _Iemploy_2r2 \ _Iaria_0r2 \ _Iaria_1r2 \ _Iaria_2r2 \ _Iseifa_0r2 \ _Iseifa_1r2 \ _Iseifa_2r2

matrix rownames r6 = `row_names'
matrix colnames r6 = `years2'

matrix r7=_Iagecat_0r3 \ _Iagecat_1r3 \ _Iagecat_2r3 \ _Isex_1r3 \ _Isex_2r3 \ _Ieduc_0r3 \ _Ieduc_1r3 \ _Ieduc_2r3 \ _Icob_1r3 \ _Icob_2r3 \ _Icob_3r3 \ _Ilanguage_1r3 \ _Ilanguage_2r3 \ _Imarital_0r3 \ _Imarital_1r3 \ _Imarital_2r3 \ _Iliving_1r3 \ _Iliving_2r3 \ _Iemploy_0r3 \ _Iemploy_1r3 \ _Iemploy_2r3 \ _Iaria_0r3 \ _Iaria_1r3 \ _Iaria_2r3 \ _Iseifa_0r3 \ _Iseifa_1r3 \ _Iseifa_2r3

matrix rownames r7 = `row_names'
matrix colnames r7 = `years1'

matrix r8=_Iagecat_0r4 \ _Iagecat_1r4 \ _Iagecat_2r4 \ _Isex_1r4 \ _Isex_2r4 \ _Ieduc_0r4 \ _Ieduc_1r4 \ _Ieduc_2r4 \ _Icob_1r4 \ _Icob_2r4 \ _Icob_3r4 \ _Ilanguage_1r4 \ _Ilanguage_2r4 \ _Imarital_0r4 \ _Imarital_1r4 \ _Imarital_2r4 \ _Iliving_1r4 \ _Iliving_2r4 \ _Iemploy_0r4 \ _Iemploy_1r4 \ _Iemploy_2r4 \ _Iaria_0r4 \ _Iaria_1r4 \ _Iaria_2r4 \ _Iseifa_0r4 \ _Iseifa_1r4 \ _Iseifa_2r4

matrix rownames r8 = `row_names'
matrix colnames r8 = `years2'

matrix p=_Iagecat_0p \ _Iagecat_1p \ _Iagecat_2p \ _Isex_1p \ _Isex_2p \ _Ieduc_0p \ _Ieduc_1p \ _Ieduc_2p \ _Icob_1p \ _Icob_2p \ _Icob_3p \ _Ilanguage_1p \ _Ilanguage_2p \ _Imarital_0p \ _Imarital_1p \ _Imarital_2p \ _Iliving_1p \ _Iliving_2p \ _Iemploy_0p \ _Iemploy_1p \ _Iemploy_2p \ _Iaria_0p \ _Iaria_1p \ _Iaria_2p \ _Iseifa_0p \ _Iseifa_1p \ _Iseifa_2p

matrix rownames p = "agecat0" "agecat1" "agecat2" "sex1" "sex2" "educ0" "educ1" "educ2" "cob1" "cob2" "cob3" "language1" "language2" "marital0" "marital1" "marital2" "living1" "living2" "employ0" "employ1" "employ2" "aria0" "aria1" "aria2" "seifa0" "seifa1" "seifa2"
matrix colnames p = "lonely_point" "lonely_chronic" "isolation_point" "isolation_chronic"   

putexcel set "`workdir'/Results/Raw output/modeled_bydemog.xlsx", sheet(lonely) modify
putexcel A1 = matrix(r5), names
matrix list r5

putexcel set "`workdir'/Results/Raw output/modeled_bydemog.xlsx", sheet(lonely_chronic) modify
putexcel A1 = matrix(r6), names
matrix list r6

putexcel set "`workdir'/Results/Raw output/modeled_bydemog.xlsx", sheet(support) modify
putexcel A1 = matrix(r7), names
matrix list r7

putexcel set "`workdir'/Results/Raw output/modeled_bydemog.xlsx", sheet(support_chronic) modify
putexcel A1 = matrix(r8), names
matrix list r8

putexcel set "`workdir'/Results/Raw output/modeled_bydemog.xlsx", sheet(pvalues) modify
putexcel A1 = matrix(p), names
matrix list p

//##################################################################################//
// 5. Sensitivity analysis of single-item loneliness
//----------------------------------------------------------------------------------//

// 5.1. Calculate means
svy: melogit lonely_sens c.wave##c.wave || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave
margins, at(wave=(1/23)) predict(mu) post
	matrix s1=r(table)
	matrix s1=s1[1,1..23]\s1[5..6,1..23]
	
svy: melogit lonely_sens_chronic c.wave##c.wave if wave>1 || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave
margins, at(wave=(2/23)) predict(mu) post
	matrix s2=r(table)
	matrix s2=s2[1,1..22]\s2[5..6,1..22]

// 5.2. Save results to excel
putexcel set "`workdir'/Results/Raw output/modeled_sensitivity.xlsx", sheet(lonely_sens) modify
putexcel A1 = matrix(s1), names
matrix list s1
putexcel set "`workdir'/Results/Raw output/modeled_sensitivity.xlsx", sheet(lonely_sens_chronic) modify
putexcel A1 = matrix(s2), names
matrix list s2	

//##################################################################################//
// 6. Sensitivity analysis of 3-wave chronic loneliness/isolation
//----------------------------------------------------------------------------------//

// 6.1. Calculate means
svy: melogit lonely_chronic_3 c.wave##c.wave if wave>2 || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave
margins, at(wave=(3/23)) predict(mu) post
	matrix s3=r(table)
	matrix s3=s3[1,1..21]\s3[5..6,1..21]
	
svy: melogit support_chronic_3 c.wave##c.wave if wave>2 || xhhraid: || xwaveid:, intp(15)
test c.wave c.wave#c.wave
margins, at(wave=(3/23)) predict(mu) post
	matrix s4=r(table)
	matrix s4=s4[1,1..21]\s4[5..6,1..21]

// 6.2. Save results to excel
putexcel set "`workdir'/Results/Raw output/modeled_sensitivity.xlsx", sheet(lonely_chronic_3) modify
putexcel A1 = matrix(s3), names
matrix list s3
putexcel set "`workdir'/Results/Raw output/modeled_sensitivity.xlsx", sheet(support_chronic_3) modify
putexcel A1 = matrix(s4), names
matrix list s4	

log close
