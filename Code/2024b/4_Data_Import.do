/****************************************************************************/
// 
// Syntax File 4
// Physical Activity Latent Class Analysis
// Import data into Stata and register as MI data to work with mi estimate
// Date: 6 October 2022
// Author: Philip J Clare
//
/****************************************************************************/
// 1. Load primary data and import as MI
//----------------------------------------------------------------------------

use "R:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary imputed data.dta", clear

mi import flong, m(imp) id(idproj) imputed(pcsa9 mcsa9 gh9 re9 rp9 vt9 mh9 bp9 sf9 pf9 b_bmigp b_live_u18 b_live_o18 b_cesd b_smokst b_stress b_employ b_cobcat b_pcsa b_mcsa b_educ b_gh b_marital b_seifadis b_ariapgp b_re b_mh b_rp b_vt b_sf b_bp b_age b_pf b_cancer_ever b_depression_ever b_anxiety_ever b_alcliferisk b_alcepisrisk b_alcrisk activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8) clear

save "R:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary analysis data.dta", replace

/****************************************************************************/
// 2. Load sensitivity data and import as MI
//----------------------------------------------------------------------------

use "R:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/sensitivity imputed data.dta", clear

mi import flong, m(imp) id(idproj) imputed(pcsa9 mcsa9 gh9 re9 rp9 vt9 mh9 bp9 sf9 pf9 b_bmigp b_live_u18 b_live_o18 b_cesd b_smokst b_stress b_employ b_cobcat b_pcsa b_mcsa b_educ b_gh b_marital b_seifadis b_ariapgp b_re b_mh b_rp b_vt b_sf b_bp b_age b_pf b_cancer_ever b_depression_ever b_anxiety_ever b_alcliferisk b_alcepisrisk b_alcrisk activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8) clear

save "R:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/sensitivity analysis data.dta", replace