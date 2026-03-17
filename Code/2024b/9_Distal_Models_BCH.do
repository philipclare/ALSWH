/****************************************************************************/
//
// Syntax File 9
// Physical Activity Latent Class Analysis
// Run latent class regresion and regression on distal outcomes using BCH
// Date: 10 October 2022
// Author: Philip J Clare
//
/****************************************************************************/
// 1. Load data and start log
//----------------------------------------------------------------------------

log using "Y:\PRJ-prc_alswh\Paper 0 - Latent Class Analysis\Results\distal regressions 20221011.smcl", replace

use "Y:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary analysis data - with weights.dta", clear

/****************************************************************************/
// 2. Latent class regression on baseline covariates
//----------------------------------------------------------------------------

mi estimate, dots esampvaryok errorok: mlogit modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age c.b_seifadis i.b_marital i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj) base(1)

/****************************************************************************/
// 3. Run regression on primary distal outcomes
//----------------------------------------------------------------------------

mi estimate, dots esampvaryok errorok: regress pcsa9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age c.b_seifadis i.b_marital i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress mcsa9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age c.b_seifadis i.b_marital i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

/****************************************************************************/
// 4. Run regression on secondary distal outcomes
//----------------------------------------------------------------------------

mi estimate, dots esampvaryok errorok: regress pf9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress rp9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress bp9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress gh9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress vt9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress sf9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress re9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

mi estimate, dots esampvaryok errorok: regress mh9 i.modclass i.b_alcliferisk i.b_alcepisrisk i.b_smokst ///
c.b_pcsa c.b_mcsa ib2.b_bmigp c.b_cesd c.b_stress i.b_cancer_ever i.b_depression_ever i.b_anxiety_ever ///
c.b_age i.b_marital c.b_seifadis i.b_ariapgp i.b_employ i.b_cobcat i.b_educ i.b_live_u18 i.b_live_o18 ///
[iweight=bchw], vce(cluster idproj)

log close
