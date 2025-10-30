/****************************************************************************/
//
// Syntax File 5
// Physical Activity Latent Class Analysis
// Latent class model fit
// Date: 6 October 2022
// Author: Philip J Clare
//
/****************************************************************************/
// 1. Load data and start log
//----------------------------------------------------------------------------

log using "Y:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Results/model fit 20221011.smcl", replace

use "Y:/PRJ-prc_alswh/Paper 0 - Latent Class Analysis/Data/primary analysis data.dta", clear

/****************************************************************************/
// 2. Estimate latent class models on complete data and generate fit statistics
//----------------------------------------------------------------------------

mi extract 0

gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class 1) nolog emopts(iterate(100))
estat ic
disp -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
estat lcprob

gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class 2) nolog emopts(iterate(100))
estat ic
disp -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
estat lcprob

gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class 3) nolog emopts(iterate(100))
estat ic
disp -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
estat lcprob

gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class 4) nolog emopts(iterate(100))
estat ic
disp -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
estat lcprob

gsem (activity_bin3 activity_bin4 activity_bin5 activity_bin6 activity_bin7 activity_bin8 <- ), logit lclass(class 5) nolog emopts(iterate(100))
estat ic
disp -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
estat lcprob

log close