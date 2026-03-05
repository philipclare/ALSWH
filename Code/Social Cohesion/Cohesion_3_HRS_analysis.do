
global workdir="C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Social cohesion"

use "$workdir/Data for analysis/hrs_longitudinal_combined.dta", clear

drop if weights==.

bysort id: egen sum_w = total(weights)
bysort id: gen group_n=_N
gen weight_a = weights * (group_n / sum_w)

encode id, gen(id2)
drop id
rename id2 id
order id, before(wave)

foreach i in lb020a lb020b lb020c lb020e lb020g {
	replace `i'=8-`i' 
}

egen cohesion=rowmean(lb020a lb020c lb020e lb020g)

xtset id wave

regress cohesion l1.wave [pweight=weight_a]
predict t_hat_num1
gen t_sigma_num1 = e(rmse) // get standard deviation of residuals
gen gps_num1 = normalden(cohesion, t_hat_num1, t_sigma_num1)

regress cohesion l1.wave l1.household_size l1.age l1.sex l1.mstat l1.total_children l1.edu l1.work_status l1.health l1.ht l1.db l1.heart_stroke l1.arth l1.psych l1.cancer  [pweight=weight_a]
predict t_hat_den1
gen t_sigma_den1 = e(rmse) // get standard deviation of residuals
gen gps_den1 = normalden(cohesion, t_hat_den1, t_sigma_den1)

gen wt1=gps_num1/gps_den1
replace wt1=wt1*weight_a

regress lb020b l1.wave [pweight=weight_a]
predict t_hat_num2
gen t_sigma_num2 = e(rmse) // get standard deviation of residuals
gen gps_num2 = normalden(lb020b, t_hat_num2, t_sigma_num2)

regress lb020b l1.wave l1.household_size l1.age l1.sex l1.mstat l1.total_children l1.edu l1.work_status l1.health l1.ht l1.db l1.heart_stroke l1.arth l1.psych l1.cancer  [pweight=weight_a]
predict t_hat_den2
gen t_sigma_den2 = e(rmse) // get standard deviation of residuals
gen gps_den2 = normalden(lb020b, t_hat_den2, t_sigma_den2)

gen wt2=gps_num2/gps_den2
replace wt2=wt2*weight_a

regress lb020f l1.wave [pweight=weight_a]
predict t_hat_num3
gen t_sigma_num3 = e(rmse) // get standard deviation of residuals
gen gps_num3 = normalden(lb020f, t_hat_num3, t_sigma_num3)

regress lb020f l1.wave l1.household_size l1.age l1.sex l1.mstat l1.total_children l1.edu l1.work_status l1.health l1.ht l1.db l1.heart_stroke l1.arth l1.psych l1.cancer  [pweight=weight_a]
predict t_hat_den3
gen t_sigma_den3 = e(rmse) // get standard deviation of residuals
gen gps_den3 = normalden(lb020f, t_hat_den3, t_sigma_den3)

gen wt3=gps_num3/gps_den3
replace wt3=wt3*weight_a

gen lncohesion=ln(cohesion)
gen lnlb020b=ln(lb020b)
gen lnlb020f=ln(lb020f)

gen flonely=f1.lonely_dich

qui melogit flonely c.cohesion [pweight=wt1] || id:
estat ic
qui melogit flonely c.cohesion##c.cohesion [pweight=wt1] || id:
estat ic
qui melogit flonely c.cohesion##c.cohesion##c.cohesion [pweight=wt1] || id:
estat ic
qui melogit flonely c.cohesion c.lncohesion [pweight=wt1] || id: //*******************
estat ic

qui melogit flonely c.lb020b [pweight=wt2] || id: //*******************
estat ic
qui melogit flonely c.lb020b##c.lb020b [pweight=wt2] || id:
estat ic
qui melogit flonely c.lb020b##c.lb020b##c.lb020b [pweight=wt2] || id:
estat ic
qui melogit flonely c.lb020b c.lnlb020b [pweight=wt2] || id:
estat ic

qui melogit flonely c.lb020f [pweight=wt3] || id: //*******************
estat ic
qui melogit flonely c.lb020f##c.lb020f [pweight=wt3] || id:
estat ic
qui melogit flonely c.lb020f##c.lb020f##c.lb020f [pweight=wt3] || id:
estat ic
qui melogit flonely c.lb020f c.lnlb020f [pweight=wt3] || id:
estat ic

melogit flonely c.cohesion c.lncohesion [pweight=wt1] || id:
margins , at(cohesion=1 lncohesion=0.000000) at(cohesion=2 lncohesion=0.693147)  at(cohesion=3 lncohesion=1.098612) at(cohesion=4 lncohesion=1.386294) at(cohesion=5 lncohesion=1.609438) at(cohesion=6 lncohesion=1.791759) at(cohesion=7 lncohesion=1.945910) post
nlcom (rr_aveg: ((_b[2._at] / _b[1._at]) + (_b[3._at] / _b[2._at]) + (_b[4._at] / _b[3._at]) + (_b[5._at] / _b[4._at]) + (_b[6._at] / _b[5._at]) + (_b[7._at] / _b[6._at]))/6)
matrix res_coh=r(table)'
melogit flonely c.lb020b [pweight=wt2] || id:
margins , at(lb020b=(1 2 3 4 5 6 7)) post
nlcom (rr_aveg: ((_b[2._at] / _b[1._at]) + (_b[3._at] / _b[2._at]) + (_b[4._at] / _b[3._at]) + (_b[5._at] / _b[4._at]) + (_b[6._at] / _b[5._at]) + (_b[7._at] / _b[6._at]))/6)
matrix res_rub=r(table)'
melogit flonely c.lb020f [pweight=wt3] || id:
margins , at(lb020f=(1 2 3 4 5 6 7)) post
nlcom (rr_aveg: ((_b[2._at] / _b[1._at]) + (_b[3._at] / _b[2._at]) + (_b[4._at] / _b[3._at]) + (_b[5._at] / _b[4._at]) + (_b[6._at] / _b[5._at]) + (_b[7._at] / _b[6._at]))/6)
matrix res_van=r(table)'

putexcel set "$workdir/Results/HRS results.xlsx", sheet(cohesion) modify 
putexcel A1=matrix(res_coh), names
putexcel A1="exp_level"
putexcel set "$workdir/Results/HRS results.xlsx", sheet(rubbish) modify
putexcel A1=matrix(res_rub), names
putexcel A1="exp_level"
putexcel set "$workdir/Results/HRS results.xlsx", sheet(vandalism) modify
putexcel A1=matrix(res_van), names
putexcel A1="exp_level"
