
global workdir="C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Social cohesion"

use "$workdir/Data for analysis/longitudinal_combined_hilda.dta", clear

bysort id: egen sum_w = total(weights)
bysort id: gen group_n=_N
gen weight_a = weights * (group_n / sum_w)

encode id, gen(id2)
drop id
rename id2 id
order id, before(wave)

foreach i in lslackn lslawhn lslatr lslanga lslansv lslarl lslavd {
	replace `i'=. if `i'<0
}

foreach i in lslackn lslawhn lslatr lslanga lslansv {
	tab `i'
}

foreach i in lslarl lslavd {
	tab `i'
}

foreach i in lslanga lslansv {
	replace `i'=8-`i' 
}

egen cohesion=rowmean(lslackn lslawhn lslatr lslanga lslansv)

xtset id wave

foreach i in ht db heart arth psych cancer {
	replace `i'=l1.`i' if `i'==. & l1.`i'!=.
}


regress cohesion l1.wave [pweight=weight_a]
predict t_hat_num1
gen t_sigma_num1 = e(rmse) // get standard deviation of residuals
gen gps_num1 = normalden(cohesion, t_hat_num1, t_sigma_num1)

regress cohesion l1.wave l1.household_size l1.age l1.sex l1.mstat l1.total_children l1.edu l1.work_status l1.health l1.ht l1.db l1.heart l1.arth l1.psych l1.cancer [pweight=weight_a]
predict t_hat_den1
gen t_sigma_den1 = e(rmse) // get standard deviation of residuals
gen gps_den1 = normalden(cohesion, t_hat_den1, t_sigma_den1)

gen wt1=gps_num1/gps_den1
replace wt1=wt1*weight_a

regress lslarl l1.wave [pweight=weight_a]
predict t_hat_num2
gen t_sigma_num2 = e(rmse) // get standard deviation of residuals
gen gps_num2 = normalden(lslarl, t_hat_num2, t_sigma_num2)

regress lslarl l1.wave l1.household_size l1.age l1.sex l1.mstat l1.total_children l1.edu l1.work_status l1.health l1.ht l1.db l1.heart l1.arth l1.psych l1.cancer [pweight=weight_a]
predict t_hat_den2
gen t_sigma_den2 = e(rmse) // get standard deviation of residuals
gen gps_den2 = normalden(lslarl, t_hat_den2, t_sigma_den2)

gen wt2=gps_num2/gps_den2
replace wt2=wt2*weight_a

regress lslavd l1.wave [pweight=weight_a]
predict t_hat_num3
gen t_sigma_num3 = e(rmse) // get standard deviation of residuals
gen gps_num3 = normalden(lslavd, t_hat_num3, t_sigma_num3)

regress lslavd l1.wave l1.household_size l1.age l1.sex l1.mstat l1.total_children l1.edu l1.work_status l1.health l1.ht l1.db l1.heart l1.arth l1.psych l1.cancer [pweight=weight_a]
predict t_hat_den3
gen t_sigma_den3 = e(rmse) // get standard deviation of residuals
gen gps_den3 = normalden(lslavd, t_hat_den3, t_sigma_den3)

gen wt3=gps_num3/gps_den3
replace wt3=wt3*weight_a

*keep if wt1!=. | wt2!=. | wt3!=.

gen lncohesion=ln(cohesion)
gen lnlslarl=ln(lslarl)
gen lnlslavd=ln(lslavd)

gen flonely=f1.lonely_dich

qui melogit flonely c.cohesion [pweight=wt1] || id: //*******************
estat ic
qui melogit flonely c.cohesion##c.cohesion [pweight=wt1] || id: 
estat ic
qui melogit flonely c.cohesion##c.cohesion##c.cohesion [pweight=wt1] || id:
estat ic
qui melogit flonely c.cohesion c.lncohesion [pweight=wt1] || id:
estat ic

qui melogit flonely c.lslarl [pweight=wt2] || id: //*******************
estat ic
qui melogit flonely c.lslarl##c.lslarl [pweight=wt2] || id:
estat ic
qui melogit flonely c.lslarl##c.lslarl##c.lslarl [pweight=wt2] || id:
estat ic
qui melogit flonely c.lslarl c.lnlslarl [pweight=wt2] || id:
estat ic

qui melogit flonely c.lslavd [pweight=wt3] || id:
estat ic
qui melogit flonely c.lslavd##c.lslavd [pweight=wt3] || id: //*******************
estat ic
qui melogit flonely c.lslavd##c.lslavd##c.lslavd [pweight=wt3] || id:
estat ic
qui melogit flonely c.lslavd c.lnlslavd [pweight=wt3] || id:
estat ic


melogit flonely c.cohesion [pweight=wt1] || id:
margins , at(cohesion=(1 2 3 4 5 6 7)) post
nlcom (rr_aveg: ((_b[2._at] / _b[1._at]) + (_b[3._at] / _b[2._at]) + (_b[4._at] / _b[3._at]) + (_b[5._at] / _b[4._at]) + (_b[6._at] / _b[5._at]) + (_b[7._at] / _b[6._at]))/6)
matrix res_coh=r(table)'
melogit flonely c.lslarl [pweight=wt2] || id:
margins , at(lslarl=(1 2 3 4 5)) post
nlcom (rr_aveg: ((_b[2._at] / _b[1._at]) + (_b[3._at] / _b[2._at]) + (_b[4._at] / _b[3._at]) + (_b[5._at] / _b[4._at]))/4)
matrix res_rub=r(table)'
melogit flonely c.lslavd##c.lslavd [pweight=wt3] || id:
margins , at(lslavd=(1 2 3 4 5)) post
nlcom (rr_aveg: ((_b[2._at] / _b[1._at]) + (_b[3._at] / _b[2._at]) + (_b[4._at] / _b[3._at]) + (_b[5._at] / _b[4._at]))/4)
matrix res_van=r(table)'

putexcel set "$workdir/Results/HILDA results.xlsx", sheet(cohesion) modify 
putexcel A1=matrix(res_coh), names
putexcel A1="exp_level"
putexcel set "$workdir/Results/HILDA results.xlsx", sheet(rubbish) modify
putexcel A1=matrix(res_rub), names
putexcel A1="exp_level"
putexcel set "$workdir/Results/HILDA results.xlsx", sheet(vandalism) modify
putexcel A1=matrix(res_van), names
putexcel A1="exp_level"