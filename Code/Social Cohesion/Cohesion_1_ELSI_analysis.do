
global workdir="C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Social cohesion"

use "$workdir/Data for analysis/elsi_longitudinal_combined.dta", clear

bysort id: egen sum_w = total(weights)
bysort id: gen group_n=_N
gen weight_a = weights * (group_n / sum_w)

encode id, gen(id2)
drop id
rename id2 id
order id, before(wave)

foreach i in f2 f3 f12 {
	replace `i'=. if `i'==9 | `i'==8
}

xtset id wave

logit f2 [pweight=weights]
predict t_hat_num1
replace t_hat_num1=1-t_hat_num1 if f2==0

logit f2 household_size age sex mstat total_children edu work_status health ht db heart_stroke arth psych cancer [pweight=weights]
predict t_hat_den1
replace t_hat_den1=1-t_hat_den1 if f2==0

gen wt1=t_hat_num1/t_hat_den1
replace wt1=wt1*weights

logit f3 [pweight=weights]
predict t_hat_num2
replace t_hat_num2=1-t_hat_num2 if f3==0

logit f3 household_size age sex mstat total_children edu work_status health ht db heart_stroke arth psych cancer [pweight=weights]
predict t_hat_den2
replace t_hat_den2=1-t_hat_den2 if f3==0

gen wt2=t_hat_num2/t_hat_den2
replace wt2=wt2*weights

mlogit f12 [pweight=weights]
predict temp*
gen t_hat_num3=temp1 if f12==0
replace t_hat_num3=temp2 if f12==1
replace t_hat_num3=temp3 if f12==2
drop temp*

mlogit f12 household_size age sex mstat total_children edu work_status health ht db heart_stroke arth psych cancer [pweight=weights]
predict temp*
gen t_hat_den3=temp1 if f12==0
replace t_hat_den3=temp2 if f12==1
replace t_hat_den3=temp3 if f12==2
drop temp*

gen wt3=t_hat_num3/t_hat_den3
replace wt3=wt3*weights

gen flonely=f1.lonely_dich

logit flonely i.f12 [pweight=wt3]
margins i.f12, post
nlcom (rr1: _b[1.f12] / _b[0.f12]) (rr2: _b[2.f12] / _b[0.f12])
matrix res_coh=r(table)'
logit flonely i.f2 [pweight=wt1]
margins i.f2, post
nlcom (rr1: _b[1.f2] / _b[0.f2])
matrix res_rub=r(table)'
logit flonely i.f3 [pweight=wt2]
margins i.f3, post
nlcom (rr1: _b[1.f3] / _b[0.f3])
matrix res_van=r(table)'

putexcel set "$workdir/Results/ELSI results.xlsx", sheet(cohesion) modify 
putexcel A1=matrix(res_coh), names
putexcel A1="exp_level"
putexcel set "$workdir/Results/ELSI results.xlsx", sheet(rubbish) modify
putexcel A1=matrix(res_rub), names
putexcel A1="exp_level"
putexcel set "$workdir/Results/ELSI results.xlsx", sheet(vandalism) modify
putexcel A1=matrix(res_van), names
putexcel A1="exp_level"

