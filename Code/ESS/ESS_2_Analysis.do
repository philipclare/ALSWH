
global workdir="D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-ESS"

use "$workdir\Data\Analysis data\Combined analysis data.dta", clear

encode cntry, gen(cntry_num)
encode region, gen(region_num)

recode fltlnl 1=0 2=0 3=1 4=1 .a=. .b=. .c=.
label define noyes 0 "Not lonely" 1 "Lonely at least most of the time"
label values fltlnl noyes

gen agecat=0 if agea>=18 & agea<30
replace agecat=1 if agea>=30 & agea<60

replace year=year-2010

matrix pvalues=J(5,9,.)

melogit fltlnl c.year##i.region_num [pweight=anweight] || idno: 

lincom c.year
matrix pvalues[1,1]=r(p)
lincom c.year + 2.region_num#c.year
matrix pvalues[2,1]=r(p)
lincom c.year + 3.region_num#c.year
matrix pvalues[3,1]=r(p)
lincom c.year + 4.region_num#c.year
matrix pvalues[4,1]=r(p)
lincom c.year + 5.region_num#c.year
matrix pvalues[5,1]=r(p)

melogit fltlnl c.year##i.region_num##i.gndr [pweight=anweight] || idno: 

lincom c.year
matrix pvalues[1,4]=r(p)
lincom c.year + 2.region_num#c.year
matrix pvalues[2,4]=r(p)
lincom c.year + 3.region_num#c.year
matrix pvalues[3,4]=r(p)
lincom c.year + 4.region_num#c.year
matrix pvalues[4,4]=r(p)
lincom c.year + 5.region_num#c.year
matrix pvalues[5,4]=r(p)

lincom c.year + 2.gndr#c.year
matrix pvalues[1,7]=r(p)
lincom c.year + 2.region_num#c.year + 2.gndr#c.year + 2.region_num#2.gndr#c.year
matrix pvalues[2,7]=r(p)
lincom c.year + 3.region_num#c.year + 2.gndr#c.year + 3.region_num#2.gndr#c.year
matrix pvalues[3,7]=r(p)
lincom c.year + 4.region_num#c.year + 2.gndr#c.year + 4.region_num#2.gndr#c.year
matrix pvalues[4,7]=r(p)
lincom c.year + 5.region_num#c.year + 2.gndr#c.year + 5.region_num#2.gndr#c.year
matrix pvalues[5,7]=r(p)

margins i.region_num, at (year=(0 2 4 13)) 
matrix res_overall=r(table)'

margins 2.gndr#i.region_num, at (year=(0 2 4 13)) 
matrix res_female=r(table)'

margins 1.gndr#i.region_num, at (year=(0 2 4 13)) 
matrix res_male=r(table)'

preserve
clear
svmat res_overall
export delimited "$workdir/Results/overall trend.csv", replace
restore

preserve
clear
svmat res_female
export delimited "$workdir/Results/female trend.csv", replace
restore

preserve
clear
svmat res_male
export delimited "$workdir/Results/male trend.csv", replace
restore

melogit fltlnl c.year##i.region_num##i.agecat [pweight=anweight] || idno: 

lincom c.year
matrix pvalues[1,2]=r(p)
lincom c.year + 2.region_num#c.year
matrix pvalues[2,2]=r(p)
lincom c.year + 3.region_num#c.year
matrix pvalues[3,2]=r(p)
lincom c.year + 4.region_num#c.year
matrix pvalues[4,2]=r(p)
lincom c.year + 5.region_num#c.year
matrix pvalues[5,2]=r(p)

lincom c.year + 1.agecat#c.year
matrix pvalues[1,3]=r(p)
lincom c.year + 2.region_num#c.year + 1.agecat#c.year
matrix pvalues[2,3]=r(p)
lincom c.year + 3.region_num#c.year + 1.agecat#c.year
matrix pvalues[3,3]=r(p)
lincom c.year + 4.region_num#c.year + 1.agecat#c.year
matrix pvalues[4,3]=r(p)
lincom c.year + 5.region_num#c.year + 1.agecat#c.year
matrix pvalues[5,3]=r(p)

melogit fltlnl c.year##i.region_num##i.gndr##i.agecat [pweight=anweight] || idno: 

lincom c.year
matrix pvalues[1,5]=r(p)
lincom c.year + 2.region_num#c.year
matrix pvalues[2,5]=r(p)
lincom c.year + 3.region_num#c.year
matrix pvalues[3,5]=r(p)
lincom c.year + 4.region_num#c.year
matrix pvalues[4,5]=r(p)
lincom c.year + 5.region_num#c.year
matrix pvalues[5,5]=r(p)

lincom c.year + 2.gndr#c.year
matrix pvalues[1,6]=r(p)
lincom c.year + 2.region_num#c.year + 2.gndr#c.year + 2.region_num#2.gndr#c.year
matrix pvalues[2,6]=r(p)
lincom c.year + 3.region_num#c.year + 2.gndr#c.year + 3.region_num#2.gndr#c.year
matrix pvalues[3,6]=r(p)
lincom c.year + 4.region_num#c.year + 2.gndr#c.year + 4.region_num#2.gndr#c.year
matrix pvalues[4,6]=r(p)
lincom c.year + 5.region_num#c.year + 2.gndr#c.year + 5.region_num#2.gndr#c.year
matrix pvalues[5,6]=r(p)

lincom c.year + 1.agecat#c.year
matrix pvalues[1,8]=r(p)
lincom c.year + 2.region_num#c.year + 1.agecat#c.year + 2.region_num#1.agecat#c.year
matrix pvalues[2,8]=r(p)
lincom c.year + 3.region_num#c.year + 1.agecat#c.year + 3.region_num#1.agecat#c.year
matrix pvalues[3,8]=r(p)
lincom c.year + 4.region_num#c.year + 1.agecat#c.year + 4.region_num#1.agecat#c.year
matrix pvalues[4,8]=r(p)
lincom c.year + 5.region_num#c.year + 1.agecat#c.year + 5.region_num#1.agecat#c.year
matrix pvalues[5,8]=r(p)

lincom c.year + 2.gndr#c.year
matrix pvalues[1,9]=r(p)
lincom c.year + 2.region_num#c.year + 2.gndr#c.year + 2.region_num#2.gndr#c.year + 1.agecat#c.year + 2.region_num#1.agecat#c.year + 2.region_num#2.gndr#1.agecat#c.year
matrix pvalues[2,9]=r(p)
lincom c.year + 3.region_num#c.year + 2.gndr#c.year + 3.region_num#2.gndr#c.year + 1.agecat#c.year + 3.region_num#1.agecat#c.year + 3.region_num#2.gndr#1.agecat#c.year
matrix pvalues[3,9]=r(p)
lincom c.year + 4.region_num#c.year + 2.gndr#c.year + 4.region_num#2.gndr#c.year + 1.agecat#c.year + 4.region_num#1.agecat#c.year + 4.region_num#2.gndr#1.agecat#c.year
matrix pvalues[4,9]=r(p)
lincom c.year + 5.region_num#c.year + 2.gndr#c.year + 5.region_num#2.gndr#c.year + 1.agecat#c.year + 5.region_num#1.agecat#c.year + 5.region_num#2.gndr#1.agecat#c.year
matrix pvalues[5,9]=r(p)

margins 0.agecat#i.region_num, at (year=(0 2 4 13)) 
matrix res_young=r(table)'

margins 1.agecat#i.region_num, at (year=(0 2 4 13)) 
matrix res_middle=r(table)'

margins 0.agecat#2.gndr#i.region_num, at (year=(0 2 4 13)) 
matrix res_young_female=r(table)'

margins 0.agecat#1.gndr#i.region_num, at (year=(0 2 4 13)) 
matrix res_young_male=r(table)'

margins 1.agecat#2.gndr#i.region_num, at (year=(0 2 4 13)) 
matrix res_middle_female=r(table)'

margins 1.agecat#1.gndr#i.region_num, at (year=(0 2 4 13)) 
matrix res_middle_male=r(table)'

preserve
clear
svmat res_young
export delimited "$workdir/Results/young overall trend.csv", replace
restore

preserve
clear
svmat res_middle
export delimited "$workdir/Results/middle overall trend.csv", replace
restore

preserve
clear
svmat res_young_female
export delimited "$workdir/Results/young female trend.csv", replace
restore

preserve
clear
svmat res_young_male
export delimited "$workdir/Results/young male trend.csv", replace
restore

preserve
clear
svmat res_middle_female
export delimited "$workdir/Results/middle female trend.csv", replace
restore

preserve
clear
svmat res_middle_male
export delimited "$workdir/Results/middle male trend.csv", replace
restore

preserve
clear
svmat pvalues
export delimited "$workdir/Results/pvalues.csv", replace
restore