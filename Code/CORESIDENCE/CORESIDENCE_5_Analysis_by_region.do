
global workdir="C:/Users/pcla5984/Dropbox (Sydney Uni)/Lancet series data analysis/Paper 1-analysis of living arrangement"

use "$workdir/Data/imputed_data.dta", clear

preserve
mi extract 1
keep T1 Region min_year max_year
bysort T1 Region: keep if _n==1
gen rand=rnormal()
reshape wide rand, i(Region) j(T1)
reshape long
drop rand
sort T1 Region
mkmat min_year max_year, matrix(minmax)
restore

matrix res_levels=J(450,2,.)
local k=0
forvalues j=0/29 {
	forvalues i=1/15 {
		local k=`k'+1
		matrix res_levels[`k',1]=`j'
		matrix res_levels[`k',2]=`i'
	}
}

gen wt=1/P1

mi estimate, saving(size, replace) esample(size_samp): mixed HS17 i.Region c.T1 i.Region#c.T1 c.T1#c.T1#c.T1 i.Region#c.T1#c.T1#c.T1 || C1:

mimrgns Region using size, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(size_samp)

matrix temp=r(table)'
matrix res=res_levels,temp[1..450,1],temp[1..450,5..6],minmax

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 region
drop if year<min_year | year>max_year
drop min_year max_year

export delimited using "$workdir/Results/Processed/Size - by region.csv", replace

restore

mi estimate, saving(size_adj, replace) esample(size_samp_adj): mixed HS17 i.Region c.T1 i.Region#c.T1 c.T1#c.T1#c.T1 i.Region#c.T1#c.T1#c.T1 c.P1 || C1:

mimrgns Region using size_adj, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(size_samp_adj)

matrix temp=r(table)'
matrix res=res_levels,temp[1..450,1],temp[1..450,5..6],minmax

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 region
drop if year<min_year | year>max_year
drop min_year max_year

export delimited using "$workdir/Results/Processed/Size - by region pop-adj.csv", replace

restore

mi estimate, saving(single, replace) esample(single_samp): mixed HS01 Region##c.T1##c.T1 || C1:

mimrgns Region using single, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(single_samp)

matrix temp=r(table)'
matrix res=res_levels,temp[1..450,1],temp[1..450,5..6],minmax

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 region
drop if year<min_year | year>max_year
drop min_year max_year

export delimited using "$workdir/Results/Processed/Single-person - by region.csv", replace

restore

mi estimate, saving(single_adj, replace) esample(single_samp_adj): mixed HS01 Region##c.T1##c.T1 c.P1 || C1:

mimrgns Region using single_adj, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(single_samp_adj)

matrix temp=r(table)'
matrix res=res_levels,temp[1..450,1],temp[1..450,5..6],minmax

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 region
drop if year<min_year | year>max_year
drop min_year max_year

export delimited using "$workdir/Results/Processed/Single-person - by region pop-adj.csv", replace

restore
