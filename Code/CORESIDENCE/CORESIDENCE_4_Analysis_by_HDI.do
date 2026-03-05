
global workdir="C:/Users/pcla5984/Dropbox (Sydney Uni)/Lancet series data analysis/Paper 1-analysis of living arrangement"

use "$workdir/Data/imputed_data.dta", clear

matrix res_levels=J(120,2,.)
local k=0
forvalues j=0/29 {
	forvalues i=1/4 {
		local k=`k'+1
		matrix res_levels[`k',1]=`j'
		matrix res_levels[`k',2]=`i'
	}
}

gen wt=1/P1

mi estimate, saving(size, replace) esample(size_samp): mixed HS17 i.hdi_cat##c.T1##c.T1##c.T1 || hdi_cat: || C1:

mimrgns hdi_cat using size, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(size_samp)

matrix temp=r(table)'
matrix res=res_levels,temp[1..120,1],temp[1..120,5..6]

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 hdi_cat

export delimited using "$workdir/Results/Processed/Size - by HDI.csv", replace

restore

mi estimate, saving(size_adj, replace) esample(size_samp_adj): mixed HS17 i.hdi_cat##c.T1##c.T1##c.T1 c.P1 || hdi_cat: || C1:

mimrgns hdi_cat using size_adj, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(size_samp_adj)

matrix temp=r(table)'
matrix res=res_levels,temp[1..120,1],temp[1..120,5..6]

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 hdi_cat

export delimited using "$workdir/Results/Processed/Size - by HDI pop-adj.csv", replace

restore

mi estimate, saving(single, replace) esample(single_samp): mixed HS01 i.hdi_cat##c.T1##c.T1 || hdi_cat: || C1:

mimrgns hdi_cat using single, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(single_samp)

matrix temp=r(table)'
matrix res=res_levels,temp[1..120,1],temp[1..120,5..6]

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 hdi_cat

export delimited using "$workdir/Results/Processed/Single-person - by HDI.csv", replace

restore

mi estimate, saving(single_adj, replace) esample(single_samp_adj): mixed HS01 i.hdi_cat##c.T1##c.T1 c.P1 || hdi_cat: || C1:

mimrgns hdi_cat using single_adj, at(T1=(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)) post esample(single_samp_adj)

matrix temp=r(table)'
matrix res=res_levels,temp[1..120,1],temp[1..120,5..6]

preserve
clear 
svmat res, names(col)
rename c1 year
rename c2 hdi_cat

export delimited using "$workdir/Results/Processed/Single-person - by HDI pop-adj.csv", replace

restore
