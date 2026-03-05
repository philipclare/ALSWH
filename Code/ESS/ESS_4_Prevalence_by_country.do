
global workdir="C:\Users\pcla5984\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-ESS"

use "$workdir\Data\Analysis data\Combined analysis data.dta", clear

encode cntry, gen(cntry_num)
encode region, gen(region_num)

recode fltlnl 1=0 2=0 3=1 4=1 .a=. .b=. .c=.
label define noyes 0 "Not lonely" 1 "Lonely at least some of the time"
label values fltlnl noyes

gen agecat=0 if agea>=18 & agea<30
replace agecat=1 if agea>=30 & agea<60

replace year=year-2010

mean fltlnl [pweight=anweight], over(cntry_num year)

matrix results=r(table)'
matrix results=results[1..140,1..6]
mata:
cntry = (1 \ 2 \ 3 \ 4 \ 5 \ 6 \ 7 \ 8 \ 9 \ 10 \ 11 \ 12 \ 13 \ 14 \ 15 \ 16 \ 17 \ 18 \ 19 \ 20 \ 21 \ 22 \ 23 \ 24 \ 25 \ 26 \ 27 \ 28 \ 29 \ 30 \ 31 \ 32 \ 33 \ 34 \ 35)
year = (0 \ 2 \ 4 \ 13)

// Create Cartesian product
combos = J(length(cntry)*length(year), 2, .)
row = 1
for (i=1; i<=length(cntry); i++) {
    for (j=1; j<=length(year); j++) {
        combos[row, 1] = cntry[i]
        combos[row, 2] = year[j]
        row++
    }
}
st_matrix("rowlevels", combos)
end

matrix results = rowlevels,results

matrix list results

preserve
clear
svmat results, names(col)
drop if se==.
rename c1 country
label define cntry_num 1 "Albania" 2 "Austria" 3 "Belgium" 4 "Bulgaria" 5 "Switzerland" 6 "Cyprus" 7 "Czech Republic" 8 "Germany" 9 "Denmark" 10 "Estonia" 11 "Spain" 12 "Finland" 13 "France" 14 "Great Britan" 15 "Greece" 16 "Croatia" 17 "Hungary" 18 "Ireland" 19 "Israel" 20 "Iceland" 21 "Italy" 22 "Lithuania" 23 "Latvia" 24 "Montenegro" 25 "Netherlands" 26 "Norway" 27 "Poland" 28 "Portugal" 29 "Serbia" 30 "Russia" 31 "Sweden" 32 "Slovenia" 33 "Slovakia" 34 "Ukraine" 35 "Kosovo"
label values country cntry_num
rename c2 year
replace year=year+2010
export delimited "$workdir/Results/prevalence by country.csv", replace
restore