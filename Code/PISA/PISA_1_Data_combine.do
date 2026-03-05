use "C:\Users\pcla5984\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Stata\Data_2000.dta", clear

keep cnt w_fstuwt ST03Q01 ST31Q06

gen stratum="d2000ns"
rename ST03Q01 sex
rename ST31Q06 lonely
gen year=2000

rename *, lower

recode lonely 1=4 2=3 3=2 4=1 7=. 8=. 9=.
label define ST31Q06 1 "Strongly agree" 2 "Agree" 3 "Disagree" 4 "Strongly disagree", modify

save "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2000.dta", replace

use "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Stata\Data_2003.dta", clear

keep CNT STRATUM W_FSTUWT ST03Q01 ST27Q06

rename ST03Q01 sex
rename ST27Q06 lonely
gen year=2003

rename *, lower

save "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2003.dta", replace

use "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Stata\Data_2012.dta", clear

keep CNT STRATUM W_FSTUWT ST04Q01 ST87Q06

rename ST04Q01 sex
rename ST87Q06 lonely
gen year=2012

rename *, lower

save "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2012.dta", replace

use "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Stata\Data_2015.dta", clear

keep CNT STRATUM W_FSTUWT ST004D01T ST034Q06TA

rename ST004D01T sex
rename ST034Q06TA lonely
gen year=2015

rename *, lower

save "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2015.dta", replace

use "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Stata\Data_2018.dta", clear

keep CNT STRATUM W_FSTUWT ST004D01T ST034Q06TA WB158Q01HA WB160Q01HA

rename ST004D01T sex
rename ST034Q06TA lonely
rename WB158Q01HA time_friends
rename WB160Q01HA talk_friends
gen year=2018

rename *, lower

save "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2018.dta", replace

use "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Stata\Data_2022.dta", clear

keep CNT STRATUM W_FSTUWT ST004D01T ST034Q06TA WB158Q01HA WB160Q01HA

rename ST004D01T sex
rename ST034Q06TA lonely
rename WB158Q01HA time_friends
rename WB160Q01HA talk_friends
gen year=2022

rename *, lower

save "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2022.dta", replace

use "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2000.dta", clear
append using "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2003.dta"
append using "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2012.dta"
append using "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2015.dta"
append using "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2018.dta"
append using "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Data_2022.dta"

encode cnt, gen(country)

replace lonely=. if lonely>4

gen lonely_b=1 if lonely==1 | lonely==2
replace lonely_b=0 if lonely==3 | lonely==4

save "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Combined_data.dta", replace


