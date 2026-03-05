
global workdir="C:\Users\pcla5984\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-ESS"

use "$workdir\Data\Analysis data\Combined analysis data.dta", clear

encode cntry, gen(cntry_num)
encode region, gen(region_num)

recode fltlnl 1=0 2=0 3=1 4=1 .a=. .b=. .c=.
label define noyes 0 "Not lonely" 1 "Lonely at least most of the time"
label values fltlnl noyes

gen agecat=0 if agea>=18 & agea<30
replace agecat=1 if agea>=30 & agea<60

replace year=year-2010

matrix trend=J(35,6,.)

forvalues i=1/35 {
	
	preserve
	keep if cntry_num==`i'
	qui distinct year
	if r(ndistinct)>2 {
		
		melogit fltlnl c.year [pweight=anweight] if cntry_num==`i' || idno:
		
		local est=_b[year]
		matrix temp=r(table)
		local p=temp[4,1]
		
		matrix trend[`i',1]=`est'
		matrix trend[`i',2]=`p'
	}
	
	restore

}

forvalues i=1/35 {
	
	preserve
	keep if cntry_num==`i' & agecat==0
	qui distinct year
	if r(ndistinct)>2 {
		
		capture melogit fltlnl c.year [pweight=anweight] if cntry_num==`i' || idno:

		if _rc==1 {
			meqrlogit fltlnl c.year [pweight=anweight] if cntry_num==`i' || idno:
		}
					
		local est=_b[year]
		matrix temp=r(table)
		local p=temp[4,1]
		
		matrix trend[`i',3]=`est'
		matrix trend[`i',4]=`p'
	}
	
	restore

}

forvalues i=1/35 {
	
	preserve
	keep if cntry_num==`i' & agecat==1
	qui distinct year
	if r(ndistinct)>2 {
		
		capture melogit fltlnl c.year [pweight=anweight] if cntry_num==`i' || idno:

		if _rc==1 {
			meqrlogit fltlnl c.year [pweight=anweight] if cntry_num==`i' || idno:
		}
					
		local est=_b[year]
		matrix temp=r(table)
		local p=temp[4,1]
		
		matrix trend[`i',5]=`est'
		matrix trend[`i',6]=`p'
	}
	
	restore

}

matrix list trend