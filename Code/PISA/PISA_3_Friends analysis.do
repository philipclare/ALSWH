use "D:\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-PISA\PISA data\Analysis data\Combined_data.dta", clear

keep if time_friends<90 | talk_friends<7

recode time_friends 99=.
replace time_friends=time_friends-1
recode talk_friends 9=. 99=. 3=2
replace sex=. if sex==7

foreach i in 2018 2022 {
	table country if year==`i', statistic(median time_friends) statistic(q1 time_friends) statistic(q3 time_friends)
}

foreach i in 2018 2022 {
	table country if year==`i' & sex==1, statistic(median time_friends) statistic(q1 time_friends) statistic(q3 time_friends)
}

foreach i in 2018 2022 {
	table country if year==`i' & sex==2, statistic(median time_friends) statistic(q1 time_friends) statistic(q3 time_friends)
}

foreach i in 2018 2022 {
	ranksum time_friends if year==`i', by(sex)
}

foreach i in 2018 2022 {
	bysort country: ranksum time_friends if year==`i', by(sex)
}


foreach i in 2018 2022 {
	table country if year==`i', statistic(fvperc talk_friends) 
}

foreach i in 2018 2022 {
	table country if year==`i' & sex==1, statistic(fvperc talk_friends) 
}

foreach i in 2018 2022 {
	table country if year==`i' & sex==2, statistic(fvperc talk_friends) 
}

foreach i in 2018 2022 {
	ranksum talk_friends if year==`i', by(sex)
}

foreach i in 2018 2022 {
	bysort country: ranksum talk_friends if year==`i', by(sex)
}