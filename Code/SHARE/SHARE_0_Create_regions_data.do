
import excel "C:\Users\pcla5984\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-analysis of living arrangement\Data\UN Regions.xlsx", sheet("Sheet1") firstrow clear

rename CountryorArea country
drop Code C1
rename Region region
rename Region2 region2

replace country="France" if country=="France [French Republic]"
replace country="Czech Republic" if country=="Czechia [Czech Republic]"

label define country 11 "Austria" 12 "Germany" 13 "Sweden" 14 "Netherlands" 15 "Spain" 16 "Italy" 17 "France" 18 "Denmark" 19 "Greece" 20 "Switzerland" 23 "Belgium" 25 "Israel" 28 "Czech Republic" 29 "Poland" 30 "Ireland" 31 "Luxembourg" 32 "Hungary" 33 "Portugal" 34 "Slovenia" 35 "Estonia" 47 "Croatia" 48 "Lithuania" 51 "Bulgaria" 53 "Cyprus" 55 "Finland" 57 "Latvia" 59 "Malta" 61 "Romania" 63 "Slovakia"

encode country, gen(country2) label(country)

drop country
rename country2 country

order country, before(region)

save "C:\Users\pcla5984\Sydney Uni Dropbox\Philip Clare\Lancet series data analysis\Paper 1-SHARE\Data\regions.dta", replace