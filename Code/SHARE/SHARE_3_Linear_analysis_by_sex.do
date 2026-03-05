
global workdir "C:/Users/pcla5984/Sydney Uni Dropbox/Philip Clare/Lancet series data analysis/Paper 1-SHARE"

use "$workdir/Data/share_longitudinal_combined.dta", clear

rename dn042_ sex

bysort mergeid: egen wgtsum=sum(cciw)
bysort mergeid: gen n=_N
gen weight=cciw*(n/wgtsum)
drop n wgtsum cciw

gen all=1

replace stratum1="DEX" if country==12 & stratum1==""
replace stratum1="SEX" if country==13 & stratum1==""
replace stratum1="FRX" if country==17 & stratum1==""
replace stratum1="DMX" if country==18 & stratum1==""
replace stratum1="BEX" if country==23 & stratum1==""
replace stratum1="ILX" if country==25 & stratum1==""
replace stratum1="HUX" if country==32 & stratum1==""
replace stratum1="PTX" if country==33 & stratum1==""
replace stratum1="EEX" if country==35 & stratum1==""

drop if lonely_scale==. | wave==. | country==. | stratum1=="" | weight==.

merge m:1 country using "$workdir/Data/regions.dta", keep(match master) nogen
*drop if region=="Western Asia"
encode region, gen(reg)
drop region
rename reg region
order region, before(region2)

svyset mergeid, strata(stratum1) || _n, weight(weight)

qui svydescribe, generate(single)
drop if single==1

matrix pscale=J(4,2,.)

svy, subpop(all if region==1 & sex==1): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[1,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r0m=r(table)'

svy, subpop(all if region==2 & sex==1): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[2,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r1m=r(table)'

svy, subpop(all if region==3 & sex==1): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[3,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r2m=r(table)'

svy, subpop(all if region==5 & sex==1): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[4,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r3m=r(table)'

svy, subpop(all if region==4 & sex==1): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[4,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r4m=r(table)'

svy, subpop(all if region==1 & sex==2): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[1,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r0f=r(table)'

svy, subpop(all if region==2 & sex==2): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[2,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r1f=r(table)'

svy, subpop(all if region==3 & sex==2): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[3,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r2f=r(table)'

svy, subpop(all if region==5 & sex==2): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[4,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r3f=r(table)'

svy, subpop(all if region==4 & sex==2): melogit lonely_scale c.wave i.country || mergeid:, intp(15)
matrix pscale[4,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r4f=r(table)'

matrix resm=r0m[1..5,1..2] \ r1m[1..5,1..2] \ r2m[1..5,1..2] \ r3m[1..5,1..2] \ r4m[1..5,1..2]
matrix regm=1,1\1,2\1,3\1,4\1,5\2,1\2,2\2,3\2,4\2,5\3,1\3,2\3,3\3,4\3,5\4,1\4,2\4,3\4,4\4,5\5,1\5,2\5,3\5,4\5,5

matrix res_final_scalem=regm,resm

matrix resf=r0f[1..5,1..2] \ r1f[1..5,1..2] \ r2f[1..5,1..2] \ r3f[1..5,1..2] \ r4f[1..5,1..2]
matrix regf=1,1\1,2\1,3\1,4\1,5\2,1\2,2\2,3\2,4\2,5\3,1\3,2\3,3\3,4\3,5\4,1\4,2\4,3\4,4\4,5\5,1\5,2\5,3\5,4\5,5

matrix res_final_scalef=regf,resf

preserve
clear
svmat res_final_scalem
export delimited "$workdir/Results/linear_scale_by_wave_male.csv", replace
restore

preserve
clear
svmat res_final_scalef
export delimited "$workdir/Results/linear_scale_by_wave_female.csv", replace
restore

matrix psingle=J(4,2,.)

svy, subpop(all if region==1 & sex==1): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[1,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r0m=r(table)'

svy, subpop(all if region==2 & sex==1): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[2,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r1m=r(table)'

svy, subpop(all if region==3 & sex==1): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[3,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r2m=r(table)'

svy, subpop(all if region==5 & sex==1): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[4,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r3m=r(table)'

svy, subpop(all if region==4 & sex==1): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[4,1]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r4m=r(table)'

svy, subpop(all if region==1 & sex==2): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[1,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r0f=r(table)'

svy, subpop(all if region==2 & sex==2): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[2,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r1f=r(table)'

svy, subpop(all if region==3 & sex==2): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[3,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r2f=r(table)'

svy, subpop(all if region==5 & sex==2): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[4,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r3f=r(table)'

svy, subpop(all if region==4 & sex==2): melogit lonely_single_b c.wave i.country || mergeid:, intp(15)
matrix psingle[4,2]=r(table)[4,1]
margins, at(wave=(5 6 7 8 9))
matrix r4f=r(table)'

matrix resm=r0m[1..5,1..2] \ r1m[1..5,1..2] \ r2m[1..5,1..2] \ r3m[1..5,1..2] \ r4m[1..5,1..2]
matrix regm=1,1\1,2\1,3\1,4\1,5\2,1\2,2\2,3\2,4\2,5\3,1\3,2\3,3\3,4\3,5\4,1\4,2\4,3\4,4\4,5\5,1\5,2\5,3\5,4\5,5

matrix res_final_singlem=regm,resm

matrix resf=r0f[1..5,1..2] \ r1f[1..5,1..2] \ r2f[1..5,1..2] \ r3f[1..5,1..2] \ r4f[1..5,1..2]
matrix regf=1,1\1,2\1,3\1,4\1,5\2,1\2,2\2,3\2,4\2,5\3,1\3,2\3,3\3,4\3,5\4,1\4,2\4,3\4,4\4,5\5,1\5,2\5,3\5,4\5,5

matrix res_final_singlef=regf,resf

preserve
clear
svmat res_final_singlem
export delimited "$workdir/Results/linear_single_by_wave_male.csv", replace
restore

preserve
clear
svmat res_final_singlef
export delimited "$workdir/Results/linear_single_by_wave_female.csv", replace
restore

matrix pvalues=pscale,psingle
preserve
clear
svmat pvalues
export delimited "$workdir/Results/trend_pvalues_sex.csv", replace
restore
