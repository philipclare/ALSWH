
global workdir="C:/Users/pcla5984/Dropbox (Sydney Uni)/Lancet series data analysis/Paper 1-analysis of living arrangement"

use "$workdir/Data/imputed_data.dta", clear

mi extract 1

gen lnT1=ln(T1+0.0000001)

matrix fit=J(8,4,.)

mixed HS17 i.hdi_cat##c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[1,1]=temp[1,5..6]
mixed HS17 i.hdi_cat##c.T1##c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[2,1]=temp[1,5..6]
mixed HS17 i.hdi_cat c.T1 i.hdi_cat#c.T1 c.T1#c.T1#c.T1 i.hdi_cat#c.T1#c.T1#c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[3,1]=temp[1,5..6]
mixed HS17 i.hdi_cat##c.T1##c.T1##c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[4,1]=temp[1,5..6]
mixed HS17 i.hdi_cat##c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[5,1]=temp[1,5..6]
mixed HS17 i.hdi_cat##c.T1##c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[6,1]=temp[1,5..6]
mixed HS17 i.hdi_cat c.T1 i.hdi_cat#c.T1 c.T1#c.T1#c.T1 i.hdi_cat#c.T1#c.T1#c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[7,1]=temp[1,5..6]
mixed HS17 i.hdi_cat##c.T1##c.T1##c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[8,1]=temp[1,5..6]

mixed HS01 i.hdi_cat##c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[1,3]=temp[1,5..6]
mixed HS01 i.hdi_cat##c.T1##c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[2,3]=temp[1,5..6]
mixed HS01 i.hdi_cat c.T1 i.hdi_cat#c.T1 c.T1#c.T1#c.T1 i.hdi_cat#c.T1#c.T1#c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[3,3]=temp[1,5..6]
mixed HS01 i.hdi_cat##c.T1##c.T1##c.T1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[4,3]=temp[1,5..6]
mixed HS01 i.hdi_cat##c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[5,3]=temp[1,5..6]
mixed HS01 i.hdi_cat##c.T1##c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[6,3]=temp[1,5..6]
mixed HS01 i.hdi_cat c.T1 i.hdi_cat#c.T1 c.T1#c.T1#c.T1 i.hdi_cat#c.T1#c.T1#c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[7,3]=temp[1,5..6]
mixed HS01 i.hdi_cat##c.T1##c.T1##c.T1 i.hdi_cat##c.lnT1 || C1:
	estat ic
	matrix temp=r(S)
	matrix fit[8,3]=temp[1,5..6]

matrix list fit
