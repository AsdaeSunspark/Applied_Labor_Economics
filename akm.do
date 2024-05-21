import delimited "C:\Users\Public\Documents\mael_linh\sorkin_2017\rcode\data\dfAKM.csv"

reghdfe logyh1 age1 age2 age3 if male==1, a(index1 nninouv, savefe)
rename __hdfe1__ psi_male
drop __hdfe2__ 

reghdfe logyh1 age1 age2 age3 if male==0, a(index1 nninouv, savefe)
rename __hdfe1__ psi_female
keep index1 psi_male psi_female
save  "C:\Users\Public\Documents\mael_linh\data\final\akm.dta", replace

sort index1

