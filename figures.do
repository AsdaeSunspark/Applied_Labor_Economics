import delimited "C:\Users\Public\Documents\mael_linh\sorkin_2017\rcode\data\simdExptV.csv", clear 

*temp
gen male = runiform() < 0.5
**
save "C:\Users\Public\Documents\mael_linh\data\final\vpsi.dta", replace


use "C:\Users\Public\Documents\mael_linh\data\final\vpsi.dta", clear
**

*figure 1: psi - v, 20 bins, by gender
**NOTE: do separately for each gender
xtile binm = exptv if male==1, nq(20)
bys binm: egen avepsim = mean(psi) if male==1
bys binm: gen no=_n if male==1
egen stdpsi = sd(psi) if male==1, by(binm)
gen uppsim = avepsim + stdpsi
gen lopsim = avepsim - stdpsi

twoway scatter avepsim exptv if no==1 & male==1 || lfit avepsim exptv if no==1 & male==1, lcolor(blue) || ///
line uppsim exptv if no==1  & male==1, lcolor(red) || ///
line lopsim exptv if no==1  & male==1, lcolor(red) , ///
ytitle(Firm effect in earnings) xtitle(Firm value) legend(order(1 "Male: Bin mean" 2 "Fitted values" 3 "+1 std.dev" 4 "-1 std.dev")) graphregion(color(white)) bgcolor(white)
graph export "C:\Users\Public\Documents\mael_linh\output\vpsi_male", as(png) replace 

xtile binf = exptv if male==0, nq(20)
bys binf: egen avepsif = mean(psi) if male==0
bys binf: gen nof=_n if male==0
egen stdpsif = sd(psi) if male==0, by(binf)
gen uppsif = avepsif + stdpsif
gen lopsif = avepsif - stdpsif

twoway scatter avepsif exptv if nof==1 & male==0 || lfit avepsif exptv if nof==1 & male==0, lcolor(blue) || ///
line uppsif exptv if nof==1  & male==0, lcolor(red) || ///
line lopsif exptv if nof==1  & male==0, lcolor(red) , ///
ytitle(Firm effect in earnings) xtitle(Firm value) legend(order(1 "Female: Bin mean" 2 "Fitted values" 3 "+1 std.dev" 4 "-1 std.dev")) graphregion(color(white)) bgcolor(white)
graph export "C:\Users\Public\Documents\mael_linh\output\vpsi_female", as(png) replace 


*figure 2: density of psi by gender
twoway (hist psi if male==1, color(gs8)) (hist psi if male==0, color(gs11%70)), legend(order(1 "Male" 2 "Female"))  graphregion(color(white)) bgcolor(white)
graph export "C:\Users\Public\Documents\mael_linh\output\psiden", as(png) replace 

*figure 3: density of wages by gender
bysort nninouv (an debremu): gen num =_n
sum num  if sx=="1" & cond1==1
sum num  if sx=="0" & cond1==1


sum csbrh if sx=="1" & cond1==1
count if sx=="1" & cond1==1 & csbrh>=100 & csbrh!=.

sum csbrh if sx=="0" & cond1==1 
count if sx=="0" & cond1==1 & csbrh>=100 & csbrh!=.

sum cnetnetrh if sx=="1" & cond1==1
sum cnetnetrh if sx=="0" & cond1==1

* for better visualization, limit to below 100
use "C:\Users\Public\Documents\mael_linh\data\final\transition_0207.dta", clear
twoway (hist logyh1 if male==1, color(gs8)) (hist logyh1 if male==0, color(gs11%70)), legend(order(1 "Male" 2 "Female")) graphregion(color(white)) bgcolor(white) xtitle(log hourly wages)
graph export "C:\Users\Public\Documents\mael_linh\output\wageden", as(png) replace


