************ Import dataset
*import sas using "C:\Users\Public\Documents\mael_linh\data\DADS_Panel DADS_2010\panel2010.sas7bdat"
*save "C:\Users\Public\Documents\mael_linh\data\raw\2010.dta"


// use "C:\Users\Public\Documents\Mael_Linh\data\final\byemployee_0207.dta", clear

use "C:\Users\Public\Documents\Mael_Linh\data\raw\2010.dta", clear
************ keep relevant stuffs
keep if an>=2002 & an<=2007

rename *, lower

************ 
* CLEAN
************ 
************ transfrom using CPI Jan 2024
* to import cpi data set
*note: might not need because variable is in constant error

************ drop outliers within each gender 
*note: cleaned vars starts zith c
* gross tax salaries 
sort sx sbr
gen csbr=sbr
_pctile sbr if sx=="1", p(1 99)
replace csbr=. if sx=="1" & (sbr <= r(r1) | sbr >= r(r2))
_pctile sbr if sx=="0", p(1 99)
replace csbr=. if sx=="0" & (sbr <= r(r1) | sbr >= r(r2))

*net salary
sort sx netnetr
gen cnetnetr=netnetr
_pctile netnetr if sx=="1", p(1 99)
replace cnetnetr=. if sx=="1" & (netnetr <= r(r1) | netnetr >= r(r2))
_pctile netnetr if sx=="0", p(1 99)
replace cnetnetr=. if sx=="0" & (netnetr <= r(r1) | netnetr >= r(r2))

sort sx nbheur
gen cnbheur=nbheur
_pctile nbheur if sx=="1", p(1 99)
replace cnbheur=. if sx=="1" & (nbheur <= r(r1) | nbheur >= r(r2))
_pctile nbheur if sx=="0", p(1 99)
replace cnbheur=. if sx=="0" & (nbheur <= r(r1) | nbheur >= r(r2))
replace cnbheur=. if cnbheur==0


************ transfrom to hourly wage
gen csbrh = csbr/cnbheur
gen cnetnetrh = cnetnetr/cnbheur

************ sample restrictions
*NOTE: currently, roughly treated, need to look at the literature to check the number

* individual constraints: achieve age + non-missing
gen cond1 =0
replace cond1 = 1 if age>=25 & age<=55 & cnetnetrh!=.

* firm constraints:
*emp=1 if the hourly wage is higher than 6.67 euro (Jan 2002's minimum wage)
gen emp = 1 if cnetnetrh>=6.67
*
gen sir_copy = sir
replace sir="" if emp!=1

************ smallest firms
*single-ton workers
bys nninouv: gen countid = _n 
bys nninouv: egen sum_value = total(countid)
gen singleton = 0
replace singleton = 1 if sum_value==1

*count all observed employee + nonsingleton employee
bys sir an (nninouv): gen countid2= _n
bys sir an (nninouv): egen numemp = max(countid2)
*on average, each company-year has 4.6 observed employees
sum numemp if countid2==1, detail
sum nbsa_ent if countid2==1, detail

bys sir an (nninouv): gen countid3= _n if singleton==0
bys sir an (nninouv): egen numemp_sing = max(countid3)
*on average, each company-year has 2.5 observed non-singleton employees, nob drops to 2.9 million firms-year
sum numemp_sing if countid3==1, detail

drop countid countid2 countid3 sum_valu


************ dominant firms: 
*NOTE: variables considering annual wages have "an" at the end. Sample sizes can be different zhen consider annual and hourly wages, because there are cases with w0 working hours
*NOTE: cond1 is applied
*index
gen startd = debremu + (an-2002)*360
gen endd = finremu + (an-2002)*360
bysort nninouv (an debremu): gen duration =  endd - startd +1

** annual dominant: duration>180days
bys nninouv an: gen max1 = cnetnetrh if duration >=180 & cond1==1
bys nninouv an: egen max = max(max1)
gen dosir_temp= sir if max==cnetnetrh & cond1==1 
*note: among the highest and equally pay companies, choose the one pay for the longest time
bys nninouv an max: gen duration_temp2=duration if  duration >=180 & cond1==1
bys nninouv an max: egen duration_temp = max(duration_temp2)
bys nninouv an max: replace dosir_temp="" if duration_temp2!=duration
gen dosir = dosir_temp
bys nninouv (an debremu): gen num=_n
gen num1 = num if dosir!=""
drop max1 duration_temp dosir_temp duration_temp2

** quarterly dominant 
*exceptional rule: annual dominant 
gen dosirq1 = dosir if finremu<=90 | debremu<=90 & cond1==1
gen dosirq2 = dosir if finremu>=91 & debremu<=90 & cond1==1
gen dosirq3 = dosir if finremu>=181 & debremu<=180 & cond1==1
gen dosirq4 = dosir if finremu>=271 & debremu<=270 & cond1==1

*salary over t and t-1: cant sum because salary is annual
gen compareq1 = cnetnetrh if finremu<=90 | debremu<=90 & cond1==1
gen compareq2 = cnetnetrh if finremu>=91 & debremu<=90 & cond1==1
gen compareq3 = cnetnetrh if finremu>=181 & debremu<=180 & cond1==1
gen compareq4 = cnetnetrh if finremu>=271 & debremu<=270 & cond1==1

bysort nninouv an (num1): egen maxq1 = max(compareq1)
bysort nninouv an (num1): egen maxq2 = max(compareq2)
bysort nninouv an (num1): egen maxq3 = max(compareq3)
bysort nninouv an (num1): egen maxq4 = max(compareq4)

gen pick1 = sir if maxq1==cnetnetrh
gen pick2 = sir if maxq2==cnetnetrh
gen pick3 = sir if maxq3==cnetnetrh
gen pick4 = sir if maxq4==cnetnetrh

bys nninouv an (pick1): replace pick1 = pick1[_N]
bys nninouv an (pick2): replace pick2 = pick2[_N]
bys nninouv an (pick3): replace pick3 = pick3[_N]
bys nninouv an (pick4): replace pick4 = pick4[_N]

replace dosirq1 = pick1 if dosirq1==""
replace dosirq2 = pick2 if dosirq2==""
replace dosirq3 = pick3 if dosirq3==""
replace dosirq4 = pick4 if dosirq4==""
sort nninouv an num

drop maxq* compare* pick*
************ transition
/* NOTE on notations:
- transition from node1 to node2
- ee and ene indicate 2 types of transition 
- all other variables (rows) have values of individual at the time employed at node1
*/
*nodes
gen node1 = dosir
bysort nninouv (num1): gen node2 = node1[_n+1]

*transition
gen ee = .
*case1: not switch to another dominant employer
bysort nninouv (num1): replace ee=1 if node1==node2 & cond1==1 & node1!=""
*case2: switch
*find last quarter employed at node1 (continuous employed: example of not continuous, id==N199800030609)
// bysort nninouv (num1): gen last = 8 if dosirq4[_n+1] == node1 & ee==. & node2!=""  & cond1==1
// bysort nninouv (num1): replace last =. if dosirq3[_n+1] != node1 & ee==. & node2!=""  & cond1==1
bysort nninouv (num1): gen last = 7 if dosirq3[_n+1] == node1 & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last =. if dosirq2[_n+1] != node1 & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last = 6 if dosirq2[_n+1] == node1 & last==. & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last =. if dosirq1[_n+1] != node1 & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last = 5 if dosirq1[_n+1] == node1 & last==. & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last =. if dosirq4 != node1 & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last = 4 if dosirq4 == node1 & last==. & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last =. if dosirq3 != node1 & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last = 3 if dosirq3 == node1 & last==. & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last =. if dosirq2 != node1 & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last = 2 if dosirq2 == node1 & last==. & ee==. & node2!="" & cond1==1
bysort nninouv (num1): replace last = 1 if dosirq1 == node1 & last==. & ee==. & node2!="" & cond1==1
*note: roughly check if any anomalies     count if last==8

*find first quarter employed at node2
// bysort nninouv (num1): gen first = 1 if dosirq1 == node2 & ee==. & cond1==1 & node2!="" & first>last
// bysort nninouv (num1): replace first = first if dosirq2 != node2 & ee==. & cond1==1
bysort nninouv (num1): gen first = 2 if dosirq2 == node2 & ee==. & cond1==1 & node2!="" & 2>last
bysort nninouv (num1): replace first = first if dosirq3 != node2 & ee==. & cond1==1 & node2!=""
bysort nninouv (num1): replace first = 3 if dosirq3 == node2 & first==. & ee==. & cond1==1 & node2!="" & 3>last
bysort nninouv (num1): replace first = first if dosirq4 != node2 & ee==. & cond1==1 & node2!=""
bysort nninouv (num1): replace first = 4 if dosirq4 == node2 & first==. & ee==. & cond1==1 & node2!="" & 4>last
bysort nninouv (num1): replace first = first if dosirq1[_n+1] != node2 & ee==. & cond1==1 & node2!=""
bysort nninouv (num1): replace first = 5 if dosirq1[_n+1] == node2 & first==. & ee==. & cond1==1 & node2!="" & 5>last
bysort nninouv (num1): replace first = first if dosirq2[_n+1] != node2 & ee==. & cond1==1 & node2!=""
bysort nninouv (num1): replace first = 6 if dosirq2[_n+1] == node2 & first==. & ee==. & cond1==1 & node2!="" & 6>last
bysort nninouv (num1): replace first = first if dosirq3[_n+1] != node2 & ee==. & cond1==1 & node2!=""
bysort nninouv (num1): replace first = 7 if dosirq3[_n+1] == node2 & first==. & ee==. & cond1==1 & node2!="" & 7>last
bysort nninouv (num1): replace first = first if dosirq4[_n+1] != node2 & ee==. & cond1==1 & node2!=""
bysort nninouv (num1): replace first = 8 if dosirq4[_n+1] == node2 & first==. & ee==. & cond1==1 & node2!="" & 8>last
*note: roughly check if any anomalies    count if first==1

*adjacent
replace ee = 1 if first-last==1
bysort nninouv (num1): replace ee=0 if (an + 1) != an[_n+1] & ee==1

*not adjacent
gen gap = first-last
*generating check variables
gen c2 = 1 if gap>1 & last==1 & first>2 & gap!=.
gen c3 = 1 if gap>1 & last<3 & first>3 & gap!=.
gen c4 = 1 if gap>1 & last<4 & first>4 & gap!=.
gen c5 = 1 if gap>1 & last<5 & first>5 & gap!=.
gen c6 = 1 if gap>1 & last<6 & first>6 & gap!=.
gen c7 = 1 if gap>1 & last<7 & first>7 & gap!=.

gen nonemp=.
replace nonemp = 1 if c2==1 & dosirq2==""
replace nonemp = 1 if c3==1 & dosirq3==""
replace nonemp = 1 if c4==1 & dosirq4==""
replace nonemp = 1 if c5==1 & dosirq1[_n+1]==""
replace nonemp = 1 if c6==1 & dosirq2[_n+1]==""
replace nonemp = 1 if c7==1 & dosirq3[_n+1]==""
replace nonemp = 0 if c2==1 & !missing(dosirq2) & nonemp!=.
replace nonemp = 0 if c3==1 & !missing(dosirq3) & nonemp!=.
replace nonemp = 0 if c4==1 & !missing(dosirq4) & nonemp!=.
replace nonemp = 0 if c5==1 & !missing(dosirq1[_n+1]) & nonemp!=.
replace nonemp = 0 if c6==1 & !missing(dosirq2[_n+1]) & nonemp!=.
replace nonemp = 0 if c7==1 & !missing(dosirq3[_n+1]) & nonemp!=.

gen ene=.
bysort nninouv (num1): replace ene=1 if (an + 1) != an[_n+1] & num1[_n+1]!=.
replace ene=1 if nonemp==1

replace ee=0 if nonemp==1 & ee==.
replace ene=1 if ee==0 & ene==.


drop c2 c3 c4 c5 c6 c7 gap nonemp

preserve
keep if num1!=.
gen sector1 = substr(apen, 1,2)
bys nninouv (an num): gen apenyear2 = apen[_n+1]
gen sector2 = substr(apenyear2, 1,2)

forval i = 1/2 {
gen sector`i'a = "a" if sector`i'<="02" & sector`i'!=""
replace sector`i'a = "b" if sector`i'=="05" & sector`i'!=""
replace sector`i'a = "c" if sector`i'<="14" & sector`i'>="10" & sector`i'!=""
replace sector`i'a = "d" if sector`i'<="37" & sector`i'>="15" & sector`i'!=""
replace sector`i'a = "e" if sector`i'<="41" & sector`i'>="40" & sector`i'!=""
replace sector`i'a = "f" if sector`i'=="45" 
replace sector`i'a = "g" if sector`i'<="52" & sector`i'>="50" & sector`i'!=""
replace sector`i'a = "h" if sector`i'=="55" 
replace sector`i'a = "g" if sector`i'<="64" & sector`i'>="60" & sector`i'!=""
replace sector`i'a = "j" if sector`i'<="67" & sector`i'>="65" & sector`i'!=""
replace sector`i'a = "k" if sector`i'<="74" & sector`i'>="70" & sector`i'!=""
replace sector`i'a = "l" if sector`i'=="75" 
replace sector`i'a = "m" if sector`i'=="80" 
replace sector`i'a = "n" if sector`i'=="85" 
replace sector`i'a = "o" if sector`i'<="93" & sector`i'>="90" & sector`i'!=""
replace sector`i'a = "p" if sector`i'<="97" & sector`i'>="95" & sector`i'!=""
replace sector`i'a = "q" if sector`i'=="99"
}

gen male=1 if sx=="1"
replace male=0 if sx=="0"
gen year1 = an
bys nninouv (an num): gen year2 = an[_n+1]
gen age1=age
bys nninouv (an num): gen age2 = age[_n+1]
gen logy1 = log(cnetnetr)
bys nninouv (an num): gen cnetnetr2 = cnetnetr[_n+1]
gen logy2 = log(cnetnetr2)
gen logyh1 = log(cnetnetrh)
bys nninouv (an num): gen cnetnetrh2 = cnetnetrh[_n+1]
gen logyh2 = log(cnetnetrh2)
rename node1 index1
rename node2 index2

*set a rule according to sorkin's code: if worker stayed, ee + ene = 0 -> ee = ene = 0
replace ee = 0 if index1 == index2 & index1!="" & (year2-year1==1)
replace ene = 0 if index1 == index2 & index1!="" & (year2-year1==1)
count if ene!=. & ee==1 // =0, no contradicting values: all ene==. if ee==1 -> safe to replace
replace ene=0 if ee==1

br nninouv male year1 year2 age1 logy1 index1 age2 logy2 ee index2 ene logyh1 logyh2 sector1 sector2 sector1a sector2a
keep nninouv male year1 year2 age1 logy1 index1 age2 logy2 ee index2 ene logyh1 logyh2 sector1 sector2 sector1a sector2a
save  "C:\Users\Public\Documents\mael_linh\data\final\transition_0207.dta", replace
restore

save "C:\Users\Public\Documents\mael_linh\data\final\byemployee_0207.dta", replace
*************
sort nninouv an num

************ 
* DESCRIPTIVE
************ 
*****************************
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
twoway (hist csbrh if sx=="1" & cond1==1  & csbrh<100, color(gs8)) (hist csbrh if sx=="0" & cond1==1  & csbrh<100, color(gs11%70)), legend(order(1 "Male" 2 "Female"))
twoway (hist cnetnetrh if sx=="1" & cond1==1  & cnetnetrh<100, color(gs8)) (hist cnetnetrh if sx=="0" & cond1==1  & cnetnetrh<100, color(gs11%70)), legend(order(1 "Male" 2 "Female"))
twoway (hist cnbheur if sx=="1" & cond1==1, color(gs8)) (hist cnbheur if sx=="0" & cond1==1, color(gs11%70)), legend(order(1 "Male" 2 "Female"))

***********************************
*sort within id - year with obs of annual dominant firms
sort nninouv an num1
br nninouv an num sir cnetnetrh debremu finremu startd endd duration dosir num1 dosirq* node1 node2 ee ene