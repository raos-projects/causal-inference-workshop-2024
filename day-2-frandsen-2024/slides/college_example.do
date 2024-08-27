capture log close
local output college_example
log using `output'.log,replace text
set matsize 11000
/*
college_example.do implement matching/propensity/regression
methods to estimate the effect of attending a selective college
*/

use college_example,clear

* make a scatter

preserve
collapse owninc avgsatm ivyplus (count) nobs=owninc,by(inst)
qui tab nobs,matrow(nobsmat)
local ninst=r(N)
qui sum nobs
local nobsmax=r(max)
local plots
local plotsivy
forvalues j = 1/`ninst' {
	local nobs=nobsmat[`j',1]
	local shade = ceil(20+80*nobs[`j',1]/`nobsmax')
	local plots `plots' (scatter owninc avgsatm if ivyplus==1 & nobs==`nobs',mlabel(inst) mlabcolor(dkorange%`shade') msymbol(none))(scatter owninc avgsatm if ivyplus==0 & nobs==`nobs',mlabel(inst) mlabcolor(stblue%`shade') msymbol(none))
	local plotsivy `plotsivy' (scatter owninc ivyplus if ivyplus==1 & nobs==`nobs',mlabel(inst) mlabcolor(dkorange%`shade') msymbol(none))(scatter owninc ivyplus if ivyplus==0 & nobs==`nobs',mlabel(inst) mlabcolor(stblue%`shade') msymbol(none))
}

twoway `plots',legend(off) ytitle("Income ($)") xtitle("School average SAT/100")

graph export `output'scatter.pdf,replace

twoway `plotsivy',legend(off) ytitle("Income ($)") xtitle("Ivy plus") xlabel(0(1)1) xscale(range(0 1.2))
graph export `output'binaryscatter.pdf,replace
restore

* first simple comparison
sum owninc if ivyplus==1
sum owninc if ivyplus==0

reg owninc ivyplus,robust

* show noncomparability (only SAT for now)
sum sat100 if ivyplus==1
local s1=r(Var)
local m1=r(mean)
sum sat100 if ivyplus==0
local s0 = r(Var)
local m0 = r(mean)

* display normalized difference (Imbens 2015)
display "Normalized imbalance in SAT: "
display (`m1'-`m0')/sqrt((`s1'+`s0')/2)


twoway (hist sat100 if ivyplus==1,fcolor(dkorange)) ///
	(hist sat100 if ivyplus==0,fcolor(navy)), ///
	legend(order(1 "Ivy + " 2 "Less selective")) ///
	ytitle("") title("SAT Scores by Treatment Status")
graph export `output'sathist.pdf,as(pdf) replace

* a simple formal test
reg sat100 ivyplus,robust

* choose what to match on
* Just to illustrate, SAT score category
gen satcat=0
foreach cutoff in 8 10 12 14 {
	replace satcat=satcat+1 if sat100>=`cutoff'
}

tab satcat

* First matching:
foreach  c in 0 1 2 3 4 {
	reg owninc ivyplus if satcat==`c'
	local beta`c' = _b[ivyplus]
	local n`c' = e(N)
}
display "averaged matched estimate:" 
display (`beta0'*`n0'+`beta1'*`n1'+`beta2'*`n2'+`beta3'*`n3'+`beta4'*`n4')/_N

* now by reweighting by propensity:
logit ivyplus i.satcat
predict phat
gen invpwt = 1/phat
replace invpwt = 1/(1-phat) if ivyplus==0

sum owninc [w=invpwt] if ivyplus==1
sum owninc [w=invpwt] if ivyplus==0
* (or via regression just to get the difference in means):
reg owninc ivyplus [w=invpwt]

* show balance when re-weighted!
sum satcat if ivyplus==0 [w=invpwt]
sum satcat if ivyplus==1 [w=invpwt]

* show you can also get this by regression:
reg owninc i.satcat if ivyplus==0
predict mu0
reg owninc i.satcat if ivyplus==1
predict mu1
gen mudiff=mu1-mu0
sum mudiff

* or in one step:
xi i.satcat
foreach var of varlist _Isatcat* {
	sum `var'
	gen d`var'=ivyplus*(`var'-r(mean))
}

reg owninc ivyplus d_Isatcat* _Isatcat*,robust

* now combining propensity and regression
* and double-robust
gen invp1=ivyplus/phat
gen invp0=(1-ivyplus)/(1-phat)

gen term1 = mu1+ivyplus*(owninc-mu1)/phat
gen term0 = mu0+(1-ivyplus)*(owninc-mu0)/(1-phat)

sum term1
sum term0

* That does them all from scratch in the simple case. 

*Can also use canned command teffects
teffects nnmatch (owninc _Isatcat*) (ivyplus)
teffects ipw (owninc) (ivyplus _Isatcat*,probit),iterate(20)
teffects ra (owninc _Isatcat*) (ivyplus)
teffects aipw (owninc _Isatcat*) (ivyplus _Isatcat*)



* Is the coarse adjustment good enough? 

* show non-representativeness
local pretreats female black hispanic asian othrace linchat sat100 hstopten ath

foreach var of varlist `pretreats' {
	sum `var' if ivyplus==1
	local m1=r(mean)
	local v1=r(Var)
	sum `var' if ivyplus==0
	local m0 = r(mean)
	local v0 = r(Var)
	display "Normalized difference for `var': "
	display (`m1'-`m0')/sqrt((`v1'+`v0')/2)
}

* formally test lack of balance
local testspec
foreach var in `pretreats' {
	reg `var' ivyplus _Isat* d_Isat*
	estimates store `var'
	local testspec `testspec' _b[`var'_mean:ivyplus] = 
}

suest `pretreats',vce(robust)
test `testspec' 0

* Solution: control for all these variables
teffects ipw (owninc) (ivyplus `pretreats',probit),iterate(20)

teffects ra (owninc `pretreats') (ivyplus),iterate(20)
reg owninc ivyplus `pretreats',robust

* slightly different answers--worried about misspecification of regression or propensity score?
* try doubly robust!
* first "by hand"
drop term* mu*
probit ivyplus `pretreats'
predict pscore
reg owninc `pretreats' if ivyplus==1
predict mu1
reg owninc `pretreats' if ivyplus==0
predict mu0
gen term1 = mu1+ivyplus*(owninc-mu1)/pscore
gen term0 = mu0+(1-ivyplus)*(owninc-mu0)/(1-pscore)

sum term1
sum term0

* or use canned command!
teffects aipw (owninc `pretreats') (ivyplus `pretreats',probit),iterate(20)

* these all give similar answers even to the simple approach, so encouraging. Can we be sure?
* we can check balance using the propensity score weights:
local testspec
foreach var in `pretreats' {
	reg `var' ivyplus [w=ivyplus/pscore + (1-ivyplus)/(1-pscore)]
	estimates store `var'
	local testspec `testspec' _b[`var'_mean:ivyplus] = 
}
suest `pretreats',vce(robust)
test `testspec' 0

* nice! Suggests that the linear pscore model might be ok

* assess assumptions
*common support
twoway histogram sat100 if ivyplus==1,name(hist1,replace)
twoway histogram sat100 if ivyplus==0,name(hist0,replace)

graph combine hist1 hist0,rows(2) xcommon title("SAT Score Distribution by Treatment Status")

twoway histogram pscore,title("Estimated Propensity Score Distribution")
* there are some propensity scores close to zero, so could consider trimming

* test conditional independence
local testvars oneapp twoapp thrapp fourapp avgsatm
local testspec
foreach var in `testvars' {
	reg `var' ivyplus [w=ivyplus/pscore + (1-ivyplus)/(1-pscore)]
	estimates store `var'
	local testspec `testspec' _b[`var'_mean:ivyplus] = 
}
suest `testvars',vce(robust)
test `testspec' 0

* solution: add them to the control set!
local X `pretreats' `testvars' avgsatm
teffects aipw (owninc `X') (ivyplus `X'),iterate(20)

* Finally, propensity score subclassification + regression
logit ivyplus `X'
drop phat
predict phat
sum phat
* trim between .1 and .9
keep if .1<=phat & phat<=.9

* create 5 blocks based on p-score
sort phat
gen block = 1+ floor(5*(_n-1)/_N)
tab block

local sumdhat = 0
local sumvhat = 0
forvalues j = 1/5 {
	reg owninc ivyplus `X' if block==`j',r
	local sumdhat=`sumdhat'+_b[ivyplus]
	local sumvhat = `sumvhat'+_se[ivyplus]^2
}

* average to get final estimator:
display "final estimate: " `sumdhat'/5
display "standard error: " sqrt(`sumvhat'/25)

log close
