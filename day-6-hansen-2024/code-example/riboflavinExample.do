*** Code for riboflavin example
capture log close
log using riboflavin.txt , text replace 

clear all

* Load data. Assumes I am in my code subfolder
import delimited using "..\Data\riboflavin.csv", clear 

/*
* cvlasso here is REALLY slow
cvlasso y aadk_at-zur_at , nfolds(10) seed(727) 
local lam = e(lopt)
display "cv lambda : " `lam'
*cv lambda : 4.292581

lasso2 y aadk_at-zur_at , lambda(`lam')
*/
lasso2 y aadk_at-zur_at , lambda(4.292581)
matrix bcv = e(betaAll)
matrix bcv = bcv'

rlasso y aadk_at-zur_at , robust seed(72723) lassopsi xdep
matrix b1 = e(betaAll)
matrix b1 = b1'

svmat bcv
svmat b1

replace bcv = abs(bcv)
replace b1 = abs(b1)

gen vnum = _n

twoway (scatter b1 vnum) (scatter bcv vnum , color(red)) if vnum < _N , ///
	legend(label(1 "Plug-in") label(2 "CV") pos(2) ring(0)) xtitle("Var Number") ///
	ytitle("|b{sub:j}|") title("Absolute value of lasso coefficients")
graph export ..\Slides\figures\Stata_RiboflavinCoef.png , replace
graph close
	
log close
