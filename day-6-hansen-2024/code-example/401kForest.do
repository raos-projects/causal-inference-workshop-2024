*** Code for 401(k) example illustrating forests
*** Uses the rforest package: ssc install rforest
capture log close
log using 401kForest.txt , text replace 

clear all
 
use "..\Data\sipp1991.dta", clear /* Load data. Assumes I am in my code subfolder */

*** Set seed for reproducibility
set seed 71423

*** Split data into a training (~80%) and test (~20%) set
gen trdata = runiform()
replace trdata = trdata < .8

* Sample sizes
count if trdata
count if !trdata

*************************************************************************
* Look at out of bag error estimation. Follows example in "The Random Forest Algorithm for Statistical Learning" Schonlau and Zou
* This block of code is slow
gen oobError = .
gen iterNum = .

local j = 0
forvalues i = 1(20)2001 {
	local j = `j' + 1
	display "Trees = " `i'
	rforest net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	        type(reg) iter(`i') seed(720)
	* rforest defaults to using \sqrt{p} regressors at each step
	qui replace oobError = `e(OOB_Error)' in `j'
	qui replace iterNum = `i' in `j'
}
twoway (line oobError iterNum , lcolor(blue)) , legend(off)
graph export ..\Slides\figures\Stata_OOB.png , replace
graph close

display "RMSE Default = " `e(OOB_Error)'

******************************************************************************
* Look at OOB and validation error for a few different tuning choices

* Model we just estimated (2001 trees), Stata default tuning
predict yhat1
gen r2_1 = (net_tfa - yhat1)^2

* Things look stable after ~750 trees, just use 1000 from here on

* 1) Use all variables
rforest net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	        type(reg) iter(1000) seed(720) numvars(10)
predict yhatA
gen r2_A = (net_tfa - yhatA)^2

display "RMSE All = " `e(OOB_Error)'

* 2) min leaf size = 20
rforest net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	        type(reg) iter(1000) seed(720) lsize(20)
predict yhat20
gen r2_20 = (net_tfa - yhat20)^2

display "RMSE 20 = " `e(OOB_Error)'

* 3) min leaf size = 40
rforest net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	        type(reg) iter(1000) seed(720) lsize(40)
predict yhat40
gen r2_40 = (net_tfa - yhat40)^2

display "RMSE 40 = " `e(OOB_Error)'

* 4) min leaf size = 80
rforest net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	        type(reg) iter(1000) seed(720) lsize(80)
predict yhat80
gen r2_80 = (net_tfa - yhat80)^2

display "RMSE 80 = " `e(OOB_Error)'
			
* 5) min leaf size = 160
rforest net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	        type(reg) iter(1000) seed(720) lsize(160)
predict yhat160
gen r2_160 = (net_tfa - yhat160)^2

display "RMSE 160 = " `e(OOB_Error)'			

* Constant model
qui reg net_tfa if trdata
predict yhat_constant	
gen r2_constant = (net_tfa - yhat_constant)^2

* Get R^2s
qui sum r2_constant if !trdata
local tssOut = r(mean)

qui sum r2_1 if !trdata
local essOut_1 = r(mean)
local r2Out_1 = 1-(`essOut_1'/`tssOut')
display "R^2 Default:" `r2Out_1'

qui sum r2_A if !trdata
local essOut_A = r(mean)
local r2Out_A = 1-(`essOut_A'/`tssOut')
display "R^2 All:" `r2Out_A'

qui sum r2_20 if !trdata
local essOut_20 = r(mean)
local r2Out_20 = 1-(`essOut_20'/`tssOut')
display "R^2 20:" `r2Out_20'

qui sum r2_40 if !trdata
local essOut_40 = r(mean)
local r2Out_40 = 1-(`essOut_40'/`tssOut')
display "R^2 40:" `r2Out_40'

qui sum r2_80 if !trdata
local essOut_80 = r(mean)
local r2Out_80 = 1-(`essOut_80'/`tssOut')
display "R^2 80:" `r2Out_80'

qui sum r2_160 if !trdata
local essOut_160 = r(mean)
local r2Out_160 = 1-(`essOut_160'/`tssOut')
display "R^2 160:" `r2Out_160'

log close
