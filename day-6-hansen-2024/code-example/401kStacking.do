*** Code for 401(k) example illustrating HDLMs
capture log close
log using 401kStacking.txt , text replace 

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

* Normalize inc,age,fsize,educ 
replace age = age/64
replace inc = inc/250000
replace fsize = fsize/13
replace educ = educ/18

* Create polynomials
forvalues i = 2/6 {
	gen age`i' = age^`i' 
}

forvalues i = 2/8 {
	gen inc`i' = inc^`i'
}

forvalues i = 2/4 {
	gen educ`i' = educ^`i'
}

gen fsize2 = fsize^2

**************************************************************************
* Stacking over 10 learners

*** Create fold variable for 5 fold CV
* Don't need to do this as pystacked does CV internally, 
* but want to use the same folds as in previous examples
gen cvrand = runiform()
egen foldvar = cut(cvrand) if trdata , group(5)
replace foldvar = foldvar + 1

*** pystacked
* OLS with baseline variables
* OLS with polynomials and interactions
* lasso with polynomials and interactions
pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(ols) || ///
	m(ols) ///
	xvars(c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)## ///
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)) || ///
	m(lassocv) ///
	xvars(c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)## ///
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)) || ///
	m(ridgecv) ///
	xvars(c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)## ///
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)) || ///
	m(rf) opt(n_estimators(500) min_samples_leaf(20) random_state(720)) || ///
	m(gradboost) opt(learning_rate(.1) max_depth(5) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.1) max_depth(3) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(nnet) opt(hidden_layer_sizes(50 10 50) alpha(0) random_state(720)) || ///
	m(nnet) opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720)) || ///
	m(nnet) opt(hidden_layer_sizes(100 100 100 100 100) ///
		alpha(0) random_state(720)) || ///
	if trdata , type(reg) foldvar(foldvar)

pystacked , table holdout

matrix list r(m)

predict yhatcv0 
predict yhat0
predict yhatcv , basexb cv
predict yhat , basexb

*** Fit constant model for reference later
qui reg net_tfa if trdata == 1
predict yhat_c 
gen r_mean2 = (net_tfa - yhat_c)^2
qui sum r_mean2 if trdata == 1
local tssIn = r(mean)
display `RTSS (in) = ' sqrt(`tssIn')
qui sum r_mean2 if trdata == 0
local tssOut = r(mean)
display `RTSS (out) = ' sqrt(`tssOut')

* Tried stacking + 10 learners
forvalues i = 0/10 {
	qui gen rcv`i' = (net_tfa - yhatcv`i')^2
	qui gen r`i' = (net_tfa - yhat`i')^2
	qui sum rcv`i' if trdata == 1
	local esscv`i' = r(mean)
	qui sum r`i' if trdata == 1
	local essIn`i' = r(mean)
	qui sum r`i' if trdata == 0
	local essOut`i' = r(mean)
	local r2cv`i' = 1-(`esscv`i''/`tssIn')
	local r2In`i' = 1-(`essIn`i''/`tssIn')
	local r2Out`i' = 1-(`essOut`i''/`tssOut')
	display "R^2 (in/CV/out) " `i' " = " `r2In`i'' " & " `r2cv`i'' " & " `r2Out`i'' 
}
	
log close