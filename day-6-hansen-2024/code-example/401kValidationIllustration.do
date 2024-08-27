*** Code for 401(k) example illustrating model assessment via validation
capture log close
log using 401kValidationIllustration.txt , text replace
clear all
 
use "..\Data\sipp1991.dta", clear /* Load data. Assumes I am in my code subfolder */

*** Set seed for reproducibility
set seed 71423

*** Split data into a training (~ 80%) and test (~ 20%) set
gen tr = runiform()
replace tr = tr < .8

* Sample sizes
count if tr
count if !tr

*****************************************************************************
/* Basic models in training data */

*** Scatter plot
twoway (scatter net_tfa inc) if tr
graph export ..\Slides\figures\Stata_ScPlNetTFA.png , replace
graph close

*** Fit constant model for reference later
qui reg net_tfa if tr
predict yhat_c 
gen r_mean = net_tfa - yhat_c
gen r_mean2 = r_mean^2
qui sum r_mean2 if tr
local tssIn = r(mean)
qui sum r_mean2 if !tr
local tssOut = r(mean)

*** Linear model and plot
reg net_tfa inc if tr
predict yhat_lm
twoway (scatter net_tfa inc) (lfit net_tfa inc) if tr , legend(off)
graph export ..\Slides\figures\Stata_LMNetTFA.png , replace
graph close

* RMSEs and out-of-sample R^2
* Undoubtedly a better way to do this
* Could at least put in a program
gen r_lm = net_tfa - yhat_lm
gen r_lm2 = r_lm^2
qui sum r_lm2 if tr
display "LM RMSE = " sqrt(r(mean))
qui sum r_lm2 if !tr
display "LM RMSE out-of-sample = " sqrt(r(mean))
qui sum r_lm2 if !tr
local essOut_lm = r(mean)
local r2Out_lm = 1-(`essOut_lm'/`tssOut')
display "LM R^2 out-of-sample = " `r2Out_lm' 

*** Kernel regression and plot
* Brute force knn regression with uniform kernel and k = 1. Really slow.
gen yhat_kern = . 
local n = _N
forvalues i = 1/`n' {
	capture drop tmpdis tmprank tmpww tmpwwy tmpnum tmpden tmpknn
	gen tmpdis = abs(inc - inc[`i']) 
	qui replace tmpdis = 1e10 if !tr
	egen tmprank = rank(tmpdis) , track
	gen tmpww = tmprank < 2
	gen tmpwwy = tmpww*net_tfa
	egen tmpnum = total(tmpwwy)
	egen tmpden = total(tmpww)
	gen tmpknn = tmpnum/tmpden
	qui replace yhat_kern = tmpknn in `i'
}
twoway (scatter net_tfa inc) (line yhat_kern inc , sort lcolor(red)) if tr , legend(off)
graph export ..\Slides\figures\Stata_KernNetTFA.png , replace
graph close

capture drop tmpdis tmprank tmpww tmpwwy tmpnum tmpden tmpknn

* Compute in and out-of-sample R^2. 
* Undoubtedly a better way to do this.
* Could at least put in a program
gen r_knn = net_tfa - yhat_kern 
gen r_knn2 = r_knn^2
qui sum r_knn2 if tr
local essIn_knn = r(mean)
display "KNN RMSE = " sqrt(r(mean))
local r2In_knn = 1-(`essIn_knn'/`tssIn')
display "KNN R^2 = " `r2In_knn' 
qui sum r_knn2 if !tr
local essOut_knn = r(mean)
display "KNN RMSE out-of-sample = " sqrt(r(mean))
local r2Out_knn = 1-(`essOut_knn'/`tssOut')
display "KNN R^2 out-of-sample = " `r2Out_knn' 

*** Cubic polynomial regression
gen inc2 = inc^2
gen inc3 = inc^3
reg net_tfa inc inc2 inc3 if tr
predict yhat_poly
twoway (scatter net_tfa inc) (line yhat_poly inc , sort lcolor(red)) if tr , legend(off)
graph export ..\Slides\figures\Stata_PolyNetTFA.png , replace
graph close

* RMSEs and out-of-sample R^2
* Undoubtedly a better way to do this
* Could at least put in a program
gen r_poly = net_tfa - yhat_poly
gen r_poly2 = r_poly^2
qui sum r_poly2 if tr
display "Poly RMSE = " sqrt(r(mean))
qui sum r_poly2 if !tr
display "Poly RMSE out-of-sample = " sqrt(r(mean))
qui sum r_poly2 if !tr
local essOut_poly = r(mean)
local r2Out_poly = 1-(`essOut_poly'/`tssOut')
display "Poly R^2 out-of-sample = " `r2Out_poly' 

*****************************************************************************
/* Brute force cross-validation */

*** Create fold variable for 5 fold CV
gen cvrand = runiform()
egen foldvar = cut(cvrand) if tr , group(5)
replace foldvar = foldvar + 1

*** Loop over folds and compute our three prediction rules

forvalues k = 1/5 {
	display "CV fold: " `k'
	
	gen trobs`k' = foldvar != `k' if tr

	* Constant 
	qui reg net_tfa if trobs`k' == 1
	qui predict yhat_c`k' 

	* Linear Model
	qui reg net_tfa inc if trobs`k' == 1
	qui predict yhat_lm`k'
	
	* Cubic polynomial
	qui reg net_tfa inc inc2 inc3 if trobs`k' == 1
	qui predict yhat_poly`k'
	
	* KNN with 1 neighbor
	gen yhat_kern`k' = . 
	local n = _N
	forvalues i = 1/`n' {
		capture drop tmpdis tmprank tmpww tmpwwy tmpnum tmpden tmpknn
		gen tmpdis = abs(inc - inc[`i']) 
		qui replace tmpdis = 1e10 if trobs`k' != 1
		egen tmprank = rank(tmpdis) , track
		gen tmpww = tmprank < 2
		gen tmpwwy = tmpww*net_tfa
		egen tmpnum = total(tmpwwy)
		egen tmpden = total(tmpww)
		gen tmpknn = tmpnum/tmpden
		qui replace yhat_kern`k' = tmpknn in `i'
	}		
}

* Generate squared errors
forvalues k = 1/5 {
	gen r_c2_`k' = (net_tfa - yhat_c`k')^2
	gen r_lm2_`k' = (net_tfa - yhat_lm`k')^2
	gen r_knn2_`k' = (net_tfa - yhat_kern`k')^2
	gen r_poly2_`k' = (net_tfa - yhat_poly`k')^2
}

* Generate RMSEs for each fold
forvalues k = 1/5 {
	qui sum r_lm2_`k' if foldvar == `k'
	local RMSE_lm_`k' = sqrt(r(mean))
	qui sum r_knn2_`k' if foldvar == `k'
	local RMSE_knn_`k' = sqrt(r(mean))
	qui sum r_poly2_`k' if foldvar == `k'
	local RMSE_poly_`k' = sqrt(r(mean))
	display "Fold " `k' " & " `RMSE_lm_`k'' " & " `RMSE_knn_`k'' " & " `RMSE_poly_`k'' " \\"
}
	
* Overall RMSE
gen cv_lm = r_lm2_1
replace cv_lm = r_lm2_2 if foldvar == 2
replace cv_lm = r_lm2_3 if foldvar == 3
replace cv_lm = r_lm2_4 if foldvar == 4
replace cv_lm = r_lm2_5 if foldvar == 5
replace cv_lm = . if !tr
qui sum cv_lm
local cv_lm = sqrt(r(mean))

gen cv_knn = r_knn2_1
replace cv_knn = r_knn2_2 if foldvar == 2
replace cv_knn = r_knn2_3 if foldvar == 3
replace cv_knn = r_knn2_4 if foldvar == 4
replace cv_knn = r_knn2_5 if foldvar == 5
replace cv_knn = . if !tr
qui sum cv_knn
local cv_knn = sqrt(r(mean))

gen cv_poly = r_poly2_1	
replace cv_poly = r_poly2_2 if foldvar == 2
replace cv_poly = r_poly2_3 if foldvar == 3
replace cv_poly = r_poly2_4 if foldvar == 4
replace cv_poly = r_poly2_5 if foldvar == 5
replace cv_poly = . if !tr
qui sum cv_poly
local cv_poly = sqrt(r(mean))

display "Overall CV RMSE: & " `cv_lm' " & " `cv_knn' " & " `cv_poly' " \\" 

* Computed out-of-sample performance on the basis of using all training data in 
* the first code block. Here we will use the average of the CV predictions as
* the prediction rule and compute out-of-sample R^2

gen cv_pred_c = (yhat_c1 + yhat_c2 + yhat_c3 + yhat_c4 + yhat_c5)/5
gen cv_pred_lm = (yhat_lm1 + yhat_lm2 + yhat_lm3 + yhat_lm4 + yhat_lm5)/5
gen cv_pred_knn = (yhat_kern1 + yhat_kern2 + yhat_kern3 + yhat_kern4 + yhat_kern5)/5
gen cv_pred_poly = (yhat_poly1 + yhat_poly2 + yhat_poly3 + yhat_poly4 + yhat_poly5)/5

gen cv_r_c2 = (cv_pred_c - net_tfa)^2
gen cv_r_lm2 = (cv_pred_lm - net_tfa)^2
gen cv_r_knn2 = (cv_pred_knn - net_tfa)^2
gen cv_r_poly2 = (cv_pred_poly - net_tfa)^2

* Out-of-sample R^2 relative to using constant fit on full training data
qui sum r_mean2 if !tr
local tssOut = r(mean)

qui sum cv_r_c2 if !tr
local essOut_cv_c = r(mean)
qui sum cv_r_lm2 if !tr
local essOut_cv_lm = r(mean)
qui sum cv_r_knn2 if !tr
local essOut_cv_knn = r(mean)
qui sum cv_r_poly2 if !tr
local essOut_cv_poly = r(mean)
local r2Out_cvlm = 1-(`essOut_cv_lm'/`tssOut')
local r2Out_cvknn = 1-(`essOut_cv_knn'/`tssOut')
local r2Out_cvpoly = 1-(`essOut_cv_poly'/`tssOut')
display "R^2 out-of-sample = & " `r2Out_cvlm' " & " `r2Out_cvknn' " & " `r2Out_cvpoly' " \\ "


log close







