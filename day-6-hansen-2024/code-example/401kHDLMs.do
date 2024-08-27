*** Code for 401(k) example illustrating HDLMs
capture log close
log using 401kHDLMs.txt , text replace 

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

**********************************************************************
*** Linear models in training data
* Baseline
qui reg net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata
display "Base number of variables: " e(df_m)
predict yhat_ols_base	
	
* Polynomial
qui reg net_tfa e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 ///
	inc6 inc7 inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown ///
	if trdata
display "Base number of variables: " e(df_m)
predict yhat_ols_poly	

* Interacted
qui reg net_tfa /// 
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)## ///
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown) ///
	if trdata
display "Base number of variables: " e(df_m)
predict yhat_ols_int	
	
************************************************************************
*** Spaghetti plots for ridge and lasso in polynomial model
* Lasso
lasso2 net_tfa e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 ///
	inc6 inc7 inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown ///
	if trdata, plotpath(lambda) plotopt(legend(off) ylabel(none)) ///
	lambda(30010(250)10) lglmnet  /* lglmnet option makes lambda on same scale as glmnet */
graph export ..\Slides\figures\Stata_lassoSpaghetti.png , replace
graph close
	
* Ridge	
lasso2 net_tfa e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 ///
	inc6 inc7 inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown ///
	if trdata, plotpath(lambda) plotopt(legend(off) ylabel(none)) alpha(0) ///
	lambda(1001000(10000)1000) 
graph export ..\Slides\figures\Stata_ridgeSpaghetti.png , replace
graph close

****************************************************************************
*** Cross-validation for Lasso and Ridge
* Use 5-fold CV here

*** Baseline model
* Lasso
* Seed option controls the randomized split into folds (replicable).
* Last option saves the fold variable as fvar for use all the way along
cvlasso net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	lglmnet nfolds(5) seed(718) savefoldvar(fvar)	
* When you run this - you get that the optimal lambda is at the end of the range.
* Try again with a longer lambda sequence
cvlasso net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	lglmnet foldvar(fvar) lambda(24010(50)10) 
	/* Reusing the folds from the first run */

local lbaseRMSE = sqrt(e(mspemin))
display "Baseline - Lasso CV " `lbaseRMSE'
	
cvlasso, plotcv /* Cross validation plot */
graph export ..\Slides\figures\Stata_LassoBasicCV.png , replace
graph close
	
* Store the model.
est store cv_lasso_base
* get the predictions (in-sample and out-of-sample)
predict yhat_lasso_base, lopt

* Ridge - use same folds
cvlasso net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	foldvar(fvar) alpha(0) lambda(24010(50)10) plotcv
graph export ..\Slides\figures\Stata_RidgeBasicCV.png , replace
graph close	

local rbaseRMSE = sqrt(e(mspemin))
display "Baseline - Ridge CV " `rbaseRMSE'

* Store the model.
est store cv_ridge_base
* get the predictions (in-sample and out-of-sample)
predict yhat_ridge_base, lopt

* Trick to get essentially OLS results for CV comparison	
* Do lasso with a tiny penalty parameter
cvlasso net_tfa e401 age inc educ fsize marr twoearn db pira hown if trdata, ///
	lglmnet foldvar(fvar) lminratio(.0000000000001) lcount(2)

matrix mspe = e(mmspe)
local mspebase = sqrt(mspe[2,1])
display "Baseline - OLS CV " `mspebase'
matrix drop mspe

*** Polynomial model
* Lasso
cvlasso net_tfa e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 ///
	inc6 inc7 inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown ///
	if trdata, lglmnet foldvar(fvar) plotcv
graph export ..\Slides\figures\Stata_LassoPolyCV.png , replace
graph close

local lpolyRMSE = sqrt(e(mspemin))
display "Polynomial - Lasso CV " `lpolyRMSE'

* Store the model.
est store cv_lasso_poly
* get the predictions (in-sample and out-of-sample)
predict yhat_lasso_poly, lopt

* Ridge
cvlasso net_tfa e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 ///
	inc6 inc7 inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown ///
	if trdata, lglmnet foldvar(fvar) plotcv alpha(0)
graph export ..\Slides\figures\Stata_RidgePolyCV.png , replace
graph close

local rpolyRMSE = sqrt(e(mspemin))
display "Polynomial - Lasso CV " `rpolyRMSE'

* Store the model.
est store cv_ridge_poly
* get the predictions (in-sample and out-of-sample)
predict yhat_ridge_poly, lopt

* Trick to get essentially OLS results for CV comparison	
* Do lasso with a tiny penalty parameter
cvlasso net_tfa e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 ///
	inc6 inc7 inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown /// 
	if trdata, lglmnet foldvar(fvar) lminratio(.0000000000001) lcount(2)

matrix mspe = e(mmspe)
local mspepoly = sqrt(mspe[2,1])
display "Polynomial - OLS CV " `mspepoly'
matrix drop mspe

*** Interaction model
* Lasso
cvlasso net_tfa /// 
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)## ///
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown) ///
	if trdata, lglmnet foldvar(fvar) plotcv
graph export ..\Slides\figures\Stata_LassoIntCV.png , replace
graph close

local lintRMSE = sqrt(e(mspemin))
display "Interactions - Lasso CV " `lintRMSE'

* Store the model.
est store cv_lasso_int
* get the predictions (in-sample and out-of-sample)
predict yhat_lasso_int, lopt

* Ridge
cvlasso net_tfa /// 
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)## ///
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown) ///
	if trdata, lglmnet foldvar(fvar) plotcv alpha(0)
graph export ..\Slides\figures\Stata_RidgeIntCV.png , replace
graph close

local rintRMSE = sqrt(e(mspemin))
display "Interactions - Ridge CV " `rintRMSE'

* Store the model.
est store cv_ridge_int
* get the predictions (in-sample and out-of-sample)
predict yhat_ridge_int, lopt

* Trick to get essentially OLS results for CV comparison	
* Do lasso with a tiny penalty parameter
cvlasso net_tfa /// 
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown)## ///
	c.(e401 age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown) ///
	if trdata, lglmnet foldvar(fvar) lminratio(.0000000000001) lcount(2)

matrix mspe = e(mmspe)
local mspeint = sqrt(mspe[2,1])
display "Interactions - OLS CV " `mspeint'
	

****************************************************************************
* Out of sample R^2
gen r2_ols_base = (net_tfa - yhat_ols_base)^2
gen r2_ols_poly = (net_tfa - yhat_ols_poly)^2
gen r2_ols_int = (net_tfa - yhat_ols_int)^2

gen r2_lasso_base = (net_tfa - yhat_lasso_base)^2
gen r2_lasso_poly = (net_tfa - yhat_lasso_poly)^2
gen r2_lasso_int = (net_tfa - yhat_lasso_int)^2

gen r2_ridge_base = (net_tfa - yhat_ridge_base)^2
gen r2_ridge_poly = (net_tfa - yhat_ridge_poly)^2
gen r2_ridge_int = (net_tfa - yhat_ridge_int)^2

* Constant model
qui reg net_tfa if trdata
predict yhat_constant	
gen r2_constant = (net_tfa - yhat_constant)^2

* Get R^2s
qui sum r2_constant if !trdata
local tssOut = r(mean)

qui sum r2_ols_base if !trdata
local essOut_ols_base = r(mean)
local r2Out_ols_base = 1-(`essOut_ols_base'/`tssOut')
qui sum r2_ols_poly if !trdata
local essOut_ols_poly = r(mean)
local r2Out_ols_poly = 1-(`essOut_ols_poly'/`tssOut')
qui sum r2_ols_int if !trdata
local essOut_ols_int = r(mean)
local r2Out_ols_int = 1-(`essOut_ols_int'/`tssOut')

display "OLS & " `r2Out_ols_base' " & " `r2Out_ols_poly' " & " `r2Out_ols_int' " \\"

qui sum r2_lasso_base if !trdata
local essOut_lasso_base = r(mean)
local r2Out_lasso_base = 1-(`essOut_lasso_base'/`tssOut')
qui sum r2_lasso_poly if !trdata
local essOut_lasso_poly = r(mean)
local r2Out_lasso_poly = 1-(`essOut_lasso_poly'/`tssOut')
qui sum r2_lasso_int if !trdata
local essOut_lasso_int = r(mean)
local r2Out_lasso_int = 1-(`essOut_lasso_int'/`tssOut')

display "Lasso & " `r2Out_lasso_base' " & " `r2Out_lasso_poly' " & " `r2Out_lasso_int' " \\"

qui sum r2_ridge_base if !trdata
local essOut_ridge_base = r(mean)
local r2Out_ridge_base = 1-(`essOut_ridge_base'/`tssOut')
qui sum r2_ridge_poly if !trdata
local essOut_ridge_poly = r(mean)
local r2Out_ridge_poly = 1-(`essOut_ridge_poly'/`tssOut')
qui sum r2_ridge_int if !trdata
local essOut_ridge_int = r(mean)
local r2Out_ridge_int = 1-(`essOut_ridge_int'/`tssOut')

display "Ridge & " `r2Out_ridge_base' " & " `r2Out_ridge_poly' " & " `r2Out_ridge_int' " \\"

*****************************************************************************
log close

