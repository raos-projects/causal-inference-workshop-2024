*** Code for 401(k) example illustrating inference in heterogeneous effect IV model

capture log close
log using 401kLATE.txt , text replace 

clear all

timer on 1
 
* Load data. Assumes I am in my code subfolder *
use "..\Data\sipp1991.dta", clear 

*** Set seed for reproducibility
set seed 71423

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

*** Define some macros for our variables
global Y net_tfa
global X age inc educ fsize marr twoearn db pira hown
global Xpoly age age2 age3 age4 age5 age6 inc inc2 inc3 inc4 inc5 inc6 inc7 ///
	inc8 educ educ2 educ3 educ4 fsize fsize2 marr twoearn db pira hown
global Z e401
global D p401

*** Call ddml package and tell it we want to estimate the interactive model (heterogeneous effects)
*** using cross-fitting with five folds and with ten different random splits
*** of five folds

ddml init interactiveiv, kfolds(5) reps(10)

*** add learners for E[Y|X] using full stacking via pystacked
ddml E[Y|X,Z]: pystacked $Y $X || method(ols) ///
	|| method(ols) xvars($Xpoly) ///
	|| method(lassocv) xvars(c.($X)##c.($X)) ///
	|| method(ridgecv) xvars(c.($X)##c.($X)) ///
    || method(rf) opt(n_estimators(500) min_samples_leaf(20) random_state(720)) ///
	|| method(nnet) opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720)), ///
	njobs(4)

*** add learners for E[D|X] using full stacking via pystacked
ddml E[D|X,Z]: pystacked $D $X || method(logit) ///
	|| method(logit) xvars($Xpoly) ///
	|| method(lassocv) xvars(c.($X)##c.($X)) ///
	|| method(ridgecv) xvars(c.($X)##c.($X)) ///
    || method(rf) opt(n_estimators(500) min_samples_leaf(20) random_state(720)) ///
	|| method(nnet) opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720)), ///
	njobs(4) type(class)

*** add learners for E[Z|X] using full stacking via pystacked
ddml E[Z|X]: pystacked $Z $X || method(logit) ///
	|| method(logit) xvars($Xpoly) ///
	|| method(lassocv) xvars(c.($X)##c.($X)) ///
	|| method(ridgecv) xvars(c.($X)##c.($X)) ///
    || method(rf) opt(n_estimators(500) min_samples_leaf(20) random_state(720)) ///
	|| method(nnet) opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720)), ///
	njobs(4) type(class)
	
** get info on model specification
ddml describe

** cross-fitting
ddml crossfit 

*** Estimation results
ddml estimate, allcombos robust

timer off 1
timer list
timer clear

log close

