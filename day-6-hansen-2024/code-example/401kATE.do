*** Code for 401(k) example illustrating inference in heterogeneous effect model

capture log close
log using 401kATE.txt , text replace 

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
global D e401

*** Call ddml package and tell it we want to estimate the interactive model (heterogeneous effects)
*** using cross-fitting with five folds and with ten different random splits
*** of five folds

ddml init interactive, kfolds(5) reps(10)

*** add learners for E[Y|D,X]
ddml E[Y|X,D]: reg $Y $X
ddml E[Y|X,D]: reg $Y $Xpoly
ddml E[Y|X,D]: pystacked $Y c.($X)##c.($X) || method(lassocv)
ddml E[Y|X,D]: pystacked $Y c.($X)##c.($X) || method(ridgecv)
ddml E[Y|X,D]: pystacked $Y $X || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
ddml E[Y|X,D]: pystacked $Y $X || method(nnet) ///
	opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720))

*** add learners for E[D|X]
ddml E[D|X]: logit $D $X
ddml E[D|X]: pystacked $D $Xpoly || method(logit) , type(class)
ddml E[D|X]: pystacked $D c.($X)##c.($X) || method(lassocv) , type(class)
ddml E[D|X]: pystacked $D c.($X)##c.($X) || method(ridgecv) , type(class)
ddml E[D|X]: pystacked $D $X || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720)) , type(class)
ddml E[D|X]: pystacked $D $X || method(nnet) ///
	opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720)) , type(class)

** get info on model specification
ddml describe

** cross-fitting
ddml crossfit , shortstack

*** Estimation results
ddml estimate, robust

*** More estimation results
ddml estimate, allcombos robust

*** Look at out of sample performance 
ddml extract, show(mse)

*** Look at stacking weights
ddml extract, show(ssweights)

*** Look at overlap plot for stacked propensity score
ddml overlap , pslist(D_e401_ss)
graph export ..\Slides\figures\Stata_401kStackedPS.png , replace
graph close

*** Save data including predicted values and fold variable for use in estimating 
*** heterogeneous effects
save "..\Data\401kATE.dta" , replace

timer off 1
timer list
timer clear

log close



