*** Code for abortion example illustrating PLM with fixed effects

capture log close
log using DLfePLM.txt , text replace 

clear all
 
* Load data. Assumes I am in my code subfolder *
import delimited using "..\Data\levitt_ex.dat", clear 

* Drop DC, AK, HI
drop if statenum == 2 | statenum == 9 | statenum == 12

* Restrict to years from Donohue, Levitt paper
keep if year > 84 & year < 98

* Scale some variables
replace xxincome = xxincome/100
replace xxpover = xxpover/100
replace xxafdc15 = xxafdc15/10000
replace xxbeer = xxbeer/100

*** Get baseline results
reghdfe lpc_viol efaviol xx* , absorb(statenum year) cluster(statenum)
reghdfe lpc_prop efaprop xx* , absorb(statenum year) cluster(statenum)
reghdfe lpc_murd efamurd xx* , absorb(statenum year) cluster(statenum)

*** Create expansion
xtset statenum year

* Generate initial conditions and state means
local V "lpc_viol lpc_prop lpc_murd efaviol efaprop efamurd xxprison xxpolice xxunemp xxincome xxpover xxafdc15 xxgunlaw xxbeer"
foreach v of varlist `V' {
	bysort statenum (year): gen `v'0 = `v'[1]
	by statenum: egen `v'bar = mean(`v')
}

gen trend1 = (year-85)/12
gen trend2 = trend1^2
gen trend3 = trend1^3

foreach v of varlist `V' {
	gen tr1X`v'0 = trend1*`v'0
	gen tr2X`v'0 = trend2*`v'0
	gen tr3X`v'0 = trend3*`v'0
	gen tr1X`v'bar = trend1*`v'bar
	gen tr2X`v'bar = trend2*`v'bar
	gen tr3X`v'bar = trend3*`v'bar
}

*** Regression with everything
local X "xx* tr1Xxx* tr2Xxx* tr3Xxx*"
local Xviol "efaviol0 efaviolbar tr1Xefaviol* tr2Xefaviol* tr3Xefaviol*"
local Xprop "efaprop0 efapropbar tr1Xefaprop* tr2Xefaprop* tr3Xefaprop*"
local Xmurd "efamurd0 efamurdbar tr1Xefamurd* tr2Xefamurd* tr3Xefamurd*"

reghdfe lpc_viol efaviol `Xviol' `X' , absorb(statenum year) cluster(statenum)
display "Number of variables: " e(df_m)
reghdfe lpc_prop efaprop `Xprop' `X' , absorb(statenum year) cluster(statenum)
display "Number of variables: " e(df_m)
reghdfe lpc_murd efamurd `Xmurd' `X' , absorb(statenum year) cluster(statenum)
display "Number of variables: " e(df_m)

*** Set seed for reproducibility
set seed 73023

*** Lasso results using plug-in penalization and clustered loadings
pdslasso lpc_viol efaviol (`Xviol' `X' i.year) , fe partial(i.year) cluster(statenum) loptions(xdep)
pdslasso lpc_prop efaprop (`Xprop' `X' i.year) , fe partial(i.year) cluster(statenum) loptions(xdep)
pdslasso lpc_murd efamurd (`Xmurd' `X' i.year) , fe partial(i.year) cluster(statenum) loptions(xdep)

*** Try DML with differenced data
gen Dviol = D.lpc_viol
gen Dprop = D.lpc_prop
gen Dmurd = D.lpc_murd

gen Daviol = D.efaviol
gen Daprop = D.efaprop
gen Damurd = D.efamurd

local xx "xxprison xxpolice xxunemp xxincome xxpover xxafdc15 xxgunlaw xxbeer"

foreach v of varlist `xx' {
	gen L`v' = L.`v'
}
	
global Xv efaviol0 efaviolbar xx* Lxx* trend1
global Xp efaprop0 efapropbar xx* Lxx* trend1
global Xm efamurd0 efamurdbar xx* Lxx* trend1

* Violent crime
ddml init partial, kfolds(5) reps(10) fcluster(statenum)

*** add learners for E[Y|X]
ddml E[Y|X]: reg Dviol efaviol0 efaviolbar xx* Lxx* i.year
ddml E[Y|X]: pystacked Dviol $Xv i.year || method(lassocv)
ddml E[Y|X]: pystacked Dviol $Xv || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(5) random_state(720))
ddml E[Y|X]: pystacked Dviol $Xv || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
ddml E[Y|X]: pystacked Dviol $Xv || method(nnet) ///
	opt(hidden_layer_sizes(100 50 50 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))
ddml E[Y|X]: pystacked Dviol $Xv || method(nnet) ///
	opt(hidden_layer_sizes(100 100 100 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))	

*** add learners for E[D|X]
ddml E[D|X]: reg Daviol efaviol0 efaviolbar xx* Lxx* i.year
ddml E[D|X]: pystacked Daviol $Xv i.year || method(lassocv)
ddml E[D|X]: pystacked Daviol $Xv || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(5) random_state(720))
ddml E[D|X]: pystacked Daviol $Xv || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
ddml E[D|X]: pystacked Daviol $Xv || method(nnet) ///
	opt(hidden_layer_sizes(100 50 50 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))
ddml E[D|X]: pystacked Daviol $Xv || method(nnet) ///
	opt(hidden_layer_sizes(100 100 100 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))	

** cross-fitting
ddml crossfit 
ddml estimate , cluster(statenum) 


* Property crime
ddml init partial, kfolds(5) reps(10) fcluster(statenum)

*** add learners for E[Y|X]
ddml E[Y|X]: reg Dprop efaprop0 efapropbar xx* Lxx* i.year
ddml E[Y|X]: pystacked Dprop $Xp i.year || method(lassocv)
ddml E[Y|X]: pystacked Dprop $Xp || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(5) random_state(720))
ddml E[Y|X]: pystacked Dprop $Xp || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
ddml E[Y|X]: pystacked Dprop $Xp || method(nnet) ///
	opt(hidden_layer_sizes(100 50 50 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))
ddml E[Y|X]: pystacked Dprop $Xp || method(nnet) ///
	opt(hidden_layer_sizes(100 100 100 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))	

*** add learners for E[D|X]
ddml E[D|X]: reg Daprop efaprop0 efapropbar xx* Lxx* i.year
ddml E[D|X]: pystacked Daprop $Xp i.year || method(lassocv)
ddml E[D|X]: pystacked Daprop $Xp || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(5) random_state(720))
ddml E[D|X]: pystacked Daprop $Xp || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
ddml E[D|X]: pystacked Daprop $Xp || method(nnet) ///
	opt(hidden_layer_sizes(100 50 50 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))
ddml E[D|X]: pystacked Daprop $Xp || method(nnet) ///
	opt(hidden_layer_sizes(100 100 100 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))	

** cross-fitting
ddml crossfit 
ddml estimate , cluster(statenum) 


* Murder
ddml init partial, kfolds(5) reps(10) fcluster(statenum)

*** add learners for E[Y|X]
ddml E[Y|X]: reg Dmurd efamurd0 efamurdbar xx* Lxx* i.year
ddml E[Y|X]: pystacked Dmurd $Xm i.year || method(lassocv)
ddml E[Y|X]: pystacked Dmurd $Xm || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(5) random_state(720))
ddml E[Y|X]: pystacked Dmurd $Xm || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
ddml E[Y|X]: pystacked Dmurd $Xm || method(nnet) ///
	opt(hidden_layer_sizes(100 50 50 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))
ddml E[Y|X]: pystacked Dmurd $Xm || method(nnet) ///
	opt(hidden_layer_sizes(100 100 100 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))	

*** add learners for E[D|X]
ddml E[D|X]: reg Damurd efamurd0 efamurdbar xx* Lxx* i.year
ddml E[D|X]: pystacked Damurd $Xm i.year || method(lassocv)
ddml E[D|X]: pystacked Damurd $Xm || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(5) random_state(720))
ddml E[D|X]: pystacked Damurd $Xm || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
ddml E[D|X]: pystacked Damurd $Xm || method(nnet) ///
	opt(hidden_layer_sizes(100 50 50 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))
ddml E[D|X]: pystacked Damurd $Xm || method(nnet) ///
	opt(hidden_layer_sizes(100 100 100 100) alpha(0) random_state(720) ///
	validation_fraction(.2) n_iter_no_change(5) max_iter(1000))	

** cross-fitting
ddml crossfit 
ddml estimate , cluster(statenum) 



log close



