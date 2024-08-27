*** Code for minimum wage example illustrating inference in heterogeneous effect
*** DiD model

capture log close
log using mwDiD.txt , text replace 

clear all
 
* Load data. Assumes I am in my code subfolder *
import delimited using "..\Data\mwexample.csv", clear 

*** Set seed for reproducibility
set seed 73023

*** Probably a slicker way to do all of this
*** Set up some panel stuff
tsset countyreal year

* Generate initial conditions
bysort countyreal (year): gen lemp0 = lemp[1]
bysort countyreal (year): gen lpop0 = lpop[1]
bysort countyreal (year): gen lpay0 = lavg_pay[1]

list lemp lemp0 lpop lpop0 lavg_pay lpay0 in 1/14

* We are going to look at ATET for observations treated in 2004
* 2003 will serve as the baseline pre-period
bysort countyreal (year): gen lemp03 = lemp[3]
list countyreal year lemp lemp03 in 1/14

* We will look at effect in 2002 (``pre-trend'')
bysort countyreal (year): gen lemp02 = lemp[2]
* We will look at effect in 2004-2007
bysort countyreal (year): gen lemp04 = lemp[4]
bysort countyreal (year): gen lemp05 = lemp[5]
bysort countyreal (year): gen lemp06 = lemp[6]
bysort countyreal (year): gen lemp07 = lemp[7]

* Change in outcome variables
gen dy02 = lemp02-lemp03
gen dy04 = lemp04-lemp03
gen dy05 = lemp05-lemp03
gen dy06 = lemp06-lemp03
gen dy07 = lemp07-lemp03

* Cohort of interest
gen G04 = (g == 2004)

* Standardize our control variables
egen s_lemp0 = std(lemp0)
egen s_lpop0 = std(lpop0)
egen s_lpay0 = std(lpay0)

* At this point, have flattened everything so don't need the panel anymore
* for estimating the effect for the 2004 cohort
* Just drop everything so we're a cross-section again
drop if year < 2007

* Create region dummies, polynomials and interactions
gen s_lemp0_1 = s_lemp0
gen s_lemp0_2 = s_lemp0^2
gen s_lemp0_3 = s_lemp0^3

gen s_lpop0_1 = s_lpop0
gen s_lpop0_2 = s_lpop0^2
gen s_lpop0_3 = s_lpop0^3

gen s_lpay0_1 = s_lpay0
gen s_lpay0_2 = s_lpay0^2
gen s_lpay0_3 = s_lpay0^3

qui tab region , gen(regdum)

*** Control variable sets
global Xr regdum1 regdum2 regdum3 regdum4 
global Xc s_lemp0_1 s_lpop0_1 s_lpay0_1
global Xemp s_lemp0_1 s_lemp0_2 s_lemp0_3
global Xpop s_lpop0_1 s_lpop0_2 s_lpop0_3
global Xpay s_lpay0_1 s_lpay0_2 s_lpay0_3
global D G04

*** Model for 2002 effect
global Y dy02

ddml init interactive if (g == 0 | g > 2002) , mname(m02) kfolds(5) reps(10)

*** add learners for E[Y|D,X]
ddml E[Y|X,D], mname(m02) learner(Y1_02): reg $Y
ddml E[Y|X,D], mname(m02) learner(Y2_02): reg $Y $Xr $Xc
ddml E[Y|X,D], mname(m02) learner(Y3_02): ///
	pystacked $Y $Xr##c.($Xc) || method(lassocv)
ddml E[Y|X,D], mname(m02) learner(Y4_02): ///
	pystacked $Y $Xr##c.($Xc) || method(ridgecv)
ddml E[Y|X,D], mname(m02) learner(Y5_02): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv)
ddml E[Y|X,D], mname(m02) learner(Y6_02): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv)
ddml E[Y|X,D], mname(m02) learner(Y7_02): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720))
ddml E[Y|X,D], mname(m02) learner(Y8_02): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
	
*** add learners for E[D|X]
ddml E[D|X], mname(m02) learner(D1_02): reg $D
ddml E[D|X], mname(m02) learner(D2_02): pystacked $D $Xr $Xc || method(logit) , type(class)
ddml E[D|X], mname(m02) learner(D3_02): ///
	pystacked $D $Xr##c.($Xc) || method(lassocv) , type(class)
ddml E[D|X], mname(m02) learner(D4_02): ///
	pystacked $D $Xr##c.($Xc) || method(ridgecv) , type(class)
ddml E[D|X], mname(m02) learner(D5_02): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv) , type(class)
ddml E[D|X], mname(m02) learner(D6_02): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv) , type(class)
ddml E[D|X], mname(m02) learner(D7_02): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720)) , type(class)
ddml E[D|X], mname(m02) learner(D8_02): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720)) , type(class)

** cross-fitting
ddml crossfit , mname(m02) 

*** Estimation results
ddml estimate, mname(m02) robust atet

*** Model for 2004 effect
global Y dy04

ddml init interactive if (g == 0 | g > 2003) , mname(m04) kfolds(5) reps(10)

*** add learners for E[Y|D,X]
ddml E[Y|X,D], mname(m04) learner(Y1_04): reg $Y
ddml E[Y|X,D], mname(m04) learner(Y2_04): reg $Y $Xr $Xc
ddml E[Y|X,D], mname(m04) learner(Y3_04): ///
	pystacked $Y $Xr##c.($Xc) || method(lassocv)
ddml E[Y|X,D], mname(m04) learner(Y4_04): ///
	pystacked $Y $Xr##c.($Xc) || method(ridgecv)
ddml E[Y|X,D], mname(m04) learner(Y5_04): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv)
ddml E[Y|X,D], mname(m04) learner(Y6_04): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv)
ddml E[Y|X,D], mname(m04) learner(Y7_04): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720))
ddml E[Y|X,D], mname(m04) learner(Y8_04): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
	
*** add learners for E[D|X]
ddml E[D|X], mname(m04) learner(D1_04): reg $D
ddml E[D|X], mname(m04) learner(D2_04): pystacked $D $Xr $Xc || method(logit) , type(class)
ddml E[D|X], mname(m04) learner(D3_04): ///
	pystacked $D $Xr##c.($Xc) || method(lassocv) , type(class)
ddml E[D|X], mname(m04) learner(D4_04): ///
	pystacked $D $Xr##c.($Xc) || method(ridgecv) , type(class)
ddml E[D|X], mname(m04) learner(D5_04): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv) , type(class)
ddml E[D|X], mname(m04) learner(D6_04): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv) , type(class)
ddml E[D|X], mname(m04) learner(D7_04): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720)) , type(class)
ddml E[D|X], mname(m04) learner(D8_04): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720)) , type(class)

** cross-fitting
ddml crossfit , mname(m04) 

*** Estimation results
ddml estimate, mname(m04) robust atet

*** Model for 2005 effect
global Y dy05

ddml init interactive if (g == 0 | g == 2004 | g > 2005) , mname(m05) kfolds(5) reps(10)

*** add learners for E[Y|D,X]
ddml E[Y|X,D], mname(m05) learner(Y1_05): reg $Y
ddml E[Y|X,D], mname(m05) learner(Y2_05): reg $Y $Xr $Xc
ddml E[Y|X,D], mname(m05) learner(Y3_05): ///
	pystacked $Y $Xr##c.($Xc) || method(lassocv)
ddml E[Y|X,D], mname(m05) learner(Y4_05): ///
	pystacked $Y $Xr##c.($Xc) || method(ridgecv)
ddml E[Y|X,D], mname(m05) learner(Y5_05): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv)
ddml E[Y|X,D], mname(m05) learner(Y6_05): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv)
ddml E[Y|X,D], mname(m05) learner(Y7_05): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720))
ddml E[Y|X,D], mname(m05) learner(Y8_05): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
	
*** add learners for E[D|X]
ddml E[D|X], mname(m05) learner(D1_05): reg $D
ddml E[D|X], mname(m05) learner(D2_05): pystacked $D $Xr $Xc || method(logit) , type(class)
ddml E[D|X], mname(m05) learner(D3_05): ///
	pystacked $D $Xr##c.($Xc) || method(lassocv) , type(class)
ddml E[D|X], mname(m05) learner(D4_05): ///
	pystacked $D $Xr##c.($Xc) || method(ridgecv) , type(class)
ddml E[D|X], mname(m05) learner(D5_05): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv) , type(class)
ddml E[D|X], mname(m05) learner(D6_05): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv) , type(class)
ddml E[D|X], mname(m05) learner(D7_05): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720)) , type(class)
ddml E[D|X], mname(m05) learner(D8_05): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720)) , type(class)

** cross-fitting
ddml crossfit , mname(m05) 

*** Estimation results
ddml estimate, mname(m05) robust atet

*** Model for 2006 effect
global Y dy06

ddml init interactive if (g == 0 | g == 2004 | g > 2006) , mname(m06) kfolds(5) reps(10)

*** add learners for E[Y|D,X]
ddml E[Y|X,D], mname(m06) learner(Y1_06): reg $Y
ddml E[Y|X,D], mname(m06) learner(Y2_06): reg $Y $Xr $Xc
ddml E[Y|X,D], mname(m06) learner(Y3_06): ///
	pystacked $Y $Xr##c.($Xc) || method(lassocv)
ddml E[Y|X,D], mname(m06) learner(Y4_06): ///
	pystacked $Y $Xr##c.($Xc) || method(ridgecv)
ddml E[Y|X,D], mname(m06) learner(Y5_06): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv)
ddml E[Y|X,D], mname(m06) learner(Y6_06): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv)
ddml E[Y|X,D], mname(m06) learner(Y7_06): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720))
ddml E[Y|X,D], mname(m06) learner(Y8_06): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
	
*** add learners for E[D|X]
ddml E[D|X], mname(m06) learner(D1_06): reg $D
ddml E[D|X], mname(m06) learner(D2_06): pystacked $D $Xr $Xc || method(logit) , type(class)
ddml E[D|X], mname(m06) learner(D3_06): ///
	pystacked $D $Xr##c.($Xc) || method(lassocv) , type(class)
ddml E[D|X], mname(m06) learner(D4_06): ///
	pystacked $D $Xr##c.($Xc) || method(ridgecv) , type(class)
ddml E[D|X], mname(m06) learner(D5_06): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv) , type(class)
ddml E[D|X], mname(m06) learner(D6_06): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv) , type(class)
ddml E[D|X], mname(m06) learner(D7_06): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720)) , type(class)
ddml E[D|X], mname(m06) learner(D8_06): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720)) , type(class)

** cross-fitting
ddml crossfit , mname(m06) 

*** Estimation results
ddml estimate, mname(m06) robust atet

*** Model for 2007 effect
global Y dy07

ddml init interactive if (g == 0 | g == 2004 | g > 2007) , mname(m07) kfolds(5) reps(10)

*** add learners for E[Y|D,X]
ddml E[Y|X,D], mname(m07) learner(Y1_07): reg $Y
ddml E[Y|X,D], mname(m07) learner(Y2_07): reg $Y $Xr $Xc
ddml E[Y|X,D], mname(m07) learner(Y3_07): ///
	pystacked $Y $Xr##c.($Xc) || method(lassocv)
ddml E[Y|X,D], mname(m07) learner(Y4_07): ///
	pystacked $Y $Xr##c.($Xc) || method(ridgecv)
ddml E[Y|X,D], mname(m07) learner(Y5_07): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv)
ddml E[Y|X,D], mname(m07) learner(Y6_07): ///
	pystacked $Y $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv)
ddml E[Y|X,D], mname(m07) learner(Y7_07): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720))
ddml E[Y|X,D], mname(m07) learner(Y8_07): pystacked $Y $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720))
	
*** add learners for E[D|X]
ddml E[D|X], mname(m07) learner(D1_07): reg $D
ddml E[D|X], mname(m07) learner(D2_07): pystacked $D $Xr $Xc || method(logit) , type(class)
ddml E[D|X], mname(m07) learner(D3_07): ///
	pystacked $D $Xr##c.($Xc) || method(lassocv) , type(class)
ddml E[D|X], mname(m07) learner(D4_07): ///
	pystacked $D $Xr##c.($Xc) || method(ridgecv) , type(class)
ddml E[D|X], mname(m07) learner(D5_07): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(lassocv) , type(class)
ddml E[D|X], mname(m07) learner(D6_07): ///
	pystacked $D $Xr##c.($Xemp)##c.($Xpop)##c.($Xpay) || method(ridgecv) , type(class)
ddml E[D|X], mname(m07) learner(D7_07): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(3) random_state(720)) , type(class)
ddml E[D|X], mname(m07) learner(D8_07): pystacked $D $Xr $Xc || method(rf) ///
	opt(n_estimators(500) min_samples_leaf(20) random_state(720)) , type(class)

** cross-fitting
ddml crossfit , mname(m07) 

*** Estimation results
ddml estimate, mname(m07) robust atet

log close



