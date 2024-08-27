*** Code for 401(k) example illustrating boosted trees
capture log close
log using 401kBoost.txt , text replace 

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

****************************************************************************
*** Fit boosted regression tree models. Going to start in python

* Python for loops and Stata are finicky. Going to brute force 5-fold CV here
* rather than try to get nested for loops to work
*** Create fold variable for 5 fold CV
gen cvrand = runiform()
egen foldvar = cut(cvrand) if tr , group(5)
replace foldvar = foldvar + 1
recode foldvar . = -99999

python:

from sfi import Data
import numpy as np
from sklearn.ensemble import GradientBoostingRegressor as gbr
import matplotlib.pyplot as plt

# Use the sfi Data class to pull data from Stata variables into Python
X = np.array(Data.get("e401 age inc educ fsize marr twoearn db pira hown"))
y = np.array(Data.get("net_tfa"))
T = np.array(Data.get("trdata"))
Fvar = np.array(Data.get("foldvar"))

# Variable names
Xcols = ["e401", "age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown"]
ycols = ["net_tfa"]

# Get training and test data
Xtrain = X[T == 1 , ]
ytrain = y[T == 1]
Xtest = X[T == 0 , ]
ytest = y[T == 0 , ]

# Get cross-validation data
Xtrain1 = X[Fvar != 1, ]
Xtest1 = X[Fvar == 1, ]
ytrain1 = y[Fvar != 1]
ytest1 = y[Fvar == 1]

Xtrain2 = X[Fvar != 2, ]
Xtest2 = X[Fvar == 2, ]
ytrain2 = y[Fvar != 2]
ytest2 = y[Fvar == 2]

Xtrain3 = X[Fvar != 3, ]
Xtest3 = X[Fvar == 3, ]
ytrain3 = y[Fvar != 3]
ytest3 = y[Fvar == 3]

Xtrain4 = X[Fvar != 4, ]
Xtest4 = X[Fvar == 4, ]
ytrain4 = y[Fvar != 4]
ytest4 = y[Fvar == 4]

Xtrain5 = X[Fvar != 5, ]
Xtest5 = X[Fvar == 5, ]
ytrain5 = y[Fvar != 5]
ytest5 = y[Fvar == 5]

# Get constant fit for comparison
ymean = np.mean(ytrain)
intss = np.mean(np.square(ytrain - ymean))
oostss = np.mean(np.square(ytest - ymean))

# gradient boosting with 1000 boosting iteration but other default tuning choices
reg1 = gbr(n_estimators = 1000)
reg1.fit(Xtrain, ytrain)

testmse1 = np.zeros((1000,), dtype=np.float64)
for i, y_pred in enumerate(reg1.staged_predict(Xtest)): testmse1[i] = np.mean(np.square(ytest-y_pred))

trainmse1 = np.zeros((1000,), dtype=np.float64)
for i, y_pred in enumerate(reg1.staged_predict(Xtrain)): trainmse1[i] = np.mean(np.square(ytrain-y_pred))

inR2 = 1-trainmse1/intss
outR2 = 1-testmse1/oostss

'Best OOS Rsq {:.3F}'.format(np.max(outR2))

# Do cross validation - code here could definitely be improved
cvreg1 = gbr(n_estimators = 1000)
cvreg1.fit(Xtrain1, ytrain1)

cvsse = np.zeros((1000,), dtype=np.float64)
for i, y_pred in enumerate(cvreg1.staged_predict(Xtest1)): cvsse[i] = np.sum(np.square(ytest1-y_pred))

cvreg2 = gbr(n_estimators = 1000)
cvreg2.fit(Xtrain2, ytrain2)
for i, y_pred in enumerate(cvreg2.staged_predict(Xtest2)): cvsse[i] = cvsse[i] + np.sum(np.square(ytest2-y_pred))

cvreg3 = gbr(n_estimators = 1000)
cvreg3.fit(Xtrain3, ytrain3)
for i, y_pred in enumerate(cvreg3.staged_predict(Xtest3)): cvsse[i] = cvsse[i] + np.sum(np.square(ytest3-y_pred))

cvreg4 = gbr(n_estimators = 1000)
cvreg4.fit(Xtrain4, ytrain4)
for i, y_pred in enumerate(cvreg4.staged_predict(Xtest4)): cvsse[i] = cvsse[i] + np.sum(np.square(ytest4-y_pred))

cvreg5 = gbr(n_estimators = 1000)
cvreg5.fit(Xtrain5, ytrain5)
for i, y_pred in enumerate(cvreg5.staged_predict(Xtest5)): cvsse[i] = cvsse[i] + np.sum(np.square(ytest5-y_pred))

cvmse = cvsse/Xtrain.shape[0]
cvR2 = 1 - cvmse/intss

'CV MSE min B {}'.format(np.argmax(cvR2))
'Validation Rsq at CV B = {:.3F}'.format(outR2[np.argmax(cvR2)])

iter = range(1,1001)

fig, ax = plt.subplots()
ax.plot(iter, inR2, color = "red", label = "In sample Rsq")
ax.plot(iter, outR2, color = "blue", label = "Validation Rsq")
ax.plot(iter, cvR2, color = "green", label = "CV Rsq")
ax.legend()
ax.set_xlabel("Boosting Iteration")
fig.savefig('..\\Slides\\figures\\boost_iterations.pdf')

# Implement early stopping. Use default tolerance of 1e-4. Monitor performace 
# on 20% validation sample. Stop if 10 iterations of no improvement. All
# other parameters at default. 
esreg = gbr(validation_fraction = .2, n_iter_no_change = 10, random_state = 720)
esreg.fit(Xtrain, ytrain)

'Number of iterations: {}'.format(esreg.n_estimators_)

# No easy way to see what the validation sample is or performance on it
# Just look at performance in the hold out validation sample

testmse_es = np.zeros((esreg.n_estimators_,), dtype=np.float64)
for i, y_pred in enumerate(esreg.staged_predict(Xtest)): testmse_es[i] = np.mean(np.square(ytest-y_pred))

esoutR2 = 1-testmse_es/oostss

'Early Stopping Validation Rsq = {:.3F}'.format(esoutR2[-1])

esiter = range(1,esreg.n_estimators_+1)

fig, ax = plt.subplots()
ax.plot(iter, inR2, color = "red", label = "In sample Rsq")
ax.plot(iter, outR2, color = "blue", label = "Validation Rsq")
ax.plot(iter, cvR2, color = "green", label = "CV Rsq")
ax.plot(esiter, esoutR2, color = "magenta", label = "Validation Rsq - Early")
ax.legend()
ax.set_xlabel("Boosting Iteration")
fig.savefig('..\\Slides\\figures\\es_iterations.pdf')

end

***************************************************************************
*** Use pystacked to look at performance under several additional choices

recode foldvar -99999 = .

pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(gradboost) opt(learning_rate(.3) n_estimators(500) max_depth(6) /// 
		random_state(720) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.3) max_depth(6) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.1) n_estimators(500) max_depth(6) ///
		random_state(720) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.1) max_depth(6) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.1) max_depth(5) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.1) max_depth(4) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.1) max_depth(3) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.1) max_depth(2) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(2) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.01) max_depth(6) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.01) max_depth(5) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.01) max_depth(4) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.01) max_depth(3) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) min_samples_leaf(20)) || ///
	m(gradboost) opt(learning_rate(.01) max_depth(2) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) min_samples_leaf(20)) || ///
	if trdata , type(reg) foldvar(foldvar)

pystacked , table holdout

matrix list r(m)

predict yhatcv , basexb cv
predict yhat , basexb

*** Fit constant model for reference later
qui reg net_tfa if trdata == 1
predict yhat_c 
gen r_mean2 = (net_tfa - yhat_c)^2
qui sum r_mean2 if trdata == 1
local tssIn = r(mean)
qui sum r_mean2 if trdata == 0
local tssOut = r(mean)

* Tried 13 learners
forvalues i = 1/13 {
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
