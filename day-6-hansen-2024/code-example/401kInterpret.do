*** Code for 401(k) example illustrating black box interpretation
capture log close
log using 401kInterpret.txt , text replace 

clear all

timer on 1
 
use "..\Data\sipp1991.dta", clear /* Load data. Assumes I am in my code subfolder */

* Normalize inc,age,fsize,educ 
* Seems to matter for NNs
replace age = age/64
replace inc = inc/250000
replace fsize = fsize/13
replace educ = educ/18

*** Set seed for reproducibility
set seed 71423

*** Split data into a training (~80%) and test (~20%) set
gen trdata = runiform()
replace trdata = trdata < .8

* Sample sizes
count if trdata
count if !trdata

****************************************************************************
* Drop column importance

* Look at drop column importance for DNN and RF
* 1) Fit full model
* NN
qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
		m(nnet) opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720)) || ///
		if trdata , type(reg)
		
qui predict yhatNN

* Random forest
qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
		m(rf) opt(n_estimators(500) min_samples_leaf(20) random_state(720)) || ///
		if trdata , type(reg)
		
qui predict yhatRF

* Refit dropping one variable at a time
local X "e401 age inc educ fsize marr twoearn db pira hown"

foreach y of local X {
	local Xy : list X - y
	
	* NN
	qui pystacked net_tfa `Xy' || ///
		m(nnet) opt(hidden_layer_sizes(50 50 50 50) alpha(0) random_state(720)) || ///
		if trdata , type(reg)
		
	qui predict yhatNN`y'

	* Random forest
	qui pystacked net_tfa `Xy' || ///
		m(rf) opt(n_estimators(500) min_samples_leaf(20) random_state(720)) || ///
		if trdata , type(reg)
		
	qui predict yhatRF`y'
}

gen rNN2 = (net_tfa - yhatNN)^2
gen rRF2 = (net_tfa - yhatRF)^2
local X "e401 age inc educ fsize marr twoearn db pira hown"
qui sum rNN2 if trdata == 0
local SSNN = r(mean)
qui sum rRF2 if trdata == 0
local SSRF = r(mean)

foreach y of local X {
	gen rNN2`y' = (net_tfa - yhatNN`y')^2
	gen rRF2`y' = (net_tfa - yhatNN`y')^2
	
	qui sum rNN2`y' if trdata == 0
	local SSNN`y' = r(mean)
	qui sum rRF2`y' if trdata == 0
	local SSRF`y' = r(mean)
	
	local VINN`y' = `SSNN`y''/`SSNN' - 1
	local VIRF`y' = `SSRF`y''/`SSRF' - 1

	display "Drop column variable importance"
	display "`y'" " & " `VINN`y'' " & " `VIRF`y''
}
	
*****************************************************************************
* Permutation variable importance and partial dependence in python

python:

from sfi import Data
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestRegressor as rf
from sklearn.neural_network import MLPRegressor as nn
from sklearn.inspection import permutation_importance as permimp
from sklearn.inspection import partial_dependence as partdep

# Use the sfi Data class to pull data from Stata variables into Python
X = np.array(Data.get("e401 age inc educ fsize marr twoearn db pira hown"))
y = np.array(Data.get("net_tfa"))
T = np.array(Data.get("trdata"))

# Variable names
Xcols = np.array(["e401", "age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown"])
ycols = ["net_tfa"]
p = X.shape[1]

# Get training and test data
Xtrain = X[T == 1 , ]
ytrain = y[T == 1]
Xtest = X[T == 0 , ]
ytest = y[T == 0]	
	
# random forest
reg1 = rf(n_estimators = 500, min_samples_leaf = 20, random_state = 720)
reg1.fit(Xtrain, ytrain)

print(f"RF train accuracy: {reg1.score(Xtrain, ytrain):.3f}")
print(f"RF test accuracy: {reg1.score(Xtest, ytest):.3f}")

# Permutation importance

rfimp = permimp(reg1, Xtest, ytest, n_repeats = 250, random_state = 720)

for i in rfimp.importances_mean.argsort()[::-1]: print(f"{Xcols[i]:<8}" f"{rfimp.importances_mean[i]:.3f}" f" +/- {rfimp.importances_std[i]:.3f}")

# Make box plots
rf_sortedimp = rfimp.importances_mean.argsort()
rf_importances = pd.DataFrame(rfimp.importances[rf_sortedimp].T, columns = Xcols[rf_sortedimp])

fig, ax = plt.subplots()
rf_importances.plot.box(vert = False, whis = 10, ax = ax)
ax.set_title("Permutation Importance, RF")
ax.set_xlabel("Decrease in R^2")
ax.figure.tight_layout()
fig.savefig('..\\Slides\\figures\\permimpRF.pdf')

# Partial dependence 
# income
pdrf_inc = partdep(reg1, features = [2], X = X, categorical_features = (0,4,5,6,7,8,9))
pdrf_inc_x = (list(pdrf_inc.values())[1][0])*250000 # sci-kit learn 1.2.2
#pdrf_inc_x = (pdrf_inc.grid_values[0])*250000 #sci-kit learn 1.3.0
pdrf_inc_y = pdrf_inc.average[0]

fig, ax = plt.subplots()
ax.plot(pdrf_inc_x, pdrf_inc_y, color = "red")
ax.set_xlabel("Income")
ax.set_ylabel("Net_TFA")
ax.set_title("Partial Dependence (RF) - Income")
fig.savefig('..\\Slides\\figures\\partdepRFinc.pdf')

# pira
pdrf_pira = partdep(reg1, features = [8], X = X, categorical_features = (0,4,5,6,7,8,9))
#sci-kit learn 1.2.2
print(f"RF pira = {list(pdrf_pira.values())[1][0][0]:.0f} : {pdrf_pira.average[0][0]:.3f}")
print(f"RF pira = {list(pdrf_pira.values())[1][0][1]:.0f} : {pdrf_pira.average[0][1]:.3f}")

#sci-kit learn 1.3.0
#print(f"RF pira = {pdrf_pira.grid_values[0][0]:.0f} : {pdrf_pira.average[0][0]:.3f}")
#print(f"RF pira = {pdrf_pira.grid_values[0][1]:.0f} : {pdrf_pira.average[0][1]:.3f}")
	
# neural net
reg2 = nn(hidden_layer_sizes = (50, 50, 50, 50), alpha = 0, random_state = 720)
reg2.fit(Xtrain, ytrain)

print(f"NN train accuracy: {reg2.score(Xtrain, ytrain):.3f}")
print(f"NN test accuracy: {reg2.score(Xtest, ytest):.3f}")

# Permutation importance
nnimp = permimp(reg2, Xtest, ytest, n_repeats = 250, random_state = 720)

for i in nnimp.importances_mean.argsort()[::-1]: print(f"{Xcols[i]:<8}" f"{nnimp.importances_mean[i]:.3f}" f" +/- {nnimp.importances_std[i]:.3f}")	

# Make box plots
nn_sortedimp = nnimp.importances_mean.argsort()
nn_importances = pd.DataFrame(nnimp.importances[nn_sortedimp].T, columns = Xcols[nn_sortedimp])

fig, ax = plt.subplots()
nn_importances.plot.box(vert = False, whis = 10, ax = ax)
ax.set_title("Permutation Importance, NN")
ax.set_xlabel("Decrease in R^2")
ax.figure.tight_layout()
fig.savefig('..\\Slides\\figures\\permimpNN.pdf')
	
# Partial dependence 
# income
pdnn_inc = partdep(reg2, features = [2], X = X, categorical_features = (0,4,5,6,7,8,9))
pdnn_inc_x = (list(pdnn_inc.values())[1][0])*250000 # sci-kit learn 1.2.2
# pdnn_inc_x = (pdnn_inc.grid_values[0])*250000 # sci-kit learn 1.3.0
pdnn_inc_y = pdnn_inc.average[0]

fig, ax = plt.subplots()
ax.plot(pdnn_inc_x, pdnn_inc_y, color = "red")
ax.set_xlabel("Income")
ax.set_ylabel("Net_TFA")
ax.set_title("Partial Dependence (NN) - Income")
fig.savefig('..\\Slides\\figures\\partdepNNinc.pdf')

# pira
pdnn_pira = partdep(reg2, features = [8], X = X, categorical_features = (0,4,5,6,7,8,9))
#sci-kit learn 1.2.2
print(f"NN pira = {list(pdnn_pira.values())[1][0][0]:.0f} : {pdnn_pira.average[0][0]:.3f}")
print(f"NN pira = {list(pdnn_pira.values())[1][0][1]:.0f} : {pdnn_pira.average[0][1]:.3f}")


#sci-kit learn 1.3.0
#print(f"NN pira = {pdnn_pira.grid_values[0][0]:.0f} : {pdnn_pira.average[0][0]:.3f}")
#print(f"NN pira = {pdnn_pira.grid_values[0][1]:.0f} : {pdnn_pira.average[0][1]:.3f}")	
	
end

timer off 1
timer list
timer clear
		
log close

	