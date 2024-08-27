*** Code for 401(k) example illustrating deep nets
capture log close
log using 401kDNN.txt , text replace 

clear all
 
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
*** Fit deep neural network models. Going to start in python

python:

from sfi import Data
import numpy as np
import matplotlib.pyplot as plt

# Use the sfi Data class to pull data from Stata variables into Python
X = np.array(Data.get("e401 age inc educ fsize marr twoearn db pira hown"))
y = np.array(Data.get("net_tfa"))
T = np.array(Data.get("trdata"))

# Variable names
Xcols = ["e401", "age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown"]
ycols = ["net_tfa"]
p = X.shape[1]

# Get training and test data
Xtrain = X[T == 1 , ]
ytrain = y[T == 1]
Xtest = X[T == 0 , ]
ytest = y[T == 0]

# Get constant fit for comparison
ymean = np.mean(ytrain)
oostss = np.mean(np.square(ytest - ymean))

# Neural Net setup
from tensorflow.keras import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.layers import Dropout
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras import regularizers
from tensorflow.keras.utils import set_random_seed

set_random_seed(724)

# Model A
# Input, 4 hidden layers each with 50 neurons, output 

modelA = Sequential()
modelA.add(Dense(50, input_shape=(p,), activation = 'relu')) 
modelA.add(Dense(50, activation = 'relu'))
modelA.add(Dense(50, activation = 'relu'))
modelA.add(Dense(50, activation = 'relu'))
modelA.add(Dense(1))

# See that we set the model up correctly
modelA.summary()

# Set some fitting options
modelA.compile(loss='mse', optimizer='rmsprop')

# Fit the model, verbose = 0 turns off the display of each iteration
modelAfit = modelA.fit(Xtrain, ytrain, batch_size = 200, epochs = 200, validation_split = .2, verbose = 0)

# Get validation sample predictions
yhatA = modelA.predict(Xtest).flatten()  
# .flatten() removes dimensions so it's compatible with ytest
# Could also just use modelA.evaluate(Xtest, ytest) to compute mse
validMSEA = np.mean(np.square(ytest-yhatA))
validRMSEA = np.sqrt(validMSEA)
validR2A = 1 - validMSEA/oostss

'A: Validation RMSE = {:.0F}'.format(validRMSEA)
'A: Validation Rsq = {:.3F}'.format(validR2A)

# Plot training and validation loss
iter = range(1,201)
figA, ax = plt.subplots()
ax.plot(iter, np.sqrt(modelAfit.history['loss']))
ax.plot(iter, np.sqrt(modelAfit.history['val_loss']))
ax.set_title('DNN Input/50/50/50/50/Output')
ax.set_ylabel('RMSE')
ax.set_xlabel('epoch')
ax.legend(['train', 'test'], loc='upper left')
figA.savefig('..\\Slides\\figures\\DNNA_iterations.pdf')

# Model B
# Input, 4 hidden layers each with 50 neurons and .5 dropout, output 
modelB = Sequential()
modelB.add(Dense(50, input_shape=(p,), activation = 'relu')) 
modelB.add(Dropout(.5))
modelB.add(Dense(50, activation = 'relu'))
modelB.add(Dropout(.5))
modelB.add(Dense(50, activation = 'relu'))
modelB.add(Dropout(.5))
modelB.add(Dense(50, activation = 'relu'))
modelB.add(Dropout(.5))
modelB.add(Dense(1))

# See that we set the model up correctly
modelB.summary()

# Set some fitting options
modelB.compile(loss='mse', optimizer='rmsprop')

# Fit the model, verbose = 0 turns off the display of each iteration
modelBfit = modelB.fit(Xtrain, ytrain, batch_size = 200, epochs = 200, validation_split = .2, verbose = 0)

# Get validation sample predictions
yhatB = modelB.predict(Xtest).flatten()
validMSEB = np.mean(np.square(ytest-yhatB))
validRMSEB = np.sqrt(validMSEB)
validR2B = 1 - validMSEB/oostss

'B: Validation RMSE = {:.0F}'.format(validRMSEB)
'B: Validation Rsq = {:.3F}'.format(validR2B)

# Plot training and validation loss
iter = range(1,201)
figB, ax = plt.subplots()
ax.plot(iter, np.sqrt(modelBfit.history['loss']))
ax.plot(iter, np.sqrt(modelBfit.history['val_loss']))
ax.set_title('DNN Input/50/50/50/50/Output, .5 Dropout')
ax.set_ylabel('RMSE')
ax.set_xlabel('epoch')
ax.legend(['train', 'test'], loc='upper left')
figB.savefig('..\\Slides\\figures\\DNNB_iterations.pdf')

# Model C
# Input, 4 hidden layers each with 50 neurons and l2 regularization, output 
modelC = Sequential()
modelC.add(Dense(50, input_shape=(p,), activation = 'relu', kernel_regularizer=regularizers.L2(0.01))) 
modelC.add(Dense(50, activation = 'relu', kernel_regularizer=regularizers.L1(0.01)))
modelC.add(Dense(50, activation = 'relu', kernel_regularizer=regularizers.L1(0.01)))
modelC.add(Dense(50, activation = 'relu', kernel_regularizer=regularizers.L1(0.01)))
modelC.add(Dense(1))

# See that we set the model up correctly
modelC.summary()

# Set some fitting options
modelC.compile(loss='mse', optimizer='rmsprop')

# Fit the model, verbose = 0 turns off the display of each iteration
modelCfit = modelC.fit(Xtrain, ytrain, batch_size = 200, epochs = 200, validation_split = .2, verbose = 0)

# Get validation sample predictions
yhatC = modelC.predict(Xtest).flatten()
validMSEC = np.mean(np.square(ytest-yhatC))
validRMSEC = np.sqrt(validMSEC)
validR2C = 1 - validMSEC/oostss

'C: Validation RMSE = {:.0F}'.format(validRMSEC)
'C: Validation Rsq = {:.3F}'.format(validR2C)

# Plot training and validation loss
iter = range(1,201)
figC, ax = plt.subplots()
ax.plot(iter, np.sqrt(modelCfit.history['loss']))
ax.plot(iter, np.sqrt(modelCfit.history['val_loss']))
ax.set_title('DNN Input/50/50/50/50/Output, l2 penalty')
ax.set_ylabel('RMSE')
ax.set_xlabel('epoch')
ax.legend(['train', 'test'], loc='upper left')
figC.savefig('..\\Slides\\figures\\DNNC_iterations.pdf')

# Model D
# Input, 4 hidden layers each with 50 neurons and early stopping, output 
modelD = Sequential()
modelD.add(Dense(50, input_shape=(p,), activation = 'relu') )
modelD.add(Dense(50, activation = 'relu'))
modelD.add(Dense(50, activation = 'relu'))
modelD.add(Dense(50, activation = 'relu'))
modelD.add(Dense(1))

# See that we set the model up correctly
modelD.summary()

# Set some fitting options
modelD.compile(loss='mse', optimizer='rmsprop')

# Set early stopping rule
es = EarlyStopping(monitor='val_loss', mode='min', patience = 200, restore_best_weights = True)

# Fit the model, verbose = 0 turns off the display of each iteration
modelDfit = modelD.fit(Xtrain, ytrain, batch_size = 200, epochs = 2000, validation_split = .2, verbose = 0, callbacks = es)

# Get validation sample predictions
yhatD = modelD.predict(Xtest).flatten()
validMSED = np.mean(np.square(ytest-yhatD))
validRMSED = np.sqrt(validMSED)
validR2D = 1 - validMSED/oostss

'D: Validation RMSE = {:.0F}'.format(validRMSED)
'D: Validation Rsq = {:.3F}'.format(validR2D)

n_iter = len(modelDfit.history['val_loss'])
best_iter = np.argmin(modelDfit.history['val_loss'])

'D: Early stopping best: {}'.format(best_iter)

# Plot training and validation loss
iter = range(1,n_iter+1)
figD, ax = plt.subplots()
ax.plot(iter, np.sqrt(modelDfit.history['loss']))
ax.plot(iter, np.sqrt(modelDfit.history['val_loss']))
ax.axvline(x = best_iter, color = 'b')
ax.set_title('DNN Input/50/50/50/50/Output, l2 penalty')
ax.set_ylabel('RMSE')
ax.set_xlabel('epoch')
ax.legend(['train', 'test'], loc='upper left')
figD.savefig('..\\Slides\\figures\\DNND_iterations.pdf')

end


***************************************************************************
*** Use pystacked to look at performance under several additional choices
*** pystacked has many less options than tensorflow, but is easy to play with here

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20 20) alpha(0) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "20 20"
matrix list r(m)
qui predict yhat1 , basexb 	
	
qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20 20) alpha(.1) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "20 20 a(.1)"
matrix list r(m)
qui predict yhat2 , basexb 		
	
qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20 20) alpha(1) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "20 20 a(1)"
matrix list r(m)
qui predict yhat3 , basexb 	
	
qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20 20) alpha(0) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) max_iter(1000)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "20 20 es"
matrix list r(m)
qui predict yhat4 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(50 10 50) alpha(0) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "50 10 50"
matrix list r(m)
qui predict yhat5 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(50 10 50) alpha(.1) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "50 10 50 a(.1)"
matrix list r(m)
qui predict yhat6 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(50 10 50) alpha(1) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "50 10 50 a(1)"
matrix list r(m)
qui predict yhat7 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(50 10 50) alpha(0) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) max_iter(1000)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "50 10 50 es"
matrix list r(m)
qui predict yhat8 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20) alpha(0) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "20"
matrix list r(m)
qui predict yhat9 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20) alpha(.1) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "20 a(.1)"
matrix list r(m)
qui predict yhat10 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20) alpha(1) random_state(720)) || ///
	if trdata , type(reg) 

qui pystacked , table holdout
display "20 a(1)"
matrix list r(m)
qui predict yhat11 , basexb 	

qui pystacked net_tfa e401 age inc educ fsize marr twoearn db pira hown || ///
	m(nnet) opt(hidden_layer_sizes(20) alpha(0) random_state(720) ///
		validation_fraction(.2) n_iter_no_change(10) max_iter(1000)) || ///		
	if trdata , type(reg) 

qui pystacked , table holdout
display "20 es"
matrix list r(m)
qui predict yhat12 , basexb 	
	
*** Fit constant model for reference later
qui reg net_tfa if trdata == 1
predict yhat_c 
gen r_mean2 = (net_tfa - yhat_c)^2
qui sum r_mean2 if trdata == 0
local tssOut = r(mean)

* Tried 12 learners
forvalues i = 1/12 {
	qui gen r`i' = (net_tfa - yhat`i'1)^2
	qui sum r`i' if trdata == 0
	local essOut`i' = r(mean)
	local r2Out`i' = 1-(`essOut`i''/`tssOut')
	display "R^2 (in/CV/out) " `i' " = " `r2Out`i'' 
}



log close

