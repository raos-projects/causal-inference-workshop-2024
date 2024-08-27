*** Code for 401(k) example illustrating trees
capture log close
log using 401kTrees.txt , text replace 


clear all

timer on 1
 
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
*** Fit regression tree models. Just going to do this in python
*** Need to have "graphviz" installed. 
***         : Open terminal via anaconda. Type conda install python-graphviz
***         : OR comment out the lines that produce graphviz plots

* Fit trees with few partitions using only inc and age to illustrate
python:

from sfi import Data
import numpy as np
from sklearn import tree
from sklearn.tree import DecisionTreeRegressor
from sklearn.tree import export_text

# Use the sfi Data class to pull data from Stata variables into Python
X = np.array(Data.get("age inc"))
y = np.array(Data.get("net_tfa"))
T = np.array(Data.get("trdata"))

# Variable names
Xcols = ["age", "inc"]
ycols = ["net_tfa"]

# Get training and test data
Xtrain = X[T == 1 , ]
ytrain = y[T == 1]
Xtest = X[T == 0 , ]
ytest = y[T == 0 , ]

# Fit tree with two leaves
reg1 = DecisionTreeRegressor(max_leaf_nodes = 2)
reg1.fit(Xtrain, ytrain)

# Text representation of tree
r = export_text(reg1, feature_names=Xcols)
print(r)
# Split at inc <= 160450.50

### No graphviz: Comment these lines out
# Create a figure representing the tree using graphviz
import graphviz
from sklearn.tree import export_graphviz
dot_data = export_graphviz(reg1, out_file = None, feature_names = Xcols, label = 'root', filled = True, impurity = False, rounded = True)
sh_tree = graphviz.Source(dot_data , directory = "..\\Slides\\figures\\") 
sh_tree.save(filename = "twoLeaves.pdf" , directory = "..\\Slides\\figures\\")
sh_tree.render(filename = "twoLeaves", directory = "..\\Slides\\figures\\", format = "pdf")
### End comment out

# Fit tree with three leaves
reg2 = DecisionTreeRegressor(max_leaf_nodes = 3)
reg2.fit(Xtrain, ytrain)

# Text representation of tree
r = export_text(reg2, feature_names=Xcols)
print(r)
# Split at inc <- 65578.5

### No graphviz: Comment these lines out
# Create a figure representing the tree using graphviz
dot_data = export_graphviz(reg2, out_file = None, feature_names = Xcols, label = 'root', filled = True, impurity = False, rounded = True)
sh_tree = graphviz.Source(dot_data , directory = "..\\Slides\\figures\\") 
sh_tree.save(filename = "threeLeaves.pdf" , directory = "..\\Slides\\figures\\")
sh_tree.render(filename = "threeLeaves", directory = "..\\Slides\\figures\\", format = "pdf")
### End comment out

# Fit tree with four leaves
reg3 = DecisionTreeRegressor(max_leaf_nodes = 4)
reg3.fit(Xtrain, ytrain)

# Text representation of tree
r = export_text(reg3, feature_names=Xcols)
print(r)
# Split income > 160450.5 at age <= 58.5

### No graphviz: Comment these lines out
# Create a figure representing the tree using graphviz
dot_data = export_graphviz(reg3, out_file = None, feature_names = Xcols, label = 'root', filled = True, impurity = False, rounded = True)
sh_tree = graphviz.Source(dot_data , directory = "..\\Slides\\figures\\") 
sh_tree.save(filename = "fourLeaves.pdf" , directory = "..\\Slides\\figures\\")
sh_tree.render(filename = "fourLeaves", directory = "..\\Slides\\figures\\", format = "pdf")
### End comment out

end

 
***************************************************************************
*** Illustrate tree partitions using colorscatter: ssc install colorscatter 
gen colory = net_tfa
replace colory = -5000 if net_tfa < -5000
replace colory = 150000 if net_tfa > 150000

* First split
colorscatter age inc colory, scatter_options(legend(off)) rgb_low(255 0 0) rgb_high(0 255 0) xline(160450.5 , lcol(blue) lp(solid))
graph export ..\Slides\figures\Stata_treesplit1.png , replace
graph close

* Second split
colorscatter age inc colory, scatter_options(legend(off)) rgb_low(255 0 0) rgb_high(0 255 0) xline(160450.5 , lcol(blue) lp(solid)) xline(65578.5, lcol(red) lp(solid))
graph export ..\Slides\figures\Stata_treesplit2.png , replace
graph close

* Third split
colorscatter age inc colory, scatter_options(legend(off)) rgb_low(255 0 0) rgb_high(0 255 0) xline(160450.5, lcol(blue) lp(solid)) xline(65578.5, lcol(red) lp(solid)) tw_post( function age = 58.5, lcol(orange) range(160450.5 242124)) 
graph export ..\Slides\figures\Stata_treesplit3.png , replace
graph close


****************************************************************************
*** Look at fitting a few models and performance in holdout data

python:

from sfi import Data
import numpy as np
from sklearn import tree
from sklearn.tree import DecisionTreeRegressor

# Use the sfi Data class to pull data from Stata variables into Python
X = np.array(Data.get("e401 age inc educ fsize marr twoearn db pira hown"))
y = np.array(Data.get("net_tfa"))
T = np.array(Data.get("trdata"))

# Variable names
Xcols = ["e401", "age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown"]
ycols = ["net_tfa"]

# Get training and test data
Xtrain = X[T == 1 , ]
ytrain = y[T == 1]
Xtest = X[T == 0 , ]
ytest = y[T == 0 , ]

# Get constant fit for comparison
ymean = np.mean(ytrain)
intss = np.mean(np.square(ytrain - ymean))
oostss = np.mean(np.square(ytest - ymean))

# Fit Depth 1 tree
reg1 = DecisionTreeRegressor(max_depth = 1)
reg1.fit(Xtrain, ytrain)
yin1 = reg1.predict(Xtrain)
yhat1 = reg1.predict(Xtest)

# In sample R^2
inrsq1 = 1 - np.mean(np.square(ytrain - yin1))/intss

# Out of sample R^2
oosrsq1 = 1 - np.mean(np.square(ytest - yhat1))/oostss

# Fit Depth 2 tree
reg2 = DecisionTreeRegressor(max_depth = 2)
reg2.fit(Xtrain, ytrain)
yin2 = reg2.predict(Xtrain)
yhat2 = reg2.predict(Xtest)

# In sample R^2
inrsq2 = 1 - np.mean(np.square(ytrain - yin2))/intss

# Out of sample R^2
oosrsq2 = 1 - np.mean(np.square(ytest - yhat2))/oostss

# Fit Depth 3 tree
reg3 = DecisionTreeRegressor(max_depth = 3)
reg3.fit(Xtrain, ytrain)
yin3 = reg3.predict(Xtrain)
yhat3 = reg3.predict(Xtest)

# In sample R^2
inrsq3 = 1 - np.mean(np.square(ytrain - yin3))/intss

# Out of sample R^2
oosrsq3 = 1 - np.mean(np.square(ytest - yhat3))/oostss

# Fit Depth 12 tree
reg12 = DecisionTreeRegressor(max_depth = 12)
reg12.fit(Xtrain, ytrain)
yin12 = reg12.predict(Xtrain)
yhat12 = reg12.predict(Xtest)

# In sample R^2
inrsq12 = 1 - np.mean(np.square(ytrain - yin12))/intss

# Out of sample R^2
oosrsq12 = 1 - np.mean(np.square(ytest - yhat12))/oostss

'Depth 1 & {:.3F} & {:.3F}'.format(inrsq1, oosrsq1)
'Depth 2 & {:.3F} & {:.3F}'.format(inrsq2, oosrsq2)
'Depth 3 & {:.3F} & {:.3F}'.format(inrsq3, oosrsq3)
'Depth 12 & {:.3F} & {:.3F}'.format(inrsq12, oosrsq12)

end


****************************************************************************
*** Look at fitting a few models and performance in holdout data
*** We saw in our pictures that the splits were really focusing on outliers
*** Impose sensible minimum size for leaves

python:

from sfi import Data
import numpy as np
from sklearn import tree
from sklearn.tree import DecisionTreeRegressor

# Use the sfi Data class to pull data from Stata variables into Python
X = np.array(Data.get("e401 age inc educ fsize marr twoearn db pira hown"))
y = np.array(Data.get("net_tfa"))
T = np.array(Data.get("trdata"))

# Variable names
Xcols = ["e401", "age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown"]
ycols = ["net_tfa"]

# Get training and test data
Xtrain = X[T == 1 , ]
ytrain = y[T == 1]
Xtest = X[T == 0 , ]
ytest = y[T == 0 , ]

# Get constant fit for comparison
ymean = np.mean(ytrain)
intss = np.mean(np.square(ytrain - ymean))
oostss = np.mean(np.square(ytest - ymean))

# Fit Depth 1 tree
reg1 = DecisionTreeRegressor(max_depth = 1, min_samples_leaf = 20)
reg1.fit(Xtrain, ytrain)
yin1 = reg1.predict(Xtrain)
yhat1 = reg1.predict(Xtest)

# In sample R^2
inrsq1 = 1 - np.mean(np.square(ytrain - yin1))/intss

# Out of sample R^2
oosrsq1 = 1 - np.mean(np.square(ytest - yhat1))/oostss

# Fit Depth 2 tree
reg2 = DecisionTreeRegressor(max_depth = 2, min_samples_leaf = 20)
reg2.fit(Xtrain, ytrain)
yin2 = reg2.predict(Xtrain)
yhat2 = reg2.predict(Xtest)

# In sample R^2
inrsq2 = 1 - np.mean(np.square(ytrain - yin2))/intss

# Out of sample R^2
oosrsq2 = 1 - np.mean(np.square(ytest - yhat2))/oostss

# Fit Depth 3 tree
reg3 = DecisionTreeRegressor(max_depth = 3, min_samples_leaf = 20)
reg3.fit(Xtrain, ytrain)
yin3 = reg3.predict(Xtrain)
yhat3 = reg3.predict(Xtest)

# In sample R^2
inrsq3 = 1 - np.mean(np.square(ytrain - yin3))/intss

# Out of sample R^2
oosrsq3 = 1 - np.mean(np.square(ytest - yhat3))/oostss

# Fit Depth 12 tree
reg12 = DecisionTreeRegressor(max_depth = 12, min_samples_leaf = 20)
reg12.fit(Xtrain, ytrain)
yin12 = reg12.predict(Xtrain)
yhat12 = reg12.predict(Xtest)

# In sample R^2
inrsq12 = 1 - np.mean(np.square(ytrain - yin12))/intss

# Out of sample R^2
oosrsq12 = 1 - np.mean(np.square(ytest - yhat12))/oostss

'Depth 1 & {:.3F} & {:.3F}'.format(inrsq1, oosrsq1)
'Depth 2 & {:.3F} & {:.3F}'.format(inrsq2, oosrsq2)
'Depth 3 & {:.3F} & {:.3F}'.format(inrsq3, oosrsq3)
'Depth 12 & {:.3F} & {:.3F}'.format(inrsq12, oosrsq12)

end


****************************************************************************
*** Look at cross-validation
python:

from sfi import Data
import numpy as np
import pandas as pd
from sklearn import tree
from sklearn.tree import DecisionTreeRegressor
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import KFold
from matplotlib import pyplot as plt

# Use the sfi Data class to pull data from Stata variables into Python
X = np.array(Data.get("e401 age inc educ fsize marr twoearn db pira hown"))
y = np.array(Data.get("net_tfa"))
T = np.array(Data.get("trdata"))

# Variable names
Xcols = ["e401", "age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown"]
ycols = ["net_tfa"]

# Get training and test data
Xtrain = X[T == 1 , ]
ytrain = y[T == 1]
Xtest = X[T == 0 , ]
ytest = y[T == 0 , ]

# Get constant fit for comparison
ymean = np.mean(ytrain)
oostss = np.mean(np.square(ytest - ymean))

# Cross-validation using built in CV from scikit learn
# We will cross-validate over depth
# Evaluation metric by default is OOS R^2
# Kfold pass is meant to keep folds the same if I try different tuning
# Make sure to set same random state everywhere for replicability
grid = KFold(n_splits=5, shuffle = True, random_state = 720)

parameters = {'max_depth':range(2,20)}

cvtree1 = GridSearchCV(DecisionTreeRegressor(random_state = 720), parameters, cv = grid)  
cvtree1.fit(Xtrain, ytrain)
print (cvtree1.best_score_, cvtree1.best_params_)

# Double check that I get same results
#cvtree2 = GridSearchCV(DecisionTreeRegressor(random_state = 720), parameters, cv = grid)  
#cvtree2.fit(Xtrain, ytrain)
#print (cvtree2.best_score_, cvtree2.best_params_)

# Put results into a dataframe
cvresults1 = pd.DataFrame(cvtree1.cv_results_)

# Repeat with a minimum leaf size of 20
cvtree3 = GridSearchCV(DecisionTreeRegressor(random_state = 720, min_samples_leaf = 20), parameters, cv = grid)  
cvtree3.fit(Xtrain, ytrain)
print (cvtree3.best_score_, cvtree3.best_params_)


# Double check that I get same results
#cvtree4 = GridSearchCV(DecisionTreeRegressor(random_state = 720, min_samples_leaf = 20), parameters, cv = grid)  
#cvtree4.fit(Xtrain, ytrain)
#print (cvtree4.best_score_, cvtree4.best_params_)

# Put results into a dataframe
cvresults2 = pd.DataFrame(cvtree3.cv_results_)

# Create plot of CV with +/- 1 s.e. bars
ax = cvresults1.plot(x = "param_max_depth" , y = "mean_test_score", yerr = "std_test_score", color = "red", label = "Min 1")
ax = cvresults2.plot(ax = ax, x = "param_max_depth" , y = "mean_test_score", yerr = "std_test_score", color = "blue", label = "Min 20")
ax.legend(loc = "lower left")
fig = ax.get_figure()
fig.savefig('..\\Slides\\figures\\tree_cv.pdf')

# Get validation sample R^2
yhat1 = cvtree1.predict(Xtest)
yhat2 = cvtree3.predict(Xtest)

# Out of sample R^2
oosrsq1 = 1 - np.mean(np.square(ytest - yhat1))/oostss
oosrsq2 = 1 - np.mean(np.square(ytest - yhat2))/oostss

'CV 1 & {:.3F} & {:.3F}'.format(cvtree1.best_score_, oosrsq1)
'CV 20 & {:.3F} & {:.3F}'.format(cvtree3.best_score_, oosrsq2)

### No graphviz: Comment these lines out
# Draw cv tree
import graphviz
from sklearn.tree import export_graphviz

regcv = DecisionTreeRegressor(max_depth = cvtree3.best_params_.get("max_depth"), random_state = 720, min_samples_leaf = 20)
regcv.fit(Xtrain, ytrain)

dot_data = export_graphviz(regcv, out_file = None, feature_names = Xcols, label = 'root', filled = True, impurity = False, rounded = True)
cv_tree = graphviz.Source(dot_data , directory = "..\\Slides\\figures\\") 
cv_tree.save(filename = "Stata_cvTree.pdf" , directory = "..\\Slides\\figures\\")
cv_tree.render(filename = "Stata_cvTree", directory = "..\\Slides\\figures\\", format = "pdf")
### End comment out

end

timer off 1
timer list
timer clear

log close
