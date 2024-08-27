# 401(k) Example

######################### Libraries
library(hdm)
library(sandwich)
library(DoubleML)
library(mlr3learners)
library(mlr3)
library(data.table)
library(randomForest)
library(ranger)
library(mboost)
# uncomment the next two lines to install mlr3extralearners if needed
# library(remotes)
# remotes::install_github("mlr-org/mlr3extralearners") # needed to run boosting in DoubleML
library(mlr3extralearners)
library(xtable)

######################### Options
dpl = TRUE      # switch to save plots. Set to FALSE to just run code
codedir = "~/GitHub/NW2022_1Day/R Code" # Directory where Hansen keeps code
plotdir = "~/GitHub/NW2022_1Day/Slides/figures" # Directory where Hansen keeps figures for these slides

########################################################################
########### 401(k) Example: Inference

# Load data
data(pension)    # We've loaded it in other ways before, but the data is included
                 # in hdm
data <- pension 

# For reference, eligibility and participation effects estimated in full-sample
# without controlling for anything
base.elig = lm(net_tfa ~ e401, data = data)
HCV.base.elig <- vcovHC(base.elig)
base.elig.se <- sqrt(diag(HCV.base.elig))[2]

cat ("Eligibility ATE (no controls):",base.elig$coefficients[2],"\n") 
cat ("s.e.:",base.elig.se,"\n") 

base.part = tsls(net_tfa ~ p401 | e401, data = data, homoscedastic = FALSE)

cat ("Participation LATE (no controls):",base.part$coefficients[1],"\n") 
cat ("s.e.:",sqrt(base.part$vcov[1,1]),"\n") 


#####################################################################
# Going to use the DoubleML package and need to setup data and learners
# Just using a few learners here and not assessing robustness across sample splits

# Constructing the data (as DoubleMLData)
formula_flex = "net_tfa ~ e401 + poly(age, 6, raw=TRUE) + 
                poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) + 
                poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown"
model_flex = as.data.table(model.frame(formula_flex, pension))
x_cols = colnames(model_flex)[-c(1,2)]
data_ml = DoubleMLData$new(model_flex, y_col = "net_tfa", d_cols = "e401", x_cols=x_cols)

### Learners
# Specify learner: Lasso.
lasso <- lrn("regr.cv_glmnet",nfolds = 5, s = "lambda.min")
lasso_class <- lrn("classif.cv_glmnet", nfolds = 5, s = "lambda.min")
# Specify learner: Random Forest (with ranger package)
randomForest <- lrn("regr.ranger")
randomForest_class <- lrn("classif.ranger")
# Specify learner: Trees
trees <- lrn("regr.rpart")
trees_class <- lrn("classif.rpart")
# Specify learner: Boosted trees
boost<- lrn("regr.glmboost")
boost_class <- lrn("classif.glmboost")

# Use these variables below
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 

###################################################################
# Elibility ATE

# We're trimming the propensity score (at .01) to keep denominators from getting too small
# Using 3 cross-fit folds
set.seed(123)

##### lasso #####
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_irm = DoubleMLIRM$new(data_ml, ml_g = lasso, 
                          ml_m = lasso_class, 
                          trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
lasso_irm <- dml_irm$coef
lasso_std_irm <- dml_irm$se


# predictions
dml_irm$params_names()
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
lasso_y_irm <- sqrt(mean((y-g_hat)^2)) 
lasso_y_irm

# cross-fitted RMSE: treatment
lasso_d_irm <- sqrt(mean((d-m_hat)^2)) 
lasso_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

##### forest #####
dml_irm = DoubleMLIRM$new(data_ml, ml_g = randomForest, 
                          ml_m = randomForest_class, 
                          trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
forest_irm <- dml_irm$coef
forest_std_irm <- dml_irm$se

# predictions
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_0

# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 
forest_y_irm <- sqrt(mean((y-g_hat)^2)) 
forest_y_irm

# cross-fitted RMSE: treatment
forest_d_irm <- sqrt(mean((d-m_hat)^2)) 
forest_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

##### trees #####

dml_irm <- DoubleMLIRM$new(data_ml, ml_g = trees, ml_m = trees_class, 
                           trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
tree_irm <- dml_irm$coef
tree_std_irm <- dml_irm$se

# predictions
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 
tree_y_irm <- sqrt(mean((y-g_hat)^2)) 
tree_y_irm

# cross-fitted RMSE: treatment
tree_d_irm <- sqrt(mean((d-m_hat)^2)) 
tree_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)


##### boosting #####

dml_irm <- DoubleMLIRM$new(data_ml, ml_g = boost, ml_m = boost_class,
                           trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
boost_irm <- dml_irm$coef
boost_std_irm <- dml_irm$se

# predictions
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 
boost_y_irm <- sqrt(mean((y-g_hat)^2)) 
boost_y_irm

# cross-fitted RMSE: treatment
boost_d_irm <- sqrt(mean((d-m_hat)^2)) 
boost_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

##### Summarize results
table <- matrix(0, 4, 5)
table[1,1:5]   <- c(base.elig$coefficients[2],lasso_irm,forest_irm,tree_irm,boost_irm)
table[2,1:5]   <- c(base.elig.se,lasso_std_irm,forest_std_irm,tree_std_irm,boost_std_irm)
table[3,1:5]   <- c(sqrt(mean((y-mean(y))^2)),lasso_y_irm,forest_y_irm,tree_y_irm,boost_y_irm)
table[4,1:5]   <- c(sqrt(mean((d-mean(d))^2)),lasso_d_irm,forest_d_irm,tree_d_irm,boost_d_irm)
rownames(table) <- c("Estimate","Std.Error","RMSE Y","RMSE D")
colnames(table) <- c("No Controls","Lasso","Random Forest","Trees","Boosting")
tab<- xtable(table, digits = 2)
tab

# Use best learner for each function
set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_irm = DoubleMLIRM$new(data_ml, ml_g = randomForest, 
                          ml_m = lasso_class, 
                          trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
best_irm <- dml_irm$coef
best_std_irm <- dml_irm$se



#########################################################################
# LATE: participation instrumented for by eligibility

# Set-up IV data
# Constructing the data (as DoubleMLData)
formula_flex2 = "net_tfa ~ p401+ e401 + poly(age, 6, raw=TRUE) + poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) + poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown"
model_flex2 = as.data.table(model.frame(formula_flex2, data))
x_cols = colnames(model_flex2)[-c(1,2,3)]
data_IV = DoubleMLData$new(model_flex2, y_col = "net_tfa", d_cols = "p401", z_cols ="e401",x_cols=x_cols)

# Use same learners as before
set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_MLIIVM = DoubleMLIIVM$new(data_IV, ml_g = lasso, 
                              ml_m = lasso_class, ml_r = lasso_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                                                                                 never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
lasso_MLIIVM <- dml_MLIIVM$coef
lasso_std_MLIIVM <- dml_MLIIVM$se

# relable relative to previous section
y <- as.matrix(pension$net_tfa) 
d <- as.matrix(pension$p401) 
z <- as.matrix(pension$e401) 

# predictions
dml_MLIIVM$params_names()
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
lasso_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
lasso_y_MLIIVM

# cross-fitted RMSE: treatment
lasso_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
lasso_d_MLIIVM

# cross-fitted RMSE: instrument
lasso_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
lasso_z_MLIIVM

### random forest ###

set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_MLIIVM = DoubleMLIIVM$new(data_IV, ml_g = randomForest, 
                              ml_m = randomForest_class, ml_r = randomForest_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                                                                                               never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
forest_MLIIVM <- dml_MLIIVM$coef
forest_std_MLIIVM <- dml_MLIIVM$se

# predictions
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(Z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(Z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(Z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(Z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
forest_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
forest_y_MLIIVM

# cross-fitted RMSE: treatment
forest_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
forest_d_MLIIVM

# cross-fitted RMSE: instrument
forest_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
forest_z_MLIIVM

### trees ###

dml_MLIIVM = DoubleMLIIVM$new(data_IV, ml_g = trees, 
                              ml_m = trees_class, ml_r = trees_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                                                                                 never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
tree_MLIIVM <- dml_MLIIVM$coef
tree_std_MLIIVM <- dml_MLIIVM$se

# predictions
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(Z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(Z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(Z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(Z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
tree_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
tree_y_MLIIVM

# cross-fitted RMSE: treatment
tree_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
tree_d_MLIIVM

# cross-fitted RMSE: instrument
tree_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
tree_z_MLIIVM


### boosting ###
dml_MLIIVM = DoubleMLIIVM$new(data_IV, ml_g = boost, 
                              ml_m = boost_class, ml_r = boost_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                                                                                 never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
boost_MLIIVM <- dml_MLIIVM$coef
boost_std_MLIIVM <- dml_MLIIVM$se

# predictions
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(Z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(Z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(Z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(Z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
boost_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
boost_y_MLIIVM

# cross-fitted RMSE: treatment
boost_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
boost_d_MLIIVM

# cross-fitted RMSE: instrument
boost_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
boost_z_MLIIVM

#### Summarize results
table <- matrix(0, 5, 5)
table[1,1:5]   <- c(base.part$coefficients[1],lasso_MLIIVM,forest_MLIIVM,tree_MLIIVM,boost_MLIIVM)
table[2,1:5]   <- c(sqrt(base.part$vcov[1,1]),lasso_std_MLIIVM,forest_std_MLIIVM,tree_std_MLIIVM,boost_std_MLIIVM)
table[3,1:5]   <- c(sqrt(mean((y-mean(y))^2)),lasso_y_MLIIVM,forest_y_MLIIVM,tree_y_MLIIVM,boost_y_MLIIVM)
table[4,1:5]   <- c(sqrt(mean((d-mean(d))^2)),lasso_d_MLIIVM,forest_d_MLIIVM,tree_d_MLIIVM,boost_d_MLIIVM)
table[5,1:5]   <- c(sqrt(mean((z-mean(z))^2)),lasso_z_MLIIVM,forest_z_MLIIVM,tree_z_MLIIVM,boost_z_MLIIVM)
rownames(table) <- c("Estimate","Std.Error","RMSE Y","RMSE D","RMSE Z")
colnames(table) <- c("","Lasso","Random Forest","Trees","Boosting")
tab<- xtable(table, digits = 2)
tab

# Use best learner for each nuisance function
set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_MLIIVM = DoubleMLIIVM$new(data_IV, ml_g = boost, 
                              ml_m = lasso_class, ml_r = lasso_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                                                                                 never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
best_MLIIVM <- dml_MLIIVM$coef
best_std_MLIIVM <- dml_MLIIVM$se


