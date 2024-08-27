# Code for 401(k) example in Lecture notes 1 illustrating tree-based methods

######################### Libraries
library(rpart)
library(rpart.plot)
library(plotmo)
library(randomForest)
library(xtable)
library(xgboost)
library(fastDummies)

######################### Options
dpl = FALSE     # switch to save plots. Set to FALSE to just run code
codedir = "~/GitHub/NW2022_1Day/R Code" # Directory where Hansen keeps code
plotdir = "~/GitHub/NW2022_1Day/Slides/figures" # Directory where Hansen keeps figures for these slides

# Read data
#######################################################
setwd(codedir) 
data401k = read.table("../Data/restatw.dat", header = TRUE)  # Read in 401k data

# Variables we care about are
# net total financial assets (net_tfa)
# 401(k) participation (p401)
# 401(k) eligibility (e401)
# income (inc)
# age (age)
# family size (fsize)
# years schooling (educ)
# marital status (marr)
# gender (male)
# two-earner household (twoearn)

# Just keep the variables we will actually use in this example
data401k = subset(data401k, select=c(net_tfa, e401, age, 
                               inc, educ, fsize, marr, twoearn, db, pira, hown)) 
head(data401k)

#########################################
# Set up training and testing sample

#########################################
set.seed(8261977)
ntrain=8000    # number of training observations
tr = sample(1:nrow(data401k),ntrain)  # draw ntrain observations from original data
train = data401k[tr,]   # Training sample
test = data401k[-tr,]   # Testing sample
nTr = nrow(train)
nTe = nrow(test)

##########################################
# Illustrate regression tree methods on 401(k) data

##########################################
# Just fit a simple tree for illustration.
# No cross-validation or tuning
tree3 <- rpart(net_tfa ~ . , data = train, 
               control=rpart.control(maxdepth = 3, cp = .0001, xval = 0))

if(dpl) filenm = paste(plotdir,'/Tree1','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640)
  prp(tree3,yesno = 0, leaf.round=1, space=2, yspace=2, 
    split.space=2,shadow.col = "gray",trace = 1)
if(dpl) dev.off()

if(dpl) filenm = paste(plotdir,'/TreeFit','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640)
  plotmo(tree3, degree1 = 0, main = "Tree Fit")
if(dpl) dev.off()
  
if(dpl) filenm = paste(plotdir,'/TreeBins','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640)
  plotmo(tree3, degree1 = 0, type2 = "image", main = "Partition", image.col = heat.colors(640))
if(dpl) dev.off()
  
# Big Tree
tree4 <- rpart(net_tfa ~ . , data = train, 
               control=rpart.control(maxdepth = 15, cp = 0.00001, xval = 0))
if(dpl) filenm = paste(plotdir,'/Tree4','.pdf',sep='') 
if(dpl) pdf(file=filenm,height=10,width=10,)
prp(tree4,yesno = 0, leaf.round=1, space=2, yspace=2,
    split.space=2,shadow.col = "gray",trace = 1) # plotting the tree
if(dpl) dev.off()

# Two more trees for comparison
tree1 <- rpart(net_tfa ~ . , data = train, 
               control=rpart.control(maxdepth = 1, cp = .0001, xval = 0))
tree2 <- rpart(net_tfa ~ . , data = train, 
               control=rpart.control(maxdepth = 2, cp = .0001, xval = 0))


# In-sample and out-of-sample fit
r2 = c(1-sum(residuals(tree1)^2)/sum((train$net_tfa-mean(train$net_tfa))^2),
       1-sum(residuals(tree2)^2)/sum((train$net_tfa-mean(train$net_tfa))^2),
       1-sum(residuals(tree3)^2)/sum((train$net_tfa-mean(train$net_tfa))^2),
       1-sum(residuals(tree4)^2)/sum((train$net_tfa-mean(train$net_tfa))^2))

r2V = c(1-sum((test$net_tfa - predict(tree1, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2),
        1-sum((test$net_tfa - predict(tree2, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2),
        1-sum((test$net_tfa - predict(tree3, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2),
        1-sum((test$net_tfa - predict(tree4, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2))

r2.table <- matrix(0, 4, 2)
r2.table[,1]   <- r2
r2.table[,2]   <- r2V
rownames(r2.table)<- c("Depth 1","Depth 2",
                       "Depth 3","Big")
colnames(r2.table)<- c("in-sample","Validation")
r2.tab<- xtable(r2.table, digits =c(0,3,3))
r2.tab

########################## Simple tree (with all variables)
## Use CV built into rpart


# Big Tree
tree.cv <- rpart(net_tfa~. , data = train, 
                 control=rpart.control(maxdepth = 15, cp = 0.00001, xval = 5))  
# xval is the number of cross-validation splits. E.g. xval = 5 is five fold CV
fit.tree.cv <- printcp(tree.cv)
plotcp(tree.cv)  # Basic summary plot 

# plot cv function
if(dpl) filenm = paste(plotdir,'/CVTree','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=800,)
plot(seq(1:length(fit.tree.cv[,4])),fit.tree.cv[,4],col="black",type='l',
     ylim = c(min(fit.tree.cv[,3])-.01,1.01),xlab = "Complexity Parameter",
     ylab = expression(1-R^2), xaxt = "n")
axis(1, at = seq(1:length(fit.tree.cv[,4])), labels = round(fit.tree.cv[,1],5))
lines(seq(1:length(fit.tree.cv[,4])),fit.tree.cv[,4]-fit.tree.cv[,5],col="red",type='l')
lines(seq(1:length(fit.tree.cv[,4])),fit.tree.cv[,4]+fit.tree.cv[,5],col="red",type='l')
abline(h=min(fit.tree.cv[,4]), col = 'black', lty = 2)
lines(seq(1:length(fit.tree.cv[,4])),fit.tree.cv[,3],col="blue",type='l')
if(dpl) dev.off()

# Get CV min tree
bestcp=tree.cv$cptable[which.min(tree.cv$cptable[,"xerror"]),"CP"]
pruned.tree = prune(tree.cv,cp=bestcp)
# R^2 in validation sample
r2V.pruned = 1-sum((test$net_tfa - predict(pruned.tree, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2)
r2V.pruned

# Plot CV min tree
if(dpl) filenm = paste(plotdir,'/CVminTree','.pdf',sep='') 
if(dpl) pdf(file=filenm,height=10,width=10,)
prp(pruned.tree,yesno = 0, leaf.round=1, space=2, yspace=2,
    split.space=2,shadow.col = "gray",trace = 1) # plotting the tree
if(dpl) dev.off()

##############################################################
# Random forest

##############################################################
rForest.default = randomForest(net_tfa~. , data = train)   # Random forest with default settings
if(dpl) filenm = paste(plotdir,'/RFdefault','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=800,)
plot(rForest.default, main = "Asset Random Forest - Default Settings")  # Out-of-bag error estimates
if(dpl) dev.off()

r2V.rForest.default = 1-sum((test$net_tfa - predict(rForest.default, test))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)
cat("Validation R^2 for random forest with default settings:",r2V.rForest.default,"\n")

# Let's play with some tuning choices
# Don't randomize over variables
rForest.allx = randomForest(net_tfa~. , data = train, mtry = (ncol(train)-1))
plot(rForest.allx)
r2V.rForest.allx = 1-sum((test$net_tfa - predict(rForest.allx, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2)
cat("Validation R^2 for random forest with no x randomization:",r2V.rForest.allx,"\n")

# Restrict minimium node size to 30
rForest.30 = randomForest(net_tfa~. , data = train, nodesize = 30)
plot(rForest.30)
r2V.rForest.30 = 1-sum((test$net_tfa - predict(rForest.30, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2)
cat("Validation R^2 for random forest with min node 30:",r2V.rForest.30,"\n")

# Restrict minimium node size to 60
rForest.60 = randomForest(net_tfa~. , data = train, nodesize = 60)
plot(rForest.60)
r2V.rForest.60 = 1-sum((test$net_tfa - predict(rForest.60, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2)
cat("Validation R^2 for random forest with min node 60:",r2V.rForest.60,"\n")

# Restrict minimium node size to 120
rForest.120 = randomForest(net_tfa~. , data = train, nodesize = 120)
plot(rForest.120)
r2V.rForest.120 = 1-sum((test$net_tfa - predict(rForest.120, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2)
cat("Validation R^2 for random forest with min node 120:",r2V.rForest.120,"\n")

# Restrict minimium node size to 240
rForest.240 = randomForest(net_tfa~. , data = train, nodesize = 240)
plot(rForest.240)
r2V.rForest.240 = 1-sum((test$net_tfa - predict(rForest.240, test))^2)/sum((test$net_tfa - mean(train$net_tfa))^2)
cat("Validation R^2 for random forest with min node 240:",r2V.rForest.240,"\n")

# Table of results
rf.table <- matrix(0, 6, 2)
rf.table[1,]   <- c(rForest.default$rsq[length(rForest.default$rsq)],r2V.rForest.default)
rf.table[2,]   <- c(rForest.allx$rsq[length(rForest.allx$rsq)],r2V.rForest.allx)
rf.table[3,]   <- c(rForest.30$rsq[length(rForest.30$rsq)],r2V.rForest.30)
rf.table[4,]   <- c(rForest.60$rsq[length(rForest.60$rsq)],r2V.rForest.60)
rf.table[5,]   <- c(rForest.120$rsq[length(rForest.120$rsq)],r2V.rForest.120)
rf.table[6,]   <- c(rForest.240$rsq[length(rForest.240$rsq)],r2V.rForest.240)
rownames(rf.table)<- c("Default","No X Randomization",
                       "Min Size(30)","Min Size(60)",
                       "Min Size(120)","Min Size(240)")
colnames(rf.table)<- c("OOB R2","Validation R2")
rf.tab<- xtable(rf.table, digits =c(0,3,3))
rf.tab

###################################################################
# Boosted Trees

###################################################################
# xgboost needs a data matrix and it's own structure
train.xg <- as.matrix(train) # training sample
test.xg <- as.matrix(test) # testing sample

xgb_train = xgb.DMatrix(data = train.xg[,-1], label = train.xg[,1])
xgb_test = xgb.DMatrix(data = test.xg[,-1], label = test.xg[,1])

# Boosted tree with default xgboost settings and 500 boosting rounds. 
# Max depth = 6, learning rate = .3
xgboost.default.cv = xgb.cv(data = xgb_train,
                            nrounds = 500, print_every_n = 25, nfold = 5, 
                            showsd = FALSE) 
# xgboost's default CV training. Printing output as we go along for illustration
best.iter.default = which.min(as.matrix(xgboost.default.cv$evaluation_log[,4]))
# CV minimizing boosting iteration
xgboost.default = xgboost(data = xgb_train,
                          nrounds = 500, verbose = 0)
# Fit model using all training data

# In sample R^2
r2.boost.default.last = 
  1-sum((train$net_tfa - predict(xgboost.default, newdata = xgb_train))^2)/
  sum((train$net_tfa - mean(train$net_tfa))^2)
r2.boost.default.best = 
  1-sum((train$net_tfa - predict(xgboost.default, newdata = xgb_train,iterationrange = c(1,(best.iter.default+1))))^2)/
  sum((train$net_tfa - mean(train$net_tfa))^2)

# Validation R^2
r2V.boost.default.last = 
  1-sum((test$net_tfa - predict(xgboost.default, newdata = xgb_test))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)
r2V.boost.default.best = 
  1-sum((test$net_tfa - predict(xgboost.default, newdata = xgb_test,iterationrange = c(1,(best.iter.default+1))))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)

# Plot CV vs. in-sample fit
if(dpl) filenm = paste(plotdir,'/BoostDefault','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=800,)
plot(seq(1:500),as.matrix(xgboost.default.cv$evaluation_log[,4]),col="red",type='l',
     ylim = c(0,70000),xlab = "Boosting Iteration",
     ylab = "RMSE")
lines(seq(1:500),as.matrix(xgboost.default$evaluation_log[,2]),col="blue",type='l')
legend("topright", c("Validation","Training"), lty=c(1,1) ,col=c("red","blue"))
if(dpl) dev.off()

### Playing with tuning parameters. 500 boosting rounds
# Max depth = 6, learning rate = .1
xgboost.eta1.cv = xgb.cv(data = xgb_train,
                         nrounds = 500, print_every_n = 25, eta = .1,
                         showsd = FALSE, folds = xgboost.default.cv$folds) 
# xgboost's default CV training. Printing output as we go along for illustration.
# Using same folds as in default model.
best.iter.eta1 = which.min(as.matrix(xgboost.eta1.cv$evaluation_log[,4]))
# CV minimizing boosting iteration
xgboost.eta1 = xgboost(data = xgb_train,
                       nrounds = 500, verbose = 0, eta = .1)
# Fit model using all training data

# In sample R^2
r2.boost.eta1.last = 
  1-sum((train$net_tfa - predict(xgboost.eta1, newdata = xgb_train))^2)/
  sum((train$net_tfa - mean(train$net_tfa))^2)
r2.boost.eta1.best = 
  1-sum((train$net_tfa - predict(xgboost.eta1, newdata = xgb_train,iterationrange = c(1,(best.iter.eta1+1))))^2)/
  sum((train$net_tfa - mean(train$net_tfa))^2)

# Validation R^2
r2V.boost.eta1.last = 
  1-sum((test$net_tfa - predict(xgboost.eta1, newdata = xgb_test))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)
r2V.boost.eta1.best = 
  1-sum((test$net_tfa - predict(xgboost.eta1, newdata = xgb_test,iterationrange = c(1,(best.iter.eta1+1))))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)


### Playing with tuning parameters. 1000 boosting rounds
# Max depth = 1, learning rate = .1
xgboost.eta1.d1.cv = xgb.cv(data = xgb_train,
                            nrounds = 1000, verbose = 0, eta = .1, max_depth = 1,
                            showsd = FALSE, folds = xgboost.default.cv$folds) 
# xgboost's default CV training. Printing output as we go along for illustration.
# Using same folds as in default model.
best.iter.eta1.d1 = which.min(as.matrix(xgboost.eta1.d1.cv$evaluation_log[,4]))
# CV minimizing boosting iteration
xgboost.eta1.d1 = xgboost(data = xgb_train,
                          nrounds = 1000, verbose = 0, eta = .1, max_depth = 1)
# Fit model using all training data

# In sample R^2
r2.boost.eta1.d1.best = 
  1-sum((train$net_tfa - predict(xgboost.eta1.d1, newdata = xgb_train,iterationrange = c(1,(best.iter.eta1.d1+1))))^2)/
  sum((train$net_tfa - mean(train$net_tfa))^2)

# Validation R^2
r2V.boost.eta1.d1.best = 
  1-sum((test$net_tfa - predict(xgboost.eta1.d1, newdata = xgb_test,iterationrange = c(1,(best.iter.eta1.d1+1))))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)


### Playing with tuning parameters. 500 boosting rounds
# Max depth = 2, learning rate = .1
xgboost.eta1.d2.cv = xgb.cv(data = xgb_train,
                            nrounds = 500, verbose = 0, eta = .1, max_depth = 2,
                            showsd = FALSE, folds = xgboost.default.cv$folds) 
# xgboost's default CV training. Printing output as we go along for illustration.
# Using same folds as in default model.
best.iter.eta1.d2 = which.min(as.matrix(xgboost.eta1.d2.cv$evaluation_log[,4]))
# CV minimizing boosting iteration
xgboost.eta1.d2 = xgboost(data = xgb_train,
                          nrounds = 500, verbose = 0, eta = .1, max_depth = 2)
# Fit model using all training data

# In sample R^2
r2.boost.eta1.d2.best = 
  1-sum((train$net_tfa - predict(xgboost.eta1.d2, newdata = xgb_train,iterationrange = c(1,(best.iter.eta1.d2+1))))^2)/
  sum((train$net_tfa - mean(train$net_tfa))^2)

# Validation R^2
r2V.boost.eta1.d2.best = 
  1-sum((test$net_tfa - predict(xgboost.eta1.d2, newdata = xgb_test,iterationrange = c(1,(best.iter.eta1.d2+1))))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)


### Playing with tuning parameters. 500 boosting rounds
# Max depth = 3, learning rate = .1
xgboost.eta1.d3.cv = xgb.cv(data = xgb_train,
                            nrounds = 500, verbose = 0, eta = .1, max_depth = 3,
                            showsd = FALSE, folds = xgboost.default.cv$folds) 
# xgboost's default CV training. Printing output as we go along for illustration.
# Using same folds as in default model.
best.iter.eta1.d3 = which.min(as.matrix(xgboost.eta1.d3.cv$evaluation_log[,4]))
# CV minimizing boosting iteration
xgboost.eta1.d3 = xgboost(data = xgb_train,
                          nrounds = 500, verbose = 0, eta = .1, max_depth = 3)
# Fit model using all training data

# In sample R^2
r2.boost.eta1.d3.best = 
  1-sum((train$net_tfa - predict(xgboost.eta1.d3, newdata = xgb_train,iterationrange = c(1,(best.iter.eta1.d3+1))))^2)/
  sum((train$net_tfa - mean(train$net_tfa))^2)

# Validation R^2
r2V.boost.eta1.d3.best = 
  1-sum((test$net_tfa - predict(xgboost.eta1.d3, newdata = xgb_test,iterationrange = c(1,(best.iter.eta1.d3+1))))^2)/
  sum((test$net_tfa - mean(train$net_tfa))^2)


## Tabulate R^2 results
boost.table <- matrix(0, 7, 6)
boost.table[1,]   <- c(6,.3,500,as.matrix(xgboost.default.cv$evaluation_log[500,4]),
                       r2.boost.default.last,r2V.boost.default.last)
boost.table[2,]   <- c(6,.3,best.iter.default,min(as.matrix(xgboost.default.cv$evaluation_log[,4])),
                       r2.boost.default.best,r2V.boost.default.best)
boost.table[3,]   <- c(6,.1,500,as.matrix(xgboost.eta1.cv$evaluation_log[500,4]),
                       r2.boost.eta1.last,r2V.boost.eta1.last)
boost.table[4,]   <- c(6,.1,best.iter.eta1,min(as.matrix(xgboost.eta1.cv$evaluation_log[,4])),
                       r2.boost.eta1.best,r2V.boost.eta1.best)
boost.table[5,]   <- c(1,.1,best.iter.eta1.d1,min(as.matrix(xgboost.eta1.d1.cv$evaluation_log[,4])),
                       r2.boost.eta1.d1.best,r2V.boost.eta1.d1.best)
boost.table[6,]   <- c(2,.1,best.iter.eta1.d2,min(as.matrix(xgboost.eta1.d2.cv$evaluation_log[,4])),
                       r2.boost.eta1.d2.best,r2V.boost.eta1.d2.best)
boost.table[7,]   <- c(3,.1,best.iter.eta1.d3,min(as.matrix(xgboost.eta1.d3.cv$evaluation_log[,4])),
                       r2.boost.eta1.d3.best,r2V.boost.eta1.d3.best)
colnames(boost.table)<- c("Depth","Rate","Iter","CV RMSE","in-sample R2","Validation R2")
boost.tab<- xtable(boost.table, digits =c(0,0,1,0,3,3,3))
boost.tab


