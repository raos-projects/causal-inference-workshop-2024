# Code for 401(k) example in Lecture notes 1 illustrating HDLMs

######################### Libraries
library(glmnet)
library(xtable)

######################### Options
dpl = FALSE      # switch to save plots. Set to FALSE to just run code
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

# Normalize inc, age, fsize, educ
data401k$age = data401k$age/64
data401k$inc = data401k$inc/250000
data401k$fsize = data401k$fsize/13
data401k$educ = data401k$educ/18


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
# Some model specifications

#########################################
eq.base = net_tfa ~ e401 + age + 
  inc + educ + fsize + marr + twoearn + db + pira + hown

eq.poly = net_tfa ~ e401 + poly(age, 6, raw=TRUE) + 
  poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) + 
  poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown

eq.int = net_tfa ~ (e401 + poly(age, 6, raw=TRUE) + 
  poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) + 
  poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown)^2

################################################
# Baseline OLS results

################################################

# Estimate models in training data
reg.base <- lm(eq.base, data=train)
cat( "Number of regressors in the base model:",length(!is.na(reg.base$coef)), '\n') # number of regressors in the Basic Model

reg.poly <- lm(eq.poly, data=train)
cat("Number of regressors in the polynomial model", sum(!is.na(reg.poly$coefficients)), "\n") # number of regressors in the polynomial Model

reg.int <- lm(eq.int, data=train)
cat("Number of regressors in the interacted model", sum(!is.na(reg.int$coefficients)), "\n") # number of regressors in the interacted Model

# calculating the out-of-sample MSE and R^2
fit.reg.base <- predict(reg.base, newdata=test)
fit.reg.poly <- predict(reg.poly, newdata=test)
fit.reg.int <- predict(reg.int, newdata=test)

y.test <- test$net_tfa

MSE.test1 <- sum((y.test-fit.reg.base)^2)/length(y.test)
R2.test1<- 1- MSE.test1/(sum((y.test-mean(train$net_tfa))^2)/length(y.test))
MSE.test2 <- sum((y.test-fit.reg.poly)^2)/length(y.test)
R2.test2<- 1- MSE.test2/(sum((y.test-mean(train$net_tfa))^2)/length(y.test))
MSE.test3 <- sum((y.test-fit.reg.int)^2)/length(y.test)
R2.test3<- 1- MSE.test3/(sum((y.test-mean(train$net_tfa))^2)/length(y.test))

cat("Test R2 for the basic model: ", R2.test1)
cat("Test R2 for the polynomial model: ", R2.test2)
cat("Test R2 for the interacted model: ", R2.test3)

# in-sample MSE and R^2
sum.base <- summary(reg.base)
sum.poly <- summary(reg.poly)
sum.int <- summary(reg.int)

# R-squared and adjusted R-squared
R2V.1 <- sum.base$r.squared
cat("R-squared for the basic model: ", R2V.1, "\n")
R2V.adj1 <- sum.base$adj.r.squared
cat("adjusted R-squared for the basic model: ", R2V.adj1, "\n")

R2V.2 <- sum.poly$r.squared
cat("R-squared for the polynomial model: ", R2V.2, "\n")
R2V.adj2 <- sum.poly$adj.r.squared
cat("adjusted R-squared for the polynomial model: ", R2V.adj2, "\n")

R2V.3 <- sum.int$r.squared
cat("R-squared for the interacted model: ", R2V.3, "\n")
R2V.adj3 <- sum.int$adj.r.squared
cat("adjusted R-squared for the interacted model: ", R2V.adj3, "\n")


# Output the table
table <- matrix(0, 3, 3)
table[1,1:3]   <- c(R2.test1,R2V.1,R2V.adj1)
table[2,1:3]   <- c(R2.test2,R2V.2,R2V.adj2)
table[3,1:3]   <- c(R2.test3,R2V.3,R2V.adj3)
colnames(table)<- c("R^2_{test}$","$R^2_{sample}$","$R^2_{adjusted}$")
rownames(table)<- c("basic","polynomial", "interacted")
tab<- xtable(table, digits =c(0,3,3,3))
print(tab,type="latex") 
tab

##############################################################
# HDLMs

#############################################################
# glmnet uses x, y not formula, so get design matrices and outcomes

#############################################################
xtr.base = model.matrix(eq.base,train)[,-1]
xte.base = model.matrix(eq.base,test)[,-1]
xtr.poly = model.matrix(eq.poly,train)[,-1]
xte.poly = model.matrix(eq.poly,test)[,-1]
xtr.int = model.matrix(eq.int,train)[,-1]
xte.int = model.matrix(eq.int,test)[,-1]

ytr = train$net_tfa
yte = test$net_tfa

################################################################
# Let's see what penalized coefficients look like for 
# different penalty parameters

################################################################

## Fit ridge on grid of lambda values (chosen by default using glmnet) using polynomial model.
ridge.poly = glmnet(xtr.poly,ytr,alpha=0)  # alpha = 0 gives ridge
ridge.coef = ridge.poly$beta     # estimated slope coefficients
ridge.lambda = ridge.poly$lambda # values of penalty parameter

# "Spaghetti plot" 
if(dpl) filenm = paste(plotdir,'/RidgeSpaghetti','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot.lambda = which(ridge.lambda < 2e6)
plot(range(ridge.lambda[plot.lambda]),range(ridge.coef[,plot.lambda]),type='n',
     main='Ridge',xlab=expression(lambda),ylab='Coefficients',cex.lab=1.5)
for(i in 1:nrow(ridge.coef)) lines(ridge.lambda[plot.lambda],ridge.coef[i,plot.lambda],col=i+1,type='l')
if(dpl) dev.off()

##
## Fit lasso on grid of lambda values (chosen by default using glmnet) using polynomial model.
lasso.poly = glmnet(xtr.poly,ytr)  # default is lasso (equivalent to alpha = 1)
lasso.coef = lasso.poly$beta     # estimated slope coefficients
lasso.lambda = lasso.poly$lambda # values of penalty parameter

# "Spaghetti plot" for "main variables" 
if(dpl) filenm = paste(plotdir,'/LassoSpaghetti','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot.lambda = which(lasso.lambda < 1e6)
plot(range(lasso.lambda[plot.lambda]),range(lasso.coef[,plot.lambda]),type='n',
     main='Lasso',xlab=expression(lambda),ylab='Coefficients',cex.lab=1.5)
for(i in 1:nrow(lasso.coef)) lines(lasso.lambda[plot.lambda],lasso.coef[i,plot.lambda],col=i+1,type='l')
if(dpl) dev.off()

#################################################################
## Cross-validation for penalty parameter choice

#################################################################
cv.lasso.base = cv.glmnet(xtr.base, ytr, nfolds = 5, keep = TRUE) # keep = TRUE will keep fold ids
cv.lasso.poly = cv.glmnet(xtr.poly, ytr, foldid = cv.lasso.base$foldid)
cv.lasso.int = cv.glmnet(xtr.int, ytr, foldid = cv.lasso.base$foldid)

cv.ridge.base = cv.glmnet(xtr.base, ytr, alpha = 0, foldid = cv.lasso.base$foldid)
cv.ridge.poly = cv.glmnet(xtr.poly, ytr, alpha = 0, foldid = cv.lasso.base$foldid)
cv.ridge.int = cv.glmnet(xtr.int, ytr, alpha = 0, foldid = cv.lasso.base$foldid)

## Calculate CV for OLS for comparison and fold by fold fits for validation data
cv.ols.base = 0
cv.ols.poly = 0
cv.ols.int = 0

yhat.te.base = matrix(0,nTe,5)
yhat.te.poly = matrix(0,nTe,5)
yhat.te.int = matrix(0,nTe,5)

yhat.te.lasso.base = matrix(0,nTe,5)
yhat.te.lasso.poly = matrix(0,nTe,5)
yhat.te.lasso.int = matrix(0,nTe,5)

yhat.te.ridge.base = matrix(0,nTe,5)
yhat.te.ridge.poly = matrix(0,nTe,5)
yhat.te.ridge.int = matrix(0,nTe,5)

# Compute squared forecast loss across folds
for(k in 1:5) {
  indk = cv.lasso.base$foldid == k
  
  # OLS
  K.train = train[!indk,]
  K.test = train[indk,]
  cv.ols.base = cv.ols.base +
    sum((K.test$net_tfa - predict(lm(eq.base, data = K.train), K.test))^2)
  cv.ols.poly = cv.ols.poly +
    sum((K.test$net_tfa - predict(lm(eq.poly, data = K.train), K.test))^2)
  cv.ols.int = cv.ols.int + 
    sum((K.test$net_tfa - predict(lm(eq.int, data = K.train), K.test))^2)
  
  yhat.te.base[,k] = predict(lm(eq.base, data = K.train), test)
  yhat.te.poly[,k] = predict(lm(eq.poly, data = K.train), test)
  yhat.te.int[,k] = predict(lm(eq.int, data = K.train), test)
  
  yhat.te.lasso.base[,k] = predict(
    glmnet(xtr.base[!indk,],ytr[!indk],lambda = cv.lasso.base$lambda.min), 
    newx = xte.base)
  yhat.te.lasso.poly[,k] = predict(
    glmnet(xtr.poly[!indk,],ytr[!indk],lambda = cv.lasso.poly$lambda.min), 
    newx = xte.poly)
  yhat.te.lasso.int[,k] = predict(
    glmnet(xtr.int[!indk,],ytr[!indk],lambda = cv.lasso.int$lambda.min), 
    newx = xte.int)

  yhat.te.ridge.base[,k] = predict(
    glmnet(xtr.base[!indk,],ytr[!indk],alpha = 0,lambda = cv.ridge.base$lambda.min), 
    newx = xte.base)
  yhat.te.ridge.poly[,k] = predict(
    glmnet(xtr.poly[!indk,],ytr[!indk],alpha = 0,lambda = cv.ridge.poly$lambda.min), 
    newx = xte.poly)
  yhat.te.ridge.int[,k] = predict(
    glmnet(xtr.int[!indk,],ytr[!indk],alpha = 0,lambda = cv.ridge.int$lambda.min), 
    newx = xte.int)
  
}
cv.ols.base = cv.ols.base/nTr
cv.ols.poly = cv.ols.poly/nTr
cv.ols.int = cv.ols.int/nTr

## Plots of CV functions
# Ridge - basic
if(dpl) filenm = paste(plotdir,'/RidgeBasicCV','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot(cv.ridge.base$lambda,cv.ridge.base$cvm,type='l',
     main='Ridge - Base',xlab=expression(lambda),ylab='CV MSE',cex.lab=1.5)
abline(v=cv.ridge.base$lambda[which.min(cv.ridge.base$cvm)],col='red')
abline(h=cv.ols.base, col = 'blue')
if(dpl) dev.off()

# Ridge - polynomial
if(dpl) filenm = paste(plotdir,'/RidgePolyCV','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot(cv.ridge.poly$lambda,cv.ridge.poly$cvm,type='l',
     main='Ridge - Polynomial',xlab=expression(lambda),ylab='CV MSE',cex.lab=1.5)
abline(v=cv.ridge.poly$lambda[which.min(cv.ridge.poly$cvm)],col='red')
abline(h=cv.ols.base, col = 'blue')
if(dpl) dev.off()

# Ridge - interactions
if(dpl) filenm = paste(plotdir,'/RidgeIntCV','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot(cv.ridge.int$lambda,cv.ridge.int$cvm,type='l',
     main='Ridge - Interactions',xlab=expression(lambda),ylab='CV MSE',cex.lab=1.5)
abline(v=cv.ridge.int$lambda[which.min(cv.ridge.int$cvm)],col='red')
abline(h=cv.ols.base, col = 'blue')
if(dpl) dev.off()

# Lasso - basic
if(dpl) filenm = paste(plotdir,'/LassoBasicCV','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot(cv.lasso.base$lambda,cv.lasso.base$cvm,type='l',
     main='Lasso - Base',xlab=expression(lambda),ylab='CV MSE',cex.lab=1.5)
abline(v=cv.lasso.base$lambda[which.min(cv.lasso.base$cvm)],col='red')
abline(h=cv.ols.base, col = 'blue')
if(dpl) dev.off()

# Lasso - polynomial
if(dpl) filenm = paste(plotdir,'/LassoPolyCV','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot(cv.lasso.poly$lambda,cv.lasso.poly$cvm,type='l',
     main='Lasso - Polynomial',xlab=expression(lambda),ylab='CV MSE',cex.lab=1.5)
abline(v=cv.lasso.poly$lambda[which.min(cv.lasso.poly$cvm)],col='red')
abline(h=cv.ols.base, col = 'blue')
if(dpl) dev.off()

# Lasso - interactions
if(dpl) filenm = paste(plotdir,'/LassoIntCV','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
plot(cv.lasso.int$lambda,cv.lasso.int$cvm,type='l',
     main='Lasso - Interactions',xlab=expression(lambda),ylab='CV MSE',cex.lab=1.5)
abline(v=cv.lasso.int$lambda[which.min(cv.lasso.int$cvm)],col='red')
abline(h=cv.ols.base, col = 'blue')
if(dpl) dev.off()

## Table of CV RMSEs
table <- matrix(0, 3, 3)
table[1,]   <- c(sqrt(cv.ols.base),sqrt(cv.ols.poly),sqrt(cv.ols.int))
table[2,]   <- c(sqrt(min(cv.ridge.base$cvm)),sqrt(min(cv.ridge.poly$cvm)),sqrt(min(cv.ridge.int$cvm)))
table[3,]   <- c(sqrt(min(cv.lasso.base$cvm)),sqrt(min(cv.lasso.poly$cvm)),sqrt(min(cv.lasso.int$cvm)))
colnames(table)<- c("Base","Polynomial","Interactions")
rownames(table)<- c("OLS","Ridge","Lasso")
tab<- xtable(table, digits =c(0,0,0,0))
tab



##############################################################
## Performance on validation data

################################################################
# Prediction rule one - average cross-validated fits
r2.CVave.ols.base = 1-sum((yte-rowMeans(yhat.te.base))^2)/
  sum((yte-mean(ytr))^2)
r2.CVave.ols.poly = 1-sum((yte-rowMeans(yhat.te.poly))^2)/
  sum((yte-mean(ytr))^2)
r2.CVave.ols.int = 1-sum((yte-rowMeans(yhat.te.int))^2)/
  sum((yte-mean(ytr))^2)

r2.CVave.lasso.base = 1-sum((yte-rowMeans(yhat.te.lasso.base))^2)/
  sum((yte-mean(ytr))^2)
r2.CVave.lasso.poly = 1-sum((yte-rowMeans(yhat.te.lasso.poly))^2)/
  sum((yte-mean(ytr))^2)
r2.CVave.lasso.int = 1-sum((yte-rowMeans(yhat.te.lasso.int))^2)/
  sum((yte-mean(ytr))^2)

r2.CVave.ridge.base = 1-sum((yte-rowMeans(yhat.te.ridge.base))^2)/
  sum((yte-mean(ytr))^2)
r2.CVave.ridge.poly = 1-sum((yte-rowMeans(yhat.te.ridge.poly))^2)/
  sum((yte-mean(ytr))^2)
r2.CVave.ridge.int = 1-sum((yte-rowMeans(yhat.te.ridge.int))^2)/
  sum((yte-mean(ytr))^2)

# Prediction rule two - refit on entire data set
r2.CVref.lasso.base = 1 - 
  sum((yte - predict(cv.lasso.base, newx = xte.base, s = "lambda.min"))^2)/
  sum((yte-mean(ytr))^2)
r2.CVref.lasso.poly = 1 - 
  sum((yte - predict(cv.lasso.poly, newx = xte.poly, s = "lambda.min"))^2)/
  sum((yte-mean(ytr))^2)
r2.CVref.lasso.int = 1 - 
  sum((yte - predict(cv.lasso.int, newx = xte.int, s = "lambda.min"))^2)/
  sum((yte-mean(ytr))^2)

r2.CVref.ridge.base = 1 - 
  sum((yte - predict(cv.ridge.base, newx = xte.base, s = "lambda.min"))^2)/
  sum((yte-mean(ytr))^2)
r2.CVref.ridge.poly = 1 - 
  sum((yte - predict(cv.ridge.poly, newx = xte.poly, s = "lambda.min"))^2)/
  sum((yte-mean(ytr))^2)
r2.CVref.ridge.int = 1 - 
  sum((yte - predict(cv.ridge.int, newx = xte.int, s = "lambda.min"))^2)/
  sum((yte-mean(ytr))^2)

table <- matrix(0, 6, 3)
table[1,]   <- c(r2.CVave.ols.base,r2.CVave.ols.poly,r2.CVave.ols.int)
table[2,]   <- c(R2.test1,R2.test2,R2.test3)
table[3,]   <- c(r2.CVave.ridge.base,r2.CVave.ridge.poly,r2.CVave.ridge.int)
table[4,]   <- c(r2.CVref.ridge.base,r2.CVref.ridge.poly,r2.CVref.ridge.int)
table[5,]   <- c(r2.CVave.lasso.base,r2.CVave.lasso.poly,r2.CVave.lasso.int)
table[6,]   <- c(r2.CVref.lasso.base,r2.CVref.lasso.poly,r2.CVref.lasso.int)
colnames(table)<- c("Base","Polynomial","Interactions")
rownames(table)<- c("OLS - average","OLS - refit","Ridge - average",
                    "Ridge - refit","Lasso - average","Lasso - refit")
tab<- xtable(table, digits =c(0,3,3,3))
tab
