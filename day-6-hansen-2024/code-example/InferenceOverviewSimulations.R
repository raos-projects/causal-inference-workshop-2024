# Simulations for illustrating inferential issues

######################### Libraries
library(glmnet)
library(keras)
library(sandwich)
library(xtable)

######################### Options
dpl = FALSE      # switch to save plots. Set to FALSE to just run code
codedir = "~/GitHub/NW2022_1Day/R Code" # Directory where Hansen keeps code
plotdir = "~/GitHub/NW2022_1Day/Slides/figures" # Directory where Hansen keeps figures for these slides

#########################################################
### Simple simulation to illustrate problem of regularized inference

set.seed(5192022)
nRep = 1000 # Number of simulation replications
n = 100 # Simulation sample size
alphad = .25 # parameter of interest
alphax = .1 # coefficient on "control"
betax = 1 # coefficient on x in "first stage"

bhat.cv = matrix(0,nRep,2)
bhat.se = matrix(0,nRep,2)
bhat.ls = matrix(0,nRep,2)
for(ii in 1:nRep) {
  
  x = rnorm(n)
  d = betax*x + .25*rnorm(n)
  y = alphad*d + alphax*x + rnorm(n)
  
  X = cbind(d,x)
  las.fit = cv.glmnet(X,y)
  bhat.cv[ii,] = coef(las.fit, s = "lambda.min")[2:3]
  bhat.se[ii,] = coef(las.fit, s = "lambda.1se")[2:3]
  bhat.ls[ii,] = coef(lm(y~X))[2:3]
  
}

# summarize results
table <- matrix(0, 3, 6)
table[1,]   <- c(colMeans(bhat.ls),colMeans(bhat.cv),colMeans(bhat.se))
table[2,]   <- c(apply(bhat.ls, 2, sd), apply(bhat.cv, 2, sd), apply(bhat.se, 2, sd))
table[3,]   <- c(colMeans(bhat.ls == 0),colMeans(bhat.cv == 0),colMeans(bhat.se == 0))
colnames(table)<- c("alphaOLS","betaOLS","alphaCV","betaCV","alpha1SE","beta1SE")
rownames(table)<- c("Mean","Std. Dev.","Fraction 0")
tab<- xtable(table, digits =c(0,3,3,3,3,3,3))
tab

# Histograms
if(dpl) filenm = paste(plotdir,'/N6ToySim','.png',sep='') 
if(dpl) png(file=filenm,height=480,width=720,)
par(mfcol = c(2,3))
hist(bhat.ls[,1], 
     main = expression(hat(alpha)[OLS]), 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alphad, lwd = 2)

hist(bhat.ls[,2], 
     main = expression(hat(beta)[OLS]), 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alphax, lwd = 2)

hist(bhat.cv[,1], 
     main = expression(hat(alpha)[Lasso.CV]), 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alphad, lwd = 2)

hist(bhat.cv[,2], 
     main = expression(hat(beta)[Lasso.CV]), 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alphax, lwd = 2)

hist(bhat.se[,1], 
     main = expression(hat(alpha)[Lasso.1SE]), 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alphad, lwd = 2)

hist(bhat.se[,2], 
     main = expression(hat(beta)[Lasso.1SE]), 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alphax, lwd = 2)
if(dpl) dev.off()


#########################################################
### Simple simulation to illustrate moment conditions in high-dimensional
### linear model

nRep = 1000 # Number of simulation replications
n = 200 # Simulation sample size
p = 200 # Number of elements in X

betam = c(1,.5,rep(0,198)) # Coefficients for D on X
betay = c(.5,1,rep(0,198)) # Coefficients on X for Y on D and X
alpha0 = .25 # Coefficient on D for Y on D and X

alphahat = matrix(0,nRep,3)
for(ii in 1:nRep){
  X = matrix(rnorm(n*p),n) 
  D = X%*%betam + rnorm(n)
  Y = D*alpha0 + X%*%betay + rnorm(n)
  
  # moment (1)
  lambda.r = 2.2/sqrt(n)*qnorm(1-(.1/log(n))/(2*p))  # Plug-in penalty from BCH using true sigma
                                                     # Works best with post-Lasso
  res1 = glmnet(cbind(D,X), Y, lambda = lambda.r, penalty.factor = c(0,rep(1,p))) 
      # lasso without penalizing D
  # OLS with selected variables
  if (length(which(res1$beta[2:(p+1)] != 0)) > 0) {
    alphahat[ii,1] = lm(Y~D+X[,which(res1$beta[2:(p+1)] != 0)])$coefficient[2]
  } 
  else {
    alphahat[ii,1] = lm(Y~D)$coefficient[2]
  }
    
  # moment (2)
  res2 = glmnet(X, D, lambda = lambda.r) # lasso of D on X
  
  if (length(which(res2$beta != 0)) > 0) {
    r.D = D - predict(lm(D~X[,which(res2$beta != 0)]), newx = X[,which(res2$beta != 0)]) 
      # post-lasso of D on X with plug-in 
    alphahat[ii,2] = crossprod(r.D,Y)/crossprod(r.D,D)
  }
  else {
    r.D = D - mean(D)
    alphahat[ii,2] = crossprod(r.D,Y)/crossprod(r.D,D)
  }
  
  # moment (3) - Double selection
  res3 = glmnet(X, Y, lambda = lambda.r) # lasso of Y on X
  
  use3 = union(which(res2$beta != 0),which(res3$beta != 0))
  if (length(use3) > 0) {
    alphahat[ii,3] = lm(Y~D+X[,use3])$coefficients[2]
  }
  else {
    r.Y = Y - mean(Y)
    alphahat[ii,3] = crossprod(r.D,r.Y)/crossprod(r.D)
  }
  
}
colMeans(alphahat)
apply(alphahat, 2, sd)

if(dpl) filenm = paste(plotdir,'/N6HDLMSim','.png',sep='') 
if(dpl) png(file=filenm,height=240,width=720,)
par(mfcol = c(1,3))
hist(alphahat[,1], 
     main = "Moment (1)", 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alpha0, lwd = 2)

hist(alphahat[,2], 
     main = "Moment (2)", 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alpha0, lwd = 2)

hist(alphahat[,3], 
     main = "Moment (3)", 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alpha0, lwd = 2)

if(dpl) dev.off()

#########################################################
### Simple simulation to illustrate impact of over-fitting and sample splitting
### in partially linear model using DNN as learner

set.seed(1234)

n_rep = 1000
n_obs = 200
n_vars = 150

alpha = 0.5 # Coefficient of interest

SX = chol(toeplitz(.7^(seq(0,n_vars-1)))) # cholesky of covariance matrix for controls

alphahat = matrix(0,n_rep,2) # Estimate using orthogonal moment condition using 
                             # full sample and sample splitting
se.alphahat = matrix(0, n_rep, 2) # Standard errors
for(ii in 1:n_rep){
  
  cat(paste(ii, " ")) 
  flush.console()
  
  X = matrix(rnorm(n_obs*n_vars),n_obs)%*%SX 
  D = X[,1] + .25*(exp(X[,3])/(1+exp(X[,3]))) + rnorm(n_obs)
  Y = D*alpha + X[,3] + .25*(exp(X[,1])/(1+exp(X[,1]))) + rnorm(n_obs)

  # Full sample estimates
  model.Y <- keras_model_sequential()
  model.Y %>% layer_dense(units = 50, activation = 'relu', input_shape = c(n_vars)) %>% 
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model.Y %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.model.Y <- model.Y %>% fit(
    X, Y, 
    epochs = 100, batch_size = 25, 
    verbose = 0
  )
  
  rY = Y-predict(model.Y, X)
  
  k_clear_session()
  
  model.D <- keras_model_sequential()
  model.D %>% layer_dense(units = 50, activation = 'relu', input_shape = c(n_vars)) %>% 
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model.D %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.model.D <- model.D %>% fit(
    X, D, 
    epochs = 100, batch_size = 25, 
    verbose = 0
  )
  
  rD = D-predict(model.D, X)
  
  k_clear_session()
  
  alphahat[ii,1] = lm(rY~rD-1)$coefficients
  se.alphahat[ii,1] = sqrt(vcovHC(lm(rY~rD-1)))
  
  # Split sample estimate
  random = sample(1:n_obs, floor(n_obs/2))
  Y1 = Y[random]
  Y2 = Y[-random]
  D1 = D[random]
  D2 = D[-random]
  X1 = X[random,]
  X2 = X[-random,]
  
  model.Y1 <- keras_model_sequential()
  model.Y1 %>% layer_dense(units = 50, activation = 'relu', input_shape = c(n_vars)) %>% 
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model.Y1 %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.model.Y1 <- model.Y1 %>% fit(
    X2, Y2, 
    epochs = 100, batch_size = 25, 
    verbose = 0
  )
  
  rY1 = Y1-predict(model.Y1, X1)
  
  k_clear_session()
  
  model.Y2 <- keras_model_sequential()
  model.Y2 %>% layer_dense(units = 50, activation = 'relu', input_shape = c(n_vars)) %>% 
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model.Y2 %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.model.Y2 <- model.Y2 %>% fit(
    X1, Y1, 
    epochs = 100, batch_size = 25, 
    verbose = 0
  )
  
  rY2 = Y2-predict(model.Y2, X2)  
  
  k_clear_session()
  
  model.D1 <- keras_model_sequential()
  model.D1 %>% layer_dense(units = 50, activation = 'relu', input_shape = c(n_vars)) %>% 
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model.D1 %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.model.D1 <- model.D1 %>% fit(
    X2, D2, 
    epochs = 100, batch_size = 25, 
    verbose = 0
  )
  
  rD1 = D1-predict(model.D1, X1)
  
  k_clear_session()
  
  model.D2 <- keras_model_sequential()
  model.D2 %>% layer_dense(units = 50, activation = 'relu', input_shape = c(n_vars)) %>% 
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model.D2 %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.model.D2 <- model.D2 %>% fit(
    X1, D1, 
    epochs = 100, batch_size = 25, 
    verbose = 0
  )
  
  rD2 = D2-predict(model.D2, X2)  

  k_clear_session()
    
  rY = c(rY1,rY2)
  rD = c(rD1,rD2)
  
  alphahat[ii,2] = lm(rY~rD-1)$coefficients  
  se.alphahat[ii,2] = sqrt(vcovHC(lm(rY~rD-1)))
}
  
colMeans(alphahat)
apply(alphahat, 2, sd)


if(dpl) filenm = paste(plotdir,'/N6OverfitSimSolo','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=640,)
hist(alphahat[,1], 
     main = "Coefficient Estimate", 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alpha, lwd = 2)

if(dpl) dev.off()


if(dpl) filenm = paste(plotdir,'/N6OverfitSim','.png',sep='') 
if(dpl) png(file=filenm,height=360,width=720,)
par(mfcol = c(1,2))
hist(alphahat[,1], 
     main = "Full Sample", 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alpha, lwd = 2)

hist(alphahat[,2], 
     main = "Split Sample", 
     xlab = "Coefficient", col = "cadetblue1",
     breaks = 25)
abline(v = alpha, lwd = 2)

if(dpl) dev.off()

# Inference
colMeans(abs(alphahat-alpha)/se.alphahat > 1.96)
