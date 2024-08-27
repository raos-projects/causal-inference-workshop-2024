# Code for 401(k) example illustrating Stacking

######################### Libraries
library(glmnet)
library(randomForest)
library(xgboost)
library(keras)
library(xtable)
library(fastDummies)
library(LowRankQP)

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

# Just keep the variables we will actually use in this example
data401k = subset(data401k, select=c(net_tfa, e401, age, 
                                     inc, educ, fsize, marr, twoearn, db, pira, hown)) 
head(data401k)

# Normalize inc, age, fsize, educ
data401k$age = data401k$age/64
data401k$inc = data401k$inc/250000
data401k$fsize = data401k$fsize/13
data401k$educ = data401k$educ/18

######################## Candidate learners
# 1. OLS - basic
# 2. OLS - interactive
# 3. Lasso (CV) - interactive
# 4. Ridge (CV) - interactive
# 5. Random forest
# 6. Boosted trees - depth 3
# 7. Boosted trees - depth 6
# 8. DNN - input/50/50/output, .5 dropout on each layer
# 9. DNN - input/50/50/50/50/output, .5 dropout on each layer
# 10. DNN - input/50/50/50/50/output, early stopping

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

# Put together data for linear-based approaches
# Split into validation and training data
trainL <- train # training sample
testL <- test # testing sample

# Model specifications
eq.base = net_tfa ~ e401 + age + 
  inc + educ + fsize + marr + twoearn + db + pira + hown
eq.int = net_tfa ~ (e401 + poly(age, 6, raw=TRUE) + 
                      poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) + 
                      poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown)^2

# design matrices and outcome vectors
xtr = model.matrix(eq.int,trainL)[,-1]
xte = model.matrix(eq.int,testL)[,-1]

ytr = trainL$net_tfa
yte = testL$net_tfa

# Put together data for representation learners
### Random forest data

trainRF <- train # training sample
testRF <- test # testing sample

### Neural net and boosted trees data

# data matrices for xgboost
trainXG <- as.matrix(train) # training sample
testXG <- as.matrix(test) # testing sample
xgb_train = xgb.DMatrix(data = trainXG[,-1], label = trainXG[,1])
xgb_test = xgb.DMatrix(data = testXG[,-1], label = testXG[,1])

# design matrices and outcome vectors for neural nets
trainNN <- train # training sample
trainNN.X <- as.matrix(trainNN[,-1])
trainNN.y <- as.matrix(trainNN[,1])
testNN <- test # testing sample
testNN.X <- as.matrix(testNN[,-1])
testNN.y <- as.matrix(testNN[,1])
p = ncol(trainNN.X)

############################################### Cross-validation

# Form CV groups
ntr = length(ytr)
nte = length(yte)
Kf = 5  # Number of folds
sampleframe <- rep(1:Kf, ceiling( ntr/Kf ) ) 
cvgroup <- sample( sampleframe , size=ntr ,  replace=FALSE )  # CV groups

# Initialize variables for CV predictions
yhat.tr = matrix(0,ntr,10)
yhat.te = matrix(0,nte,10)

# CV loop
for(k in 1:Kf) {
  indk = cvgroup == k
  
  # Basic OLS
  K.train = trainL[!indk,]
  K.test = trainL[indk,]
  lsk = lm(eq.base, data = K.train)
  yhat.tr[indk,1] = predict(lsk, K.test)
  yhat.te[,1] = yhat.te[,1] + predict(lsk, testL)/Kf
  
  # Flexible OLS
  lsk = lm(eq.int, data = K.train)
  yhat.tr[indk,2] = predict(lsk, K.test)
  yhat.te[,2] = yhat.te[,2] + predict(lsk, testL)/Kf
  
  # Lasso
  K.xtr = xtr[!indk,]
  K.ytr = ytr[!indk]
  K.xte = xtr[indk,]
  K.yte = ytr[indk]
  
  lassok = cv.glmnet(K.xtr,K.ytr)
  yhat.tr[indk,3] = predict(lassok, newx = K.xte, s = "lambda.min")
  yhat.te[,3] = yhat.te[,3] + predict(lassok, newx = xte, s = "lambda.min")/Kf
  
  # Ridge
  ridgek = cv.glmnet(K.xtr,K.ytr, alpha = 0)
  yhat.tr[indk,4] = predict(ridgek, newx = K.xte, s = "lambda.min")
  yhat.te[,4] = yhat.te[,4] + predict(ridgek, newx = xte, s = "lambda.min")/Kf
  
  # Random Forest
  K.trainRF = trainRF[!indk,]
  K.testRF = trainRF[indk,]
  rfk = randomForest(net_tfa~. , data = K.trainRF)
  yhat.tr[indk,5] = predict(rfk, K.testRF)
  yhat.te[,5] = yhat.te[,5] + predict(rfk, testRF)/Kf
  
  # Boosted tree 3
  k.xgb_train = xgb.DMatrix(data = trainXG[!indk,-1], 
                            label = trainXG[!indk,1])
  k.xgb_test = xgb.DMatrix(data = trainXG[indk,-1], 
                           label = trainXG[indk,1])
  
  btk = xgb.cv(data = k.xgb_train,
               nrounds = 1000, verbose = 0, eta = .1, max_depth = 3, nfold = 5)
  best.iter = which.min(as.matrix(btk$evaluation_log[,4]))
  btk = xgboost(data = k.xgb_train,
                nrounds = 1000, verbose = 0, eta = .1, max_depth = 3)
  yhat.tr[indk,6] = predict(btk, newdata = k.xgb_test, iterationrange = c(1,(best.iter+1)))
  yhat.te[,6] = yhat.te[,6] + predict(btk, newdata = xgb_test, iterationrange = c(1,(best.iter+1)))/Kf
  
  # Boosted tree 5
  btk = xgb.cv(data = k.xgb_train,
               nrounds = 1000, verbose = 0, eta = .1, max_depth = 5, nfold = 5)
  best.iter = which.min(as.matrix(btk$evaluation_log[,4]))
  btk = xgboost(data = k.xgb_train,
                nrounds = 1000, verbose = 0, eta = .1, max_depth = 5)
  yhat.tr[indk,7] = predict(btk, newdata = k.xgb_test, iterationrange = c(1,(best.iter+1)))
  yhat.te[,7] = yhat.te[,7] + predict(btk, newdata = xgb_test, iterationrange = c(1,(best.iter+1)))/Kf
  
  # DNN 50/50, .5
  K.trainNN.X = trainNN.X[!indk,]
  K.trainNN.y = trainNN.y[!indk]
  K.testNN.X = trainNN.X[indk,]
  K.testNN.y = trainNN.y[indk]
  
  NNmodel <- keras_model_sequential()
  NNmodel %>% layer_dense(units = 50, activation = 'relu', input_shape = c(p)) %>% 
    layer_dropout(rate = .5) %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dropout(rate = .5) %>%
    layer_dense(units = 1)
  
  NNmodel %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.NNmodel <- NNmodel %>% fit(
    K.trainNN.X, K.trainNN.y, 
    epochs = 200, batch_size = 200,
    validation_split = .2, verbose = 0
  )
  
  yhat.tr[indk,8] = predict(NNmodel, K.testNN.X)
  yhat.te[,8] = yhat.te[,8] + predict(NNmodel, testNN.X)/Kf
  
  # DNN 50/50/50/50, .5
  NNmodel <- keras_model_sequential()
  NNmodel %>% layer_dense(units = 50, activation = 'relu', input_shape = c(p)) %>% 
    layer_dropout(rate = .5) %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dropout(rate = .5) %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dropout(rate = .5) %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dropout(rate = .5) %>%
    layer_dense(units = 1)
  
  NNmodel %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  fit.NNmodel <- NNmodel %>% fit(
    K.trainNN.X, K.trainNN.y, 
    epochs = 200, batch_size = 200,
    validation_split = .2, verbose = 0
  )
  
  yhat.tr[indk,9] = predict(NNmodel, K.testNN.X)
  yhat.te[,9] = yhat.te[,9] + predict(NNmodel, testNN.X)/Kf
  
  # DNN 50/50/50/50, early stopping
  NNmodel <- keras_model_sequential()
  NNmodel %>% layer_dense(units = 50, activation = 'relu', input_shape = c(p)) %>% 
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 1)
  
  NNmodel %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop())
  
  early.stop <- callback_early_stopping(monitor = "val_loss", patience = 100, 
                                        restore_best_weights = TRUE)
  
  fit.NNmodel <- NNmodel %>% fit(
    K.trainNN.X, K.trainNN.y, 
    epochs = 200, batch_size = 200,
    validation_split = .2, verbose = 0,
    callbacks = list(early.stop)
  )
  
  yhat.tr[indk,10] = predict(NNmodel, K.testNN.X)
  yhat.te[,10] = yhat.te[,10] + predict(NNmodel, testNN.X)/Kf
  
  
}

# Estimate ensemble weights via simple regression
w1 = lm(ytr~yhat.tr-1)

# Estimate ensemble weights constrained to be positive and sum to one
Vmat = crossprod(yhat.tr)
dvec = -t(yhat.tr)%*%ytr
A = as.matrix(rep(1,10))
u = rep(1,10)
wC = LowRankQP(Vmat,dvec,t(A),1,u,method = "LU")

##################################
# Compile results

CV.MSE = c(colMeans((ytr-yhat.tr)^2),mean((ytr-yhat.tr%*%w1$coefficients)^2),
           mean((ytr-yhat.tr%*%wC$alpha)^2))
Test.MSE = c(colMeans((yte-yhat.te)^2),mean((yte-yhat.te%*%w1$coefficients)^2),
             mean((yte-yhat.te%*%wC$alpha)^2))
CV.R2 = 1-CV.MSE/mean((ytr-mean(ytr))^2)
Test.R2 = 1-Test.MSE/mean((yte-mean(ytr))^2)

table <- matrix(0, 12, 3)
table[,1]   <- CV.R2
table[,2]   <- Test.R2
table[,3]   <- c(wC$alpha,0,0)
colnames(table)<- c("CV R2","Test R2","Weight")
rownames(table)<- c("OLS - basic", "OLS - flexible", "Lasso (CV)",
                    "Ridge (CV)", "Random forest", "Boosted trees - depth 3",
                    "Boosted trees - depth 5", "DNN - 50/50, dropout",
                    "DNN - 50/50/50/50, dropout", "DNN - 50/50/50/50, early stopping",
                    "Stacking","Stacking - constrained")
tab<- xtable(table, digits =c(0,3,3,3))
tab




