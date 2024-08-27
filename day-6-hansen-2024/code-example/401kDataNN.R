# Code for 401(k) example illustrating NNs

######################### Libraries
library(keras)
library(xtable)
library(fastDummies)

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

#########################################
# Set up training and testing sample

#########################################
set.seed(8261977)
ntrain=8000    # number of training observations
tr = sample(1:nrow(data401k),ntrain)  # draw ntrain observations from original data
train = data401k[tr,]   # Training sample
train.X <- as.matrix(train[,-1])
train.y <- as.matrix(train[,1])
test = data401k[-tr,]   # Testing sample
test.X <- as.matrix(test[,-1])
test.y <- as.matrix(test[,1])
nTr = nrow(train)
nTe = nrow(test)
p = ncol(train.X)

############################################ 
# Attempt 1
########################## Let's specify an architecture
model.A <- keras_model_sequential()
model.A %>% layer_dense(units = 50, activation = 'relu', input_shape = c(p)) %>% 
  layer_dropout(rate = .5) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = .5) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = .5) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = .5) %>%
  layer_dense(units = 1)

# Specifies a model with p inputs (input_shape), 4 hidden layers with 50 neurons and 50% dropout,
# and a scalar output
summary(model.A)

# Get model ready for estimation
model.A %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop())

# Fit the model for 200 epochs using a batch size of 200, monitor a 20% validation sample
fit.model.A <- model.A %>% fit(
  train.X, train.y, 
  epochs = 200, batch_size = 200,
  validation_split = .2
)


# out-of-sample R^2
yhat.A = predict(model.A, test.X)
R2V.A = 1-sum((test.y-yhat.A)^2)/sum((test.y - mean(train.y))^2)
cat("Model A R2 on validation data:",R2V.A,"\n")


############################################ Attempt 2
########################## What happens without dropout
model.B <- keras_model_sequential()
model.B %>% layer_dense(units = 50, activation = 'relu', input_shape = c(p)) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 1)

summary(model.B)

# Get model ready for estimation
model.B %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop())

# Fit the model for 100 epochs using a batch size of 200, monitor a 20% validation sample
fit.model.B <- model.B %>% fit(
  train.X, train.y, 
  epochs = 200, batch_size = 200,
  validation_split = .2
)

if(dpl) filenm = paste(plotdir,'/NNMonitorB','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=1280,)
plot(fit.model.B)
if(dpl) dev.off()

# out-of-sample R^2
yhat.B = predict(model.B, test.X)
R2V.B = 1-sum((test.y-yhat.B)^2)/sum((test.y - mean(train.y))^2)
cat("Model B R2 on validation data:",R2V.B,"\n")

############################################ Attempt 3
########################## What happens without dropout but with early stopping
model.C <- keras_model_sequential()
model.C %>% layer_dense(units = 50, activation = 'relu', input_shape = c(p)) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 1)

summary(model.C)

# Get model ready for estimation
model.C %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop())

# Define early stopping rule
early.stop <- callback_early_stopping(monitor = "val_loss", patience = 50, 
                                      restore_best_weights = TRUE)
# Patience is number of epochs to check for early stopping. If 50 epochs pass without
# improvement, training stops. Goes back to coefficient estimates with best validation
# loss

# Fit the model for 100 epochs using a batch size of 200, monitor a 20% validation sample
fit.model.C <- model.C %>% fit(
  train.X, train.y, 
  epochs = 200, batch_size = 200,
  validation_split = .2, 
  callbacks = list(early.stop)
)

if(dpl) filenm = paste(plotdir,'/NNMonitorC','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=1280,)
plot(fit.model.C)
if(dpl) dev.off()

# out-of-sample R^2
yhat.C = predict(model.C, test.X)
R2V.C = 1-sum((test.y-yhat.C)^2)/sum((test.y - mean(train.y))^2)
cat("Model C R2 on validation data:",R2V.C,"\n")


############################################ Attempt 4
########################## What happens with explicit regularization and 
# early stopping
model.D <- keras_model_sequential()
model.D %>% layer_dense(units = 50, activation = 'relu', input_shape = c(p),
                        kernel_regularizer = regularizer_l1(l = .01)) %>% 
  layer_dense(units = 50, activation = 'relu',
              kernel_regularizer = regularizer_l1(l = .01)) %>%
  layer_dense(units = 50, activation = 'relu',
              kernel_regularizer = regularizer_l1(l = .01)) %>%
  layer_dense(units = 50, activation = 'relu',
              kernel_regularizer = regularizer_l1(l = .01)) %>%
  layer_dense(units = 1)

summary(model.D)

# Get model ready for estimation
model.D %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop())

# Fit the model for 200 epochs using a batch size of 200, monitor a 20% validation sample
fit.model.D <- model.D %>% fit(
  train.X, train.y, 
  epochs = 200, batch_size = 200,
  validation_split = .2,
  callbacks = list(early.stop)
)

if(dpl) filenm = paste(plotdir,'/NNMonitorD','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=1280,)
plot(fit.model.D)
if(dpl) dev.off()

# out-of-sample R^2
yhat.D = predict(model.D, test.X)
R2V.D = 1-sum((test.y-yhat.D)^2)/sum((test.y - mean(train.y))^2)
cat("Model D R2 on validation data:",R2V.D,"\n")


