# Code for 401(k) example in Lecture notes 1 illustrating
# model assessment via validation

######################### Libraries
library(ggplot2)
library(kknn)
library(xtable)

######################### Options
dpl = FALSE      # switch to save plots. Set to FALSE to just run code
codedir = "~/GitHub/NW2022_1Day/R Code" # Directory where Hansen keeps code
plotdir = "~/GitHub/NW2022_1Day/Slides/figures" # Directory where Hansen keeps figures for these slides

######################### Read data
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

########################## Estimation and testing sample
set.seed(8261977)
ntrain=8000    # number of training observations
tr = sample(1:nrow(data401k),ntrain)  # draw ntrain observations from original data
train = data401k[tr,]   # Training sample
test = data401k[-tr,]   # Testing sample

################################################################
########################## Scatterplot 
plot.NetTFA = ggplot(train, aes(x=inc, y=net_tfa)) + geom_point() + xlab("Income") + ylab("Net Financial Assets") # Basic scatterplot
if(dpl) ggsave(paste(plotdir,'/ScPlNetTFA.png',sep=''), width = 10, height = 10)  # Save plot
if(!dpl) plot.NetTFA # plot picture if not saving

########################## Linear model in income
plot.LMNetTFA = ggplot(train, aes(x=inc, y=net_tfa)) + 
  geom_point() + xlab("Income") + ylab("Net Financial Assets") +
  geom_smooth(method=lm, se=FALSE) # Scatterplot with linear model
if(dpl) ggsave(paste(plotdir,'/LMNetTFA.png',sep=''), width = 10, height = 10)  # Save plot
if(!dpl) plot.LMNetTFA # plot picture if not saving

lm.nettfa = lm(net_tfa~inc, data = train)  # Linear regression results
lm.yhat = predict(lm.nettfa, test) # out-of-sample prediction

lm.MSE = mean((test$net_tfa - lm.yhat)^2)  # out-of-sample MSE
lm.RMSE = sqrt(lm.MSE)                     # out-of-sample RMSE
lm.OOSR2 = 1-lm.MSE/mean((test$net_tfa-mean(train$net_tfa))^2) # out-of-sample R^2

########################## Vastly overfit KNN
tmp.train = train[c("net_tfa","inc")]   # Just keep the variables we want
tmp.test = test[c("net_tfa","inc")]     # Just keep the variables we want

ind = order(tmp.train[,"inc"]) #sorting test makes the plots below easier to make.
tmp.train = tmp.train[ind,]

knn.out = kknn(net_tfa~inc,tmp.train,tmp.train,k=1,kernel = "rectangular")  # 1 nearest neighbor fit
knn.plot = data.frame(tmp.train[,"net_tfa"],tmp.train[,"inc"],knn.out$fitted.values)
names(knn.plot) = c("net_tfa","inc","fits")

plot.KNNNetTFA = plot.NetTFA + geom_line(data = knn.plot, aes(x = inc, y = fits), color = "blue")
if(dpl) ggsave(paste(plotdir,'/KNNNetTFA.png',sep=''), width = 10, height = 10)  # Save plot
if(!dpl) plot.KNNNetTFA # plot picture if not saving

knnR2 = 1-sum((knn.plot$net_tfa-knn.plot$fits)^2)/sum((knn.plot$net_tfa-mean(knn.plot$net_tfa))^2)  # In-sample R^2

knn.oos = kknn(net_tfa~inc,tmp.train,tmp.test,k=1,kernel = "rectangular")  # 1 nearest neighbor fit getting out-of-sample predictions
knn.yhat = knn.oos$fitted.values  # out-of-sample fits
knn.MSE = mean((tmp.test$net_tfa - knn.yhat)^2)  # out-of-sample MSE
knn.RMSE = sqrt(knn.MSE)                     # out-of-sample RMSE
knn.OOSR2 = 1-knn.MSE/mean((tmp.test$net_tfa-mean(train$net_tfa))^2) # out-of-sample R^2


########################### Sensible polynomial
plot.PolyNetTFA = ggplot(train, aes(x=inc, y=net_tfa)) + 
  geom_point() + xlab("Income") + ylab("Net Financial Assets") +
  geom_smooth(method=lm, formula = y ~ poly(x,3,raw = TRUE), se=FALSE) # Scatterplot with polynomial fit
if(dpl) ggsave(paste(plotdir,'/PolyNetTFA.png',sep=''), width = 10, height = 10)  # Save plot
if(!dpl) plot.PolyNetTFA # plot picture if not saving

poly.nettfa = lm(net_tfa ~ poly(inc,3,raw = TRUE), data = train)  # polynomial regression results for later

poly.yhat = predict(poly.nettfa, test) # out-of-sample prediction

poly.MSE = mean((test$net_tfa - poly.yhat)^2)  # out-of-sample MSE
poly.RMSE = sqrt(poly.MSE)                     # out-of-sample RMSE
poly.OOSR2 = 1-poly.MSE/mean((test$net_tfa-mean(train$net_tfa))^2) # out-of-sample R^2

########################### Results
in.RMSE = c(summary(lm.nettfa)$sigma,sqrt(mean((knn.plot$net_tfa-knn.plot$fits)^2)),
            summary(poly.nettfa)$sigma)  # in-sample RMSE
out.RMSE = c(lm.RMSE,knn.RMSE,poly.RMSE) # out-of-sample RMSE
in.R2 = c(summary(lm.nettfa)$r.squared,knnR2,
          summary(poly.nettfa)$r.squared)  # in-sample R2
out.R2 = c(lm.OOSR2,knn.OOSR2,poly.OOSR2) # out-of-sample R2

table<- matrix(0, 4, 3)          # Create table for displaying
table[1,]<- in.RMSE              # Note that "<-" is the same as "="
table[2,]<- out.RMSE
table[3,]<- in.R2  
table[4,]<- out.R2
colnames(table)<- c("Linear","Overfit","Polynomial")
rownames(table)<- c("RMSE (in-sample)", "RMSE (out-of-sample)", "R2 (in-sample)", "R2 (out-of-sample)")	
nd <- matrix(0,4,4)  # construct matrix telling number of digits to display
nd[3,2:4] = 3
nd[4,2:4] = 3
tab<- xtable(table, digits=nd)   # Puts it in format for copying to latex
tab

#############################################################################
############################# Brute force 5-fold CV
# Setup for brute force K-Fold CV
Kf = 5  # Number of folds
sampleframe <- rep(1:Kf, ceiling( ntrain/Kf ) ) 
cvgroup <- sample( sampleframe , size=ntrain ,  replace=FALSE )  # CV groups
nTe = nrow(test)

# Compute squared forecast loss across folds
SSE = matrix(0,5,3)
colnames(SSE) = c("Linear","Overfit","Polynomial")
rownames(SSE) = c("Fold1","Fold2","Fold3","Fold4","Fold5")

yhat.base = matrix(0,nTe,5)
yhat.knn = matrix(0,nTe,5)
yhat.poly = matrix(0,nTe,5)

for(k in 1:Kf) {
  indk = cvgroup == k
  K.train = data401k[!indk,]
  K.test = data401k[indk,]
  SSE[k,1] = sum((K.test$net_tfa - 
                    predict(lm(net_tfa~inc, data = K.train), K.test))^2)
  SSE[k,2] = sum((K.test$net_tfa - 
                    kknn(net_tfa~inc,K.train,K.test,k=1,kernel = "rectangular")$fitted.values)^2)
  SSE[k,3] = sum((K.test$net_tfa - 
                    predict(lm(net_tfa~poly(inc,3,raw = TRUE), data = K.train), K.test))^2)
  
  yhat.base[,k] = predict(lm(net_tfa~inc, data = K.train), test)
  yhat.knn[,k] = kknn(net_tfa~inc,K.train,test,k=1,kernel = "rectangular")$fitted.values
  yhat.poly[,k] = predict(lm(net_tfa~poly(inc,3,raw = TRUE), data = K.train), test)
}

SSEtab<- xtable(SSE, digits=-4)   # Puts it in format for copying to latex
SSEtab

# Summarize results across folds with RMSE
RMSE = sqrt(colSums(SSE)/ntrain)
RMSE

## Performance on validation data
# Prediction rule one - average cross-validated fits
r2.CVave.base = 1-sum((test$net_tfa-rowMeans(yhat.base))^2)/
  sum((test$net_tfa-mean(train$net_tfa))^2)
r2.CVave.knn = 1-sum((test$net_tfa-rowMeans(yhat.knn))^2)/
  sum((test$net_tfa-mean(train$net_tfa))^2)
r2.CVave.poly = 1-sum((test$net_tfa-rowMeans(yhat.poly))^2)/
  sum((test$net_tfa-mean(train$net_tfa))^2)

# Prediction rule two - fit on entire (training) data set
# Already did this in the first code block

table <- matrix(0, 2, 3)
table[1,]   <- c(r2.CVave.base,r2.CVave.knn,r2.CVave.poly)
table[2,]   <- out.R2
colnames(table)<- c("Linear","KNN","Polynomial")
rownames(table)<- c("CV Average","Refit")
tab<- xtable(table, digits =c(0,3,3,3))
tab


