###############################################################################
## Local Randomization
## Author: Gonzalo Vazquez-Bare
###############################################################################

rm(list = ls())

#install.packages("rdrobust")
#install.packages("rddensity")
#install.packages("rdlocrand")
#install.packages("rdmulti")
library(rdrobust)
library(rddensity)
library(rdlocrand)
library(rdmulti)

###############################################################################
## Setup
###############################################################################

data <- read.csv("../Datasets/senate.csv")
dim(data)
head(data)

# Select predetermined covariates to be used for window selector

X  <-  cbind(data$presdemvoteshlag1,
             data$population/1000000,
             data$demvoteshlag1,
             data$demvoteshlag2,
             data$demwinprv1,
             data$demwinprv2,
             data$dopen,
             data$dmidterm,
             data$dpresdem)

# Assign names to the covariates

colnames(X) <-  c("DemPres Vote",
                  "Population",
                  "DemSen Vote t-1",
                  "DemSen Vote t-2",
                  "DemSen Win t-1",
                  "DemSen Win t-2",
                  "Open", "Midterm",
                  "DemPres")

# Running variable and outcome variable

R <- data$demmv
Y <- data$demvoteshfor2
D <- as.numeric(R>=0)

###############################################################################
## rdwinselect
###############################################################################

# Initial window of 0.5, window increments of 0.125

tmp <- rdwinselect(R,X,wmin=.5,wstep=.125)

# Large sample approximate inference and plot

tmp <- rdwinselect(R,X,wmin=.5,wstep=.125,approx=TRUE,nwin=50,quietly=TRUE,plot=TRUE)

# Increasing windows by adding at least 5 observations: symmetric windows

tmp <- rdwinselect(R,X,wmin=.5,wobs=5)
tmp <- rdwinselect(R,X,wmin=.5,wobs=5,dropmissing=TRUE)

# Increasing windows by adding at least 5 observations: asymmetric windows

tmp <- rdwinselect(R,X,wmin=.5,wobs=5,dropmissing=TRUE,wasymmetric=TRUE)

# Initial window with at least 10 observations

tmp <- rdwinselect(R,X,obsmin=10,wobs=5)

###############################################################################
## rdrandinf
###############################################################################

w <- 0.75

# Randomization inference using recommended window

tmp <- rdrandinf(Y,R,wl=-w,wr=w)


# Randomization inference using recommended window, all statistics

tmp <- rdrandinf(Y,R,wl=-w,wr=w,statistic='all')

# Linear parametric adjustment

tmp <- rdrandinf(Y,R,wl=-w,wr=w,p=1)

reg_below <- lm(Y[abs(R)<=w & R<0]~R[abs(R)<=w & R<0])
reg_above <- lm(Y[abs(R)<=w & R>=0]~R[abs(R)<=w & R>=0])

r_below <- data.frame(R = seq(-w,0,by=0.1))
r_above <- data.frame(R = seq(0,w,by=0.1))

pred_below <- predict(reg_below,r_below)
pred_above <- predict(reg_above,r_above)

plot(R[abs(R)<=w],Y[abs(R)<=w])
lines(r_above[,1],pred_above)
lines(r_below[,1],pred_below)
segments(-w,mean(Y[abs(R)<=w & R<0],na.rm=TRUE),0,mean(Y[abs(R)<=w & R<0],na.rm=TRUE),lty="dashed")
segments(0,mean(Y[abs(R)<=w & R>=0],na.rm=TRUE),w,mean(Y[abs(R)<=w & R>=0],na.rm=TRUE),lty="dashed")
abline(v=0,lty="dashed")

# Change null hypothesis

tmp <- rdrandinf(Y,R,wl=-w,wr=w,nulltau=9)

# Permutation-based CI

tmp <- rdrandinf(Y,R,wl=-w,wr=w,ci=.05)
