###############################################################################
## Fuzzy RD
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

data <- read.csv("../Datasets/uruguaycct.csv")
dim(data)
head(data)

table(data$T,data$D)

# Drop data with missing treatment

data <- data[!is.na(data$D),]

###############################################################################
## Effect on birth weight
###############################################################################

## First stage

rdplot(data$D,data$X)
fs <- rdrobust(data$D,data$X)
summary(fs)

## Intention-to-treat

rdplot(data$Y1,data$X)
itt <- rdrobust(data$Y1,data$X)
summary(itt)

## Fuzzy RD: average effect on compliers

# rdrobust command

frd <- rdrobust(data$Y1,data$X,fuzzy=data$D)
summary(frd)
h <- frd$bws[1,1]
b <- frd$bws[2,1]

# By hand

itt_bh <- rdrobust(data$Y1,data$X,h=h,b=b)

fs_bh <- rdrobust(data$D,data$X,h=h,b=b)

print(paste0("Tau_rd = itt / fs = ",round(itt_bh$Estimate[1]/fs_bh$Estimate[1],5)))

## Comparison with 2SLS for p=0

#install.packages("ivreg")
library(ivreg)

rdr_0 <- rdrobust(data$Y1,data$X,fuzzy=data$D,p=0,kernel="uniform",masspoints="off")
summary(rdr_0)

ivreg <- ivreg(Y1 ~ D | T, data=data,subset=abs(X)<=rdr_0$bws[1,1])
summary(ivreg)

###############################################################################
## Effect on employment
###############################################################################

## Intention-to-treat

rdplot(data$Y2,data$X)
rdr_emp <- rdrobust(data$Y2,data$X)
summary(rdr_emp)

## Fuzzy RD: average effect on compliers

rdr_emp_fuzzy <- rdrobust(data$Y2,data$X,fuzzy=data$D)
summary(rdr_emp_fuzzy)
