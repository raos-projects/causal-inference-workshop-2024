###############################################################################
## RD estimation and inference
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

data <- read.csv("../Datasets/headstart.csv")
dim(data)
head(data)

c <- 59.1984

## Centered running variable

data$X <- data$povrate60 - c

## Outcome variable

names(data)[names(data)=="mort_age59_related_postHS"] <- "Y"

## Treatment variable

data$D <- as.numeric(data$X>=0)


###############################################################################
## Visual representation
###############################################################################

plot(data$X,data$Y)
abline(v=0,lty="dashed")

rdplot(data$Y,data$X)
rdplot(data$Y,data$X,p=1)
rdplot(data$Y,data$X,p=2)

###############################################################################
## Parametric estimation
###############################################################################

#install.packages("lmtest")
#install.packages("sandwich")
library(lmtest)
library(sandwich)

lr1 <- lm(Y~X*D,data=data)
coeftest(lr1,vcov = vcovHC(lr1,type="HC1"))

lr2 <- lm(Y~D*(X+I(X^2)),data=data)
coeftest(lr2,vcov = vcovHC(lr2,type="HC1"))

lr2bw <- lm(Y~D*(X+I(X^2)),data=data,subset=abs(data$X)<=9)
coeftest(lr2bw,vcov = vcovHC(lr2bw,type="HC1"))


###############################################################################
## Nonparametric estimation with robust bias-corrected inference
###############################################################################

## rdrobust

rdr <- rdrobust(data$Y,data$X)
summary(rdr)

rdr0 <- rdrobust(data$Y,data$X,p=0)
summary(rdr0)

rdrk <- rdrobust(data$Y,data$X,kernel="uniform")
summary(rdrk)

rdr_all <- rdrobust(data$Y,data$X,all=TRUE)
summary(rdr_all)

## Incorporate covariates

Z60 = cbind(data$census1960_pop,
            data$census1960_pctsch1417,
            data$census1960_pctsch534, 
            data$census1960_pctsch25plus,
            data$census1960_pop1417,
            data$census1960_pop534,
            data$census1960_pop25plus,
            data$census1960_pcturban,
            data$census1960_pctblack)
rdr_covs <- rdrobust(data$Y,data$X,covs=Z60)
summary(rdr_covs)

## Plot estimated TE

bwsel <- rdbwselect(data$Y,data$X)
names(bwsel)
h <- bwsel$bws[1]
h

rdplot(data$Y,data$X,h=h,p=1,kernel='triangular')
rdplot(data$Y,data$X,h=h,p=1,subset=abs(data$X)<=h,kernel='triangular')

## rdrobust by hand

# Separate regressions for treated and untreated

bwsel <- rdbwselect(data$Y,data$X)
h <- bwsel$bws[1]

data$Kweights <- (1-abs(data$X/h))*(abs(data$X)<=h) # Triangular kernel

lr_right <- lm(Y~X,data=data,subset=(data$D==1),weights=data$Kweights)
lr_left <- lm(Y~X,data=data,subset=(data$D==0),weights=data$Kweights)

cat("Estimated TE = ",lr_right$coefficients[1]-lr_left$coefficients[1])
summary(rdr)

# Regression with weights and interactions

lr_full <- lm(Y~X*D,data=data,weights=data$Kweights)
summary(lr_full)
summary(rdr)

## Alternative bandwidth choices

bws <- rdbwselect(data$Y,data$X,all=TRUE)
summary(bws)

rdr_two <- rdrobust(data$Y,data$X,bwselect="msetwo")
summary(rdr_two)

###############################################################################
## Density test
###############################################################################

hist(data$X)
abline(v=0)

rdden <- rddensity(data$X)
rdplotdensity(rdden,data$X)
summary(rdden)

###############################################################################
## Placebo test
###############################################################################

rdr_plac1 <- rdrobust(data$mort_age59_injury_postHS,data$X)
summary(rdr_plac1)

rdr_plac2 <- rdrobust(data$mort_age59_related_preHS,data$X)
summary(rdr_plac2)
