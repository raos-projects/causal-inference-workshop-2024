###############################################################################
## RD with mulitple cutoffs or scores
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
## Multiple cutoffs: setup
###############################################################################

data <- read.csv("../Datasets/simdata_multic.csv")
dim(data)
head(data)

table(data$c)

Y <- data$y
X <- data$x
C <- data$c

###############################################################################
## rdmc
###############################################################################

# Basic syntax

aux <- rdmc(Y,X,C)

# rdrobust pooled options

aux <- rdmc(Y,X,C,pooled_opt=paste('h=20','p=2',sep=','),verbose=TRUE)

# Cutoff-specific bandwidths

aux <- rdmc(Y,X,C,h=c(11,10))

# Different bandwidth selector at each cutoff

aux <- rdmc(Y,X,C,bwselect=c('msetwo','certwo'))

# Add plot

aux <- rdmc(Y,X,C,plot=TRUE)

###############################################################################
## rdmcplot
###############################################################################

# Basic syntax

aux <- rdmcplot(Y,X,C)

# Omit bins plot

aux <- rdmcplot(Y,X,C,nobins=TRUE)

# Plot TE

aux <- rdmcplot(Y,X,C,h=c(11,12),pvec=c(1,1))

###################################################################
# Multiple cumulative cutoffs: setup
###################################################################

data <- read.csv('../Datasets/simdata_cumul.csv')
Y <- data$y
X <- data$x
cvec <- c(data$c[1],data$c[2])

###################################################################
# rdms
###################################################################

# Basic syntax

aux <- rdms(Y,X,cvec)

# Cutoff-specific bandwidths and kernels

aux <- rdms(Y,X,cvec,h=c(11,8),kernel=c('uniform','triangular'))

# Restricting the range

aux <- rdms(Y,X,cvec,range = matrix(c(0,33.5,65.5,100),ncol=2))

# Pooled estimate using rdmc

cutoff <- cvec[1]*(X<=49.5) + cvec[2]*(X>49.5)
aux <- rdmc(Y,X,cutoff)

# Plot using rdmcplot

aux <- rdmcplot(Y,X,cutoff)

###################################################################
# Bivariate score: setup
###################################################################

data <- read.csv('../Datasets/simdata_multis.csv')
head(data)

Y <- data$y
X1 <- data$x1
X2 <- data$x2
zvar <- data$t
cvec <- c(data$c1[1],data$c1[2],data$c1[3])
cvec2 <- c(data$c2[1],data$c2[2],data$c2[3])

dev.off()

plot(X1[zvar==0],X2[zvar==0],pch=4,col="blue",cex=0.75)
points(X1[zvar==1],X2[zvar==1],pch=2,col="red",cex=0.75)
segments(0,50,50,50,lwd=2)
segments(50,0,50,50,lwd=2)
text(60,60,"Control",cex=2)
text(25,25,"Treated",cex=2)
points(c(25,50,50),c(50,50,25),pch=16,cex=2)

###################################################################
# rdms
###################################################################

# Basic syntax

aux <- rdms(Y,X1,cvec,X2,zvar,cvec2)

# Cutoff-specific bandwidths

aux <- rdms(Y,X1,cvec,X2,zvar,cvec2,h=c(15,13,17))

# Pooled effect

xnorm <- apply(cbind(abs(50-X1),abs(50-X2)),1,min)*(2*zvar-1)
aux <- rdms(Y,X1,cvec,X2,zvar,cvec2,xnorm=xnorm)
