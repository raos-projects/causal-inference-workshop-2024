###############################################################################
## Power calculations in RD designs
## Author: Gonzalo Vazquez-Bare
###############################################################################

rm(list = ls())

#install.packages("rdrobust")
#install.packages("rddensity")
#install.packages("rdlocrand")
#install.packages("rdmulti")
#install.packages("rdpower")
library(rdrobust)
library(rddensity)
library(rdlocrand)
library(rdmulti)
library(rdpower)

#################################################################
# Setup
#################################################################

data <- read.csv('../Datasets/senate.csv')
Y <- data$demvoteshfor2
R <- data$demmv
names(data)

# Outcome and running variables

Z <- data[c('demvoteshfor2','demmv')]

# Covariates

covs <- data[c('population','dopen','dmidterm')]

# Cluster var

clustvar <- data$state


#################################################################
# RDPOWER
#################################################################

# rdpower against tau = 5

aux <- rdpower(data=Z,tau=5)

# rdpower with covariates

aux <- rdpower(data=Z,tau=5,covs=covs)

# rdpower with plot

aux <- rdpower(data=Z,tau=5,plot=TRUE)

# rdpower with rdrobust options

aux <- rdpower(data=Z,tau=5,h=c(16,18),b=c(18,20))
aux <- rdpower(data=Z,tau=5,kernel='uniform',cluster=data$state,vce='hc0')
aux <- rdpower(data=Z,tau=5,bwselect='certwo',vce='hc3',scaleregul=0,rho=1)

# rdpower with conventional inference

aux <- rdpower(data=Z,tau=5,all=TRUE)

# rdpower with user-specified bias and variance

rdr <- rdrobust(Y,R)

samph <- rdr$bws[1]

sampsi.l <- rdr$N_h_l
sampsi.r <- rdr$N_h_r

bias.l <- rdr$bias[1]/(rdr$bws[1]^2)
bias.r <- rdr$bias[2]/(rdr$bws[2]^2)

VL <- rdr$V_rb_l
VR <- rdr$V_rb_r

N <- sum(rdr$N)

Vl.rb <- N*rdr$bws[1]*VL[1,1]
Vr.rb <- N*rdr$bws[1]*VR[1,1]

aux <- rdpower(data=Z,tau=5,bias=c(bias.l,bias.r),variance=c(Vl.rb,Vr.rb),samph=samph,sampsi=c(sampsi.l,sampsi.r))

# rdpower manually increasing variance by 20%

rdr <- rdrobust(Y,R)

VL <- rdr$V_rb_l
VR <- rdr$V_rb_r

N = sum(rdr$N)

Vl.rb = N*rdr$bws[1]*VL[1,1]*1.2
Vr.rb = N*rdr$bws[1]*VR[1,1]*1.2

aux <- rdpower(data=Z,tau=5,variance=c(Vl.rb,Vr.rb))

# rdpower without data

aux1 <- rdpower(data=Z,tau=5)

aux <- rdpower(tau=5,
               nsamples=c(aux1$N.l,aux1$Nh.l,aux1$N.r,aux1$Nh.r),
               bias=c(aux1$bias.l,aux1$bias.r),
               variance=c(aux1$Vl.rb,aux1$Vr.rb),
               sampsi=c(aux1$sampsi.l,aux1$sampsi.r),
               samph=c(aux1$samph.l,aux1$samph.r))

# comparing exp-post power across specifications

aux1 <- rdpower(data=Z,tau=5,p=1,h=20,plot=TRUE)
aux2 <- rdpower(data=Z,tau=5,p=2,h=20,plot=TRUE)
aux3 <- rdpower(data=Z,tau=5,p=1,plot=TRUE)
aux4 <- rdpower(data=Z,tau=5,p=2,plot=TRUE)

# rdpower with clustering

aux <- rdpower(data=Z,tau=5,cluster=clustvar)

#################################################################
## RDSAMPSI
#################################################################

# rdsampsi with tau = 5

aux <- rdsampsi(data=Z,tau=5)

# rdsampsi setting bandwidth and nratio with plot

aux <- rdsampsi(data=Z,tau=5,beta=.9,samph=c(18,19),nratio=.5,plot=TRUE)

# rdsampsi with conventional inference

aux <- rdsampsi(data=Z,tau=5,all=TRUE)

# rdsampsi vs rdpower

aux1 <- rdsampsi(data=Z,tau=5)
aux2 <- rdpower(data=Z,tau=5,sampsi=c(aux1$sampsi.h.l,aux1$sampsi.h.r))

# rdsampsi without data

aux1 <- rdsampsi(data=Z,tau=5)
aux2 <- rdsampsi(tau = 5,
                 nsamples = c(aux1$N.l,aux1$Nh.l,aux1$N.r,aux1$Nh.r),
                 bias = c(aux1$bias.l,aux1$bias.r),
                 variance = c(aux1$var.l,aux1$var.r),
                 samph = c(aux1$samph.l,aux1$samph.r),
                 init.cond = aux1$init.cond)

# comparing sample sizes across designs

aux1 <- rdsampsi(data=Z,tau=5,p=0,h=20,plot=TRUE)
aux2 <- rdsampsi(data=Z,tau=5,p=1,h=20,plot=TRUE)
aux3 <- rdsampsi(data=Z,tau=5,p=0,plot=TRUE)
aux4 <- rdsampsi(data=Z,tau=5,p=1,plot=TRUE)

# rdsampsi with clustering

aux <- rdsampsi(data=Z,tau=5,cluster=clustvar)

#################################################################
## RDMDE
#################################################################

# rdmde with default options

aux <- rdmde(data=Z)

# rdmde for a power of 0.75

aux <- rdmde(data=Z,beta=0.75)

# rdmde manually setting the bandwidths

aux <- rdmde(data=Z,samph=c(12,13))

# rdmde manually setting the sample size inside the bandwidths

aux <- rdmde(data=Z,sampsi=c(350,320))

# rdmde without data

rdr <- rdrobust(Y,R)

bias.l <- rdr$bias[1]/(rdr$bws[1]^2)
bias.r <- rdr$bias[2]/(rdr$bws[2]^2)

VL <- rdr$V_rb_l
VR <- rdr$V_rb_r

N <- sum(rdr$N)

Vl.rb <- N*rdr$bws[1]*VL[1,1]
Vr.rb <- Vl.rb*1.1

aux <- rdmde(nsamples=c(600,350,700,320),bias=c(bias.l,bias.r),var=c(Vl.rb,Vr.rb),samph=17,init.cond=4)

# compare rdmde and rdpower

aux1 <- rdmde(data=Z)
mde <- aux1$mde
aux2 <- rdpower(data=Z,tau=mde)

# compare rdmde and rdsampsi

aux1 <- rdmde(data=Z)
mde <- aux1$mde
nhl <- aux1$Nh.l
nhr <- aux1$Nh.r

aux2 <- rdsampsi(data=Z,tau=mde,nratio=nhr/(nhl+nhr))

# rdmde with clustering

aux <- rdmde(data=Z,cluster=clustvar)

