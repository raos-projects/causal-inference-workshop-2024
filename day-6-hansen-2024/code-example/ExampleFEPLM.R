# PLM Abortion Example

######################### Libraries
library(glmnet)
library(sandwich)
library(fastDummies)

######################### Options
dpl = TRUE      # switch to save plots. Set to FALSE to just run code
codedir = "~/GitHub/NW2022_1Day/R Code" # Directory where Hansen keeps code
plotdir = "~/GitHub/NW2022_1Day/Slides/figures" # Directory where Hansen keeps figures for these slides

########################################################################
########### Abortion Example: Inference

### Load data
setwd(codedir) 
data.DL = read.table("../Data/levitt_ex.dat", header = TRUE, 
                     sep = "\t")  # Read in abortion data
head(data.DL)  # Check to see if things look right

### Clean out some observations
data.DL = data.DL[data.DL[,1] != 9,] # drop DC observations
data.DL = data.DL[data.DL[,1] != 2,] # drop Alaska observations
data.DL = data.DL[data.DL[,1] != 12,] # drop Hawaii observations

## Restrict to years in original Donohue and Levitt paper
data.DL = data.DL[data.DL[,2] >= 85 & data.DL[,2] <= 97,]

## We're going to use a trend later, and let's have it start at 1
data.DL[,2] = data.DL[,2] - 84

## Treat state and year as factors
data.DL$statenum = as.factor(data.DL$statenum)
data.DL$year = as.factor(data.DL$year)

# state and year dummies
DS = as.matrix(dummy_cols(data.DL$statenum, remove_selected_columns = TRUE))
DY = as.matrix(dummy_cols(data.DL$year, remove_selected_columns = TRUE))
DYS = model.matrix(lpc_viol ~ statenum+year, data = data.DL)

# scale some of the variables
data.DL$xxincome = data.DL$xxincome/100
data.DL$xxpover = data.DL$xxpover/100
data.DL$xxafdc15 = data.DL$xxafdc15/10000
data.DL$xxbeer = data.DL$xxbeer/100

############################################################
## See if we can get close to original DL results
eq.viol = lpc_viol ~ efaviol+xxprison+xxpolice+xxunemp+xxincome+xxpover+
  xxafdc15+xxgunlaw+xxbeer+statenum+year
lm.viol = lm(eq.viol, data = data.DL)
vc.viol = vcovCL(lm.viol, cluster = data.DL$statenum)
cat ("FE Viol:",lm.viol$coefficients[2],"\n") 
cat ("s.e. FE Viol:",sqrt(vc.viol[2,2]),"\n") 

eq.prop = lpc_prop ~ efaprop+xxprison+xxpolice+xxunemp+xxincome+xxpover+
  xxafdc15+xxgunlaw+xxbeer+statenum+year
lm.prop = lm(eq.prop, data = data.DL)
vc.prop = vcovCL(lm.prop, cluster = data.DL$statenum)
cat ("FE Prop:",lm.prop$coefficients[2],"\n") 
cat ("s.e. FE Prop:",sqrt(vc.prop[2,2]),"\n") 

eq.murd = lpc_murd ~ efamurd+xxprison+xxpolice+xxunemp+xxincome+xxpover+
  xxafdc15+xxgunlaw+xxbeer+statenum+year
lm.murd = lm(eq.murd, data = data.DL)
vc.murd = vcovCL(lm.murd, cluster = data.DL$statenum)
cat ("FE Murd:",lm.murd$coefficients[2],"\n") 
cat ("s.e. FE Murd:",sqrt(vc.murd[2,2]),"\n") 

##################################################################
## Initial conditions and within state means
var.names = c("efaviol","efaprop","efamurd","xxprison","xxpolice",
                 "xxunemp","xxincome","xxpover","xxafdc15","xxgunlaw","xxbeer")

# Initial conditions
x0 = as.matrix(data.DL[data.DL$year == 1,var.names])
X0 = DS%*%x0
colnames(X0) = c("efaviol0","efaprop0","efamurd0","xxprison0","xxpolice0",
              "xxunemp0","xxincome0","xxpover0","xxafdc150","xxgunlaw0","xxbeer0")
# State means
XB = DS%*%solve(t(DS)%*%DS,t(DS)%*%as.matrix(data.DL[,var.names]))
colnames(XB) = c("efaviolB","efapropB","efamurdB","xxprisonB","xxpoliceB",
              "xxunempB","xxincomeB","xxpoverB","xxafdc15B","xxgunlawB","xxbeerB")

# Combine to raw data
data.DL = cbind(data.DL,X0,XB)

####################################################################
## Make dictionary expansion

# go back to treating year as numeric for a smooth deterministic trend
data.DL$year = as.numeric(data.DL$year)
data.DL$year = data.DL$year/max(data.DL$year) # scale to unit interval
data.DL$year2 = data.DL$year^2
data.DL$year3 = data.DL$year^3

x.names = c("xxprison","xxpolice","xxunemp","xxincome","xxpover","xxafdc15","xxgunlaw","xxbeer")
x0.names = c("xxprison0","xxpolice0","xxunemp0","xxincome0","xxpover0","xxafdc150","xxgunlaw0","xxbeer0")
xB.names = c("xxprisonB","xxpoliceB","xxunempB","xxincomeB","xxpoverB","xxafdc15B","xxgunlawB","xxbeerB")

controls.viol = paste(paste(x.names, collapse="+"),
                      "+ (",paste(x.names, collapse="+"),
                      "):(year+year2+year3)",
                      "+ (efaviol0 + ",
                      paste(x0.names, collapse="+")," + efaviolB + ",
                      paste(xB.names, collapse="+"),"):(year+year2+year3)")
controls.prop = paste(paste(x.names, collapse="+"),
                      "+ (",paste(x.names, collapse="+"),
                      "):(year+year2+year3)",
                      "+ (efaprop0 + ",
                      paste(x0.names, collapse="+")," + efapropB + ",
                      paste(xB.names, collapse="+"),"):(year+year2+year3)")
controls.murd = paste(paste(x.names, collapse="+"),
                      "+ (",paste(x.names, collapse="+"),
                      "):(year+year2+year3)",
                      "+ (efamurd0 + ",
                      paste(x0.names, collapse="+")," + efamurdB + ",
                      paste(xB.names, collapse="+"),"):(year+year2+year3)")

X.viol = model.matrix(as.formula(paste("lpc_viol ~ ",controls.viol)),
                      data = data.DL)[,-1]

X.prop = model.matrix(as.formula(paste("lpc_prop ~ ",controls.prop)),
                      data = data.DL)[,-1]

X.murd = model.matrix(as.formula(paste("lpc_murd ~ ",controls.murd)),
                      data = data.DL)[,-1]

# Partial out state and year dummies
y.viol = data.DL$lpc_viol - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%as.matrix(data.DL$lpc_viol))
d.viol = data.DL$efaviol - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%as.matrix(data.DL$efaviol))
X.viol = X.viol - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%X.viol)

y.prop = data.DL$lpc_prop - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%as.matrix(data.DL$lpc_prop))
d.prop = data.DL$efaprop - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%as.matrix(data.DL$efaprop))
X.prop = X.prop - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%X.prop)

y.murd = data.DL$lpc_murd - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%as.matrix(data.DL$lpc_murd))
d.murd = data.DL$efamurd - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%as.matrix(data.DL$efamurd))
X.murd = X.murd - DYS%*%solve(t(DYS)%*%DYS,t(DYS)%*%X.murd)

############################################################################
## TWFE with flexible trends

lm.ft.viol = lm(as.formula(paste("lpc_viol ~ efaviol + ",controls.viol,
                                 " + as.factor(statenum) + as.factor(year)")), 
                data = data.DL)
vc.ft.viol = vcovCL(lm.ft.viol, cluster = data.DL$statenum)
cat ("FE Viol (Flexible):",lm.ft.viol$coefficients[2],"\n") 
cat ("s.e. FE Viol (Flexible):",sqrt(vc.ft.viol[2,2]),"\n") 

lm.ft.prop = lm(as.formula(paste("lpc_prop ~ efaprop + ",controls.prop,
                                 " + as.factor(statenum) + as.factor(year)")), 
                data = data.DL)
vc.ft.prop = vcovCL(lm.ft.prop, cluster = data.DL$statenum)
cat ("FE Prop (Flexible):",lm.ft.prop$coefficients[2],"\n") 
cat ("s.e. FE Prop (Flexible):",sqrt(vc.ft.prop[2,2]),"\n") 

lm.ft.murd = lm(as.formula(paste("lpc_murd ~ efamurd + ",controls.murd,
                                 " + as.factor(statenum) + as.factor(year)")), 
                data = data.DL)
vc.ft.murd = vcovCL(lm.ft.murd, cluster = data.DL$statenum)
cat ("FE Murd (Flexible):",lm.ft.murd$coefficients[2],"\n") 
cat ("s.e. FE Murd (Flexible):",sqrt(vc.ft.murd[2,2]),"\n") 

cat ("sample size: ",nrow(data.DL),"; number of (noncollinear) variables: ",lm.ft.viol$rank)

############################################################################
## Double selection with clustered loadings

cl.lasso = function(X, y, cluster.dummies, init.resid) {

  n = nrow(X)
  p = ncol(X)
    
  Syx = X*init.resid # score
  DSyx = t(as.matrix(cluster.dummies))%*%as.matrix(Syx) # within cluster sum
  Ups0 = sqrt(colSums(DSyx^2))/n # sd of score
  
  lam = 2.2*sqrt(n)*qnorm(1-(.1/log(n))/(2*p))
  scaledX = X/Ups0
  las.fit = glmnet(scaledX , y, lambda = lam, standardize = FALSE) 
  
  bhat = las.fit$beta  
  
  return(bhat)
  
  # Could iterate a couple times to update loadings but doesn't do much in 
  # Hansen's experience 
}


# Double selection - Violent crime
las.y.viol = cl.lasso(X = X.viol, y = y.viol, cluster.dummies = DS, 
                      init.resid = lm(lpc_viol ~ 
                                        xxprison+xxpolice+xxunemp+xxincome+xxpover+
                                        xxafdc15+xxgunlaw+xxbeer+
                                        as.factor(statenum)+as.factor(year), data = data.DL)$residuals)

las.d.viol = cl.lasso(X = X.viol, y = d.viol, cluster.dummies = DS,
                      init.resid = lm(efaviol ~ 
                                        xxprison+xxpolice+xxunemp+xxincome+xxpover+
                                        xxafdc15+xxgunlaw+xxbeer+
                                        as.factor(statenum)+as.factor(year), data = data.DL)$residuals)
use.viol = union(which(las.y.viol != 0),which(las.d.viol != 0))

RHS.PDS.viol = cbind(d.viol,X.viol[,use.viol])
PDS.viol = lm(y.viol ~ RHS.PDS.viol-1)
vc.PDS.viol = vcovCL(PDS.viol, cluster = data.DL$statenum)
cat ("FE Viol (PDS):",PDS.viol$coefficients[1],"\n") 
cat ("s.e. FE Viol (PDS):",sqrt(vc.PDS.viol[1,1]),"\n") 


# Double selection - Property crime
las.y.prop = cl.lasso(X = X.prop, y = y.prop, cluster.dummies = DS, 
                      init.resid = lm(lpc_prop ~ 
                                        xxprison+xxpolice+xxunemp+xxincome+xxpover+
                                        xxafdc15+xxgunlaw+xxbeer+
                                        as.factor(statenum)+as.factor(year), data = data.DL)$residuals)

las.d.prop = cl.lasso(X = X.prop, y = d.prop, cluster.dummies = DS,
                      init.resid = lm(efaprop ~ 
                                        xxprison+xxpolice+xxunemp+xxincome+xxpover+
                                        xxafdc15+xxgunlaw+xxbeer+
                                        as.factor(statenum)+as.factor(year), data = data.DL)$residuals)
use.prop = union(which(las.y.prop != 0),which(las.d.prop != 0))

RHS.PDS.prop = cbind(d.prop,X.prop[,use.prop])
PDS.prop = lm(y.prop ~ RHS.PDS.prop-1)
vc.PDS.prop = vcovCL(PDS.prop, cluster = data.DL$statenum)
cat ("FE prop (PDS):",PDS.prop$coefficients[1],"\n") 
cat ("s.e. FE prop (PDS):",sqrt(vc.PDS.prop[1,1]),"\n") 


# Double selection - Murder
las.y.murd = cl.lasso(X = X.murd, y = y.murd, cluster.dummies = DS, 
                      init.resid = lm(lpc_murd ~ 
                                        xxprison+xxpolice+xxunemp+xxincome+xxpover+
                                        xxafdc15+xxgunlaw+xxbeer+
                                        as.factor(statenum)+as.factor(year), data = data.DL)$residuals)

las.d.murd = cl.lasso(X = X.murd, y = d.murd, cluster.dummies = DS,
                      init.resid = lm(efamurd ~ 
                                        xxprison+xxpolice+xxunemp+xxincome+xxpover+
                                        xxafdc15+xxgunlaw+xxbeer+
                                        as.factor(statenum)+as.factor(year), data = data.DL)$residuals)
use.murd = union(which(las.y.murd != 0),which(las.d.murd != 0))

RHS.PDS.murd = cbind(d.murd,X.murd[,use.murd])
PDS.murd = lm(y.murd ~ RHS.PDS.murd-1)
vc.PDS.murd = vcovCL(PDS.murd, cluster = data.DL$statenum)
cat ("FE murd (PDS):",PDS.murd$coefficients[1],"\n") 
cat ("s.e. FE murd (PDS):",sqrt(vc.PDS.murd[1,1]),"\n") 


############################################################################
## Partialing out ignoring clustering and using CV 

# Partialing out with lasso - Violent crime
cv.y.viol = cv.glmnet(X.viol, y.viol)
r.y.viol = y.viol - predict(cv.y.viol, newx = X.viol, s = "lambda.min")
cv.d.viol = cv.glmnet(X.viol, d.viol)
r.d.viol = d.viol - predict(cv.d.viol, newx = X.viol, s = "lambda.min")
  
CV.viol = lm(r.y.viol ~ r.d.viol-1)
vc.CV.viol = vcovCL(CV.viol, cluster = data.DL$statenum)
cat ("FE Viol (CV):",CV.viol$coefficients,"\n") 
cat ("s.e. FE Viol (CV):",sqrt(vc.CV.viol),"\n") 

# Partialing out with lasso - Property crime
cv.y.prop = cv.glmnet(X.prop, y.prop)
r.y.prop = y.prop - predict(cv.y.prop, newx = X.prop, s = "lambda.min")
cv.d.prop = cv.glmnet(X.prop, d.prop)
r.d.prop = d.prop - predict(cv.d.prop, newx = X.prop, s = "lambda.min")

CV.prop = lm(r.y.prop ~ r.d.prop-1)
vc.CV.prop = vcovCL(CV.prop, cluster = data.DL$statenum)
cat ("FE prop (CV):",CV.prop$coefficients,"\n") 
cat ("s.e. FE prop (CV):",sqrt(vc.CV.prop),"\n") 

# Partialing out with lasso - Murder
cv.y.murd = cv.glmnet(X.murd, y.murd)
r.y.murd = y.murd - predict(cv.y.murd, newx = X.murd, s = "lambda.min")
cv.d.murd = cv.glmnet(X.murd, d.murd)
r.d.murd = d.murd - predict(cv.d.murd, newx = X.murd, s = "lambda.min")

CV.murd = lm(r.y.murd ~ r.d.murd-1)
vc.CV.murd = vcovCL(CV.murd, cluster = data.DL$statenum)
cat ("FE murd (CV):",CV.murd$coefficients,"\n") 
cat ("s.e. FE murd (CV):",sqrt(vc.CV.murd),"\n") 

