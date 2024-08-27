# Code for 401(k) example in Lecture notes 1 illustrating tree-based methods

######################### Libraries
library(randomForest)
library(xtable)

######################### Options
dpl = FALSE     # switch to save plots. Set to FALSE to just run code
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

##############################################################
# Random forest

# Restrict minimium node size to 60
rForest.60 = randomForest(net_tfa~. , data = train, nodesize = 60)

######################################### 
## Let's look at variable importance 


# Define function to compute permutation variable importance in training data
# for random forest. 
var.imp <- function(pred.model, newY, newX, B=500){
  
  # Compute baseline fit measure
  e0 = sum((newY - predict(pred.model, newX))^2)
  n = nrow(newX)
  p = ncol(newX)
  
  eB = matrix(0,B,p)
  for(b in 1:B) {
    for(j in 1:p) {
      permb = sample(1:n)
      newXb = newX
      newXb[,j] = newX[permb,j]
      eB[b,j] = sum((newY - predict(pred.model, newXb))^2)
    }
  }
  
  eB = eB/e0
  VI = colMeans(eB)
  
  output = list(e0,eB,VI)
  names(output) = c("e0","eB","VI")
  
  return(output)
}

# Variable importance in random forest
VI.rf = var.imp(rForest.60, newY = test[,1], newX = test[,-1], B = 100)
colnames(VI.rf$eB) = colnames(test[,-1])

# Table of variable importance
VI.table <- matrix(0, ncol(VI.rf$eB), 1)
VI.table[,1] = VI.rf$VI[sort.list(VI.rf$VI, decreasing = TRUE)]
colnames(VI.table) = c("VI")
rownames(VI.table) = colnames(test[,-1])[sort.list(VI.rf$VI, decreasing = TRUE)]
VI.tab<- xtable(VI.table, digits =c(0,3))
VI.tab

# Plot VI distribution across permutations
if(dpl) filenm = paste(plotdir,'/VIBox','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=800,)
boxplot(VI.rf$eB[,sort.list(VI.rf$VI, decreasing = TRUE)])
if(dpl) dev.off()


######################################### 
## Let's look at partial dependence for a couple of variables 


# Define function to compute permutation variable importance in training data
# for random forest. 
partial.dep <- function(pred.model, eval.points, var.ind, dataX){
  
  n = nrow(dataX)
  s = length(eval.points)
  
  h = rep(0, s)
  for(b in 1:s) {
    newXb = dataX
    newXb[,var.ind] = rep(eval.points[b], n)
    h[b] = mean(predict(pred.model, newXb))
  }
  
  
  return(h)
}

# Partial dependence in random forest
# Income
inc.vals = seq(0,250000,10000)
h.inc = partial.dep(rForest.60, eval.points = inc.vals, 
                    var.ind = which(colnames(data401k) == "inc") , dataX = data401k)

# Plot partial dependence for income
if(dpl) filenm = paste(plotdir,'/PDInc','.png',sep='') 
if(dpl) png(file=filenm,height=640,width=800,)
plot(inc.vals,h.inc,xlab = "Income", ylab = "Average Predicted Assets", 
     type = "l", col = "blue")
if(dpl) dev.off()

# 401k eligibility
elig.vals = c(0,1)
h.elig = partial.dep(rForest.60, eval.points = elig.vals, 
                     var.ind = which(colnames(data401k) == "e401") , dataX = data401k)
cat("Average Structural Function for e401 == 0:",h.elig[1],"\n")
cat("Average Structural Function for e401 == 1:",h.elig[2],"\n")

