# Code for minimum wage example illustrating DML in DiD

rm(list = ls())

#-----------------------------------------------------------------------------
######################### Libraries
library(BMisc)
library(glmnet)
library(randomForest)
library(rpart)
library(xtable)

set.seed(772023)

codedir = "~/GitHub/NW2022_1Day/R Code" # Directory where Hansen keeps code

#-----------------------------------------------------------------------------
#### This snippet from analysis.R by Brantly Callaway (https://github.com/bcallaway11/did_chapter)
# load/process data
load("../Data/mw_data_ch.RData")  
data <- mw_data_ch
rm(mw_data_ch)
data <- subset(data, emp0A01_BS > 0)
data <- subset(data, annual_avg_pay > 0)
data <- data[complete.cases(data[,-c("state_mw", "fed_mw")]),]
data <- makeBalancedPanel(data, "countyreal", "year")
data$lemp <- log(data$emp0A01_BS)
data$pop <- as.numeric(data$pop)
data$lpop <- log(data$pop)
data$lavg_pay <- log(data$annual_avg_pay)
data$region <- 1*(data$censusdiv==1 | data$censusdiv==2) + 2*(data$censusdiv==3 | data$censusdiv==4) +
  3*(data$censusdiv==5 | data$censusdiv==6 | data$censusdiv==7) + 4*(data$censusdiv==8 | data$censusdiv==9)
data$region <- as.factor(data$region)
data$ever_treated <- 1*(data$G != 0)
data$id <- data$countyreal

# create subset dropping already and early-treated
# drops already and early-treated, similar to data used in CS
# Note that G denotes the first period when an observation is treated with
# G = 0 being observations not treated by end of sample period
data <- subset(data, (G==0) | (G>2001))

#-----------------------------------------------------------------------------
# New code from here 
#-----------------------------------------------------------------------------
# Create data for DML
# Going to focus analysis on observations that were first treated in 2004
# 
# Callaway controls linearly for lpop, lavg_pay, and region dummies
# As controls, will use 2001 outcomes, 2001 lpop, 2001 lavg_pay, 
# and region dummies
#
# As control group, we will use the not yet treated
# 
# We will pretend that there is no remaining spatial or temporal correlation
# after taking differences of outcomes and including controls

# Drop all the variables we won't use
data <- data[ , !(colnames(data) %in% c("countyreal","state_name","FIPS","emp0A01_BS","quarter",
                                        "censusdiv","pop","annual_avg_pay","state_mw","fed_mw",
                                        "ever_treated"))]

# Create treatment and control subsets for each year
treat1 <- subset(data, (G == 2004) & (year == 2001))
treat2 <- subset(data, (G == 2004) & (year == 2002))
treat3 <- subset(data, (G == 2004) & (year == 2003))
treat4 <- subset(data, (G == 2004) & (year == 2004))
treat5 <- subset(data, (G == 2004) & (year == 2005))
treat6 <- subset(data, (G == 2004) & (year == 2006))
treat7 <- subset(data, (G == 2004) & (year == 2007))

cont1 <- subset(data, (G == 0 | G > 2001) & (year == 2001))
cont2 <- subset(data, (G == 0 | G > 2002) & (year == 2002))
cont3 <- subset(data, (G == 0 | G > 2003) & (year == 2003))
cont4 <- subset(data, (G == 0 | G > 2004) & (year == 2004))
cont5 <- subset(data, (G == 0 | G > 2005) & (year == 2005))
cont6 <- subset(data, (G == 0 | G > 2006) & (year == 2006))
cont7 <- subset(data, (G == 0 | G > 2007) & (year == 2007))

# NOTE: THERE ARE NO CONTROL OBSERVATIONS IN REGION 1 IN 2007.  

# NOTE: ALL THE TREATMENT OBSERVATIONS ARE IN REGION 2.

# Only want first period for "control" variables
treat1 <- treat1[ , !(colnames(treat1) %in% c("year","G","region","treated"))]

cont1 <- cont1[ , !(colnames(cont1) %in% c("year","G","region","treated"))]

# 2003 will serve as pre period
treatB <- merge(treat3, treat1, by = "id", suffixes = c(".pre",".0"))
treatB <- treatB[ , !(colnames(treatB) %in% c("treated","lpop.pre","lavg_pay.pre","year","G"))]

contB <- merge(cont3, cont1, by = "id", suffixes = c(".pre",".0"))
contB <- contB[ , !(colnames(contB) %in% c("treated","lpop.pre","lavg_pay.pre","year","G"))]

# Create four treatment and four control data sets to use for estimating the 
# effect in 04, 05, 06, and 07
treat4 <- treat4[ , !(colnames(treat4) %in% c("lpop","lavg_pay","year","G","region"))]
treat5 <- treat5[ , !(colnames(treat5) %in% c("lpop","lavg_pay","year","G","region"))]
treat6 <- treat6[ , !(colnames(treat6) %in% c("lpop","lavg_pay","year","G","region"))]
treat7 <- treat7[ , !(colnames(treat7) %in% c("lpop","lavg_pay","year","G","region"))]

tdid04 <- merge(treat4, treatB, by = "id")
dy <- tdid04$lemp-tdid04$lemp.pre
tdid04$dy <- dy
tdid04 <- tdid04[ , !(colnames(tdid04) %in% c("id","lemp","lemp.pre"))]

tdid05 <- merge(treat5, treatB, by = "id")
dy <- tdid05$lemp-tdid05$lemp.pre
tdid05$dy <- dy
tdid05 <- tdid05[ , !(colnames(tdid05) %in% c("id","lemp","lemp.pre"))]

tdid06 <- merge(treat6, treatB, by = "id")
dy <- tdid06$lemp-tdid06$lemp.pre
tdid06$dy <- dy
tdid06 <- tdid06[ , !(colnames(tdid06) %in% c("id","lemp","lemp.pre"))]

tdid07 <- merge(treat7, treatB, by = "id")
dy <- tdid07$lemp-tdid07$lemp.pre
tdid07$dy <- dy
tdid07 <- tdid07[ , !(colnames(tdid07) %in% c("id","lemp","lemp.pre"))]

cont4 <- cont4[ , !(colnames(cont4) %in% c("lpop","lavg_pay","year","G","region"))]
cont5 <- cont5[ , !(colnames(cont5) %in% c("lpop","lavg_pay","year","G","region"))]
cont6 <- cont6[ , !(colnames(cont6) %in% c("lpop","lavg_pay","year","G","region"))]
cont7 <- cont7[ , !(colnames(cont7) %in% c("lpop","lavg_pay","year","G","region"))]

cdid04 <- merge(cont4, contB, by = "id")
dy <- cdid04$lemp-cdid04$lemp.pre
cdid04$dy <- dy
cdid04 <- cdid04[ , !(colnames(cdid04) %in% c("id","lemp","lemp.pre"))]

cdid05 <- merge(cont5, contB, by = "id")
dy <- cdid05$lemp-cdid05$lemp.pre
cdid05$dy <- dy
cdid05 <- cdid05[ , !(colnames(cdid05) %in% c("id","lemp","lemp.pre"))]

cdid06 <- merge(cont6, contB, by = "id")
dy <- cdid06$lemp-cdid06$lemp.pre
cdid06$dy <- dy
cdid06 <- cdid06[ , !(colnames(cdid06) %in% c("id","lemp","lemp.pre"))]

cdid07 <- merge(cont7, contB, by = "id")
dy <- cdid07$lemp-cdid07$lemp.pre
cdid07$dy <- dy
cdid07 <- cdid07[ , !(colnames(cdid07) %in% c("id","lemp","lemp.pre"))]

#-----------------------------------------------------------------------------
# Ready to start DML estimation here
att <- matrix(NA,4,10)
se.att <- matrix(NA,4,10)
RMSE.d <- matrix(NA,4,9)
RMSE.y <- matrix(NA,4,9)
trimmed <- matrix(NA,4,9)
for(ii in 1:4){
  
  tdata <- get(paste("tdid0",(3+ii),sep=""))  # Treatment data
  cdata <- get(paste("cdid0",(3+ii),sep=""))  # Control data
  usedata <- rbind(tdata,cdata)
  
  #-----------------------------------------------------------------------------
  # Cross-fit setup
  n <- nrow(usedata)
  Kf <- 5 # Number of folds
  sampleframe <- rep(1:Kf, ceiling(n/Kf))
  cfgroup <- sample(sampleframe, size=n, replace = FALSE) # Cross-fitting groups
  
  # Initialize variables for CV predictions
  # For ATT, need nuisance function E[Y|D=0,X], E[D|X], P(D = 1)
  # For conditional objects, will consider constant; linear index; 
  # region specific linear index; 
  # l1, l2 penalization with expansions; random forest; trees (shallow, deep, cv)
  yGd0x.fit <- matrix(NA,n,9)  
  dGx.fit <- matrix(NA,n,9)
  pd.fit <-matrix(NA,n,1)
  
  #-----------------------------------------------------------------------------
  # Cross-fit loop
  for(k in 1:Kf) {
    cat("year: ",ii+3,"; fold: ",k,"\n")
    indk <- cfgroup == k
    
    ktrain <- usedata[!indk,]
    ktest <- usedata[indk,]
    
    # Build some matrices for later
    ytrain <- as.matrix(usedata[!indk,"dy"])
    ytest <- as.matrix(usedata[indk,"dy"])
    dtrain <- as.matrix(usedata[!indk,"treated"])
    dtest <- as.matrix(usedata[indk,"treated"])
    
    # Expansion for lasso/ridge (region specific cubic polynomial)
    X.expand <- model.matrix( ~ region*(polym(lemp.0 , lpop.0 , lavg_pay.0 ,
                                              degree = 3, raw = TRUE)), 
                              data = usedata)
    
    xtrain <- as.matrix(X.expand[!indk,])
    xtest <- as.matrix(X.expand[indk,])
    
    #-----------------------------------------------------------------------------   
    # P(D = 1)
    pd.fit[indk,1] <- mean(ktrain$treated)
    
    #-----------------------------------------------------------------------------
    # E[D|X]
    
    # 1) Constant
    dGx.fit[indk,1] <- mean(ktrain$treated)
    
    # 2) Baseline controls
    glmXdk <- glm(treated ~ region + lemp.0 + lpop.0 + lavg_pay.0, 
                 family = "binomial", data = ktrain)
    dGx.fit[indk,2] <- predict(glmXdk, newdata = ktest, type = "response")
    
    # 3) Region specific linear index
    glmRXdk <- glm(treated ~ region * (lemp.0 + lpop.0 + lavg_pay.0), 
                 family = "binomial", data = ktrain)
    dGx.fit[indk,3] <- predict(glmRXdk, newdata = ktest, type = "response")
    
    # 4) Lasso - expansion - default CV tuning
    lassoXdk <- cv.glmnet(xtrain , dtrain , family = "binomial", type.measure = "mse")
    dGx.fit[indk,4] <- predict(lassoXdk, newx = xtest, type = "response" ,
                               s = "lambda.min")
    
    # 5) Ridge - expansion - default CV tuning
    ridgeXdk <- cv.glmnet(xtrain , dtrain , family = "binomial", 
                          type.measure = "mse", alpha = 0)
    dGx.fit[indk,5] <- predict(ridgeXdk, newx = xtest, type = "response" ,
                               s = "lambda.min")
    
    # 6) Random forest
    rfXdk <- randomForest(as.factor(treated) ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                          data = ktrain , mtry = 4, ntree = 1000)
    dGx.fit[indk,6] <- predict(rfXdk, ktest, type = "prob")[, 2]
    
    # 7) Tree (start big)
    btXdk <- rpart(treated ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                     data = ktrain, method = "anova",  
                     control=rpart.control(maxdepth = 15, cp = 0, xval = 5, minsplit = 10))  
    # xval is the number of cross-validation splits. E.g. xval = 5 is five fold CV
    dGx.fit[indk,7] <- predict(btXdk, ktest)
    
    # 8) Tree (small tree)
    stXdk <- rpart(treated ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                  data = ktrain, method = "anova", 
                  control=rpart.control(maxdepth = 3, cp = 0, xval = 0, minsplit = 10))  
    # xval is the number of cross-validation splits. E.g. xval = 5 is five fold CV
    dGx.fit[indk,8] <- predict(stXdk, ktest) 
    
    # 9) Tree (cv)
    bestcp <- btXdk$cptable[which.min(btXdk$cptable[,"xerror"]),"CP"]
    cvXdk <- prune(btXdk , cp=bestcp)
    dGx.fit[indk,9] <- predict(cvXdk, ktest)
    
    #-----------------------------------------------------------------------------
    # E[Y|D=0,X]
    
    # subset to D = 0
    ktrain0 = ktrain[ktrain$treated == 0, ]
    
    ytrain0 = ytrain[ktrain$treated == 0, ]
    xtrain0 = xtrain[ktrain$treated == 0, ]
    
    # 1) Constant
    yGd0x.fit[indk,1] <- mean(ktrain0$dy)
    
    # 2) Baseline controls
    lmXyk <- lm(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0, data = ktrain0)
    yGd0x.fit[indk,2] <- predict(lmXyk, newdata = ktest)
    
    # 3) Region specific linear index
    lmRXyk <- lm(treated ~ region * (lemp.0 + lpop.0 + lavg_pay.0), 
                   data = ktrain)
    yGd0x.fit[indk,3] <- predict(lmRXyk, newdata = ktest)
    
    # 4) Lasso - expansion - default CV tuning
    lassoXyk <- cv.glmnet(xtrain0 , ytrain0)
    yGd0x.fit[indk,4] <- predict(lassoXyk, newx = xtest , s = "lambda.min")
    
    # 5) Ridge - expansion - default CV tuning
    ridgeXyk <- cv.glmnet(xtrain0 , ytrain0  , alpha = 0)
    yGd0x.fit[indk,5] <- predict(ridgeXyk, newx = xtest, s = "lambda.min")
    
    # 6) Random forest
    rfXyk <- randomForest(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                          data = ktrain0 , mtry = 4, ntree = 1000)
    yGd0x.fit[indk,6] <- predict(rfXyk, ktest)
    
    # 7) Tree (start big)
    btXyk <- rpart(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                   data = ktrain0, method = "anova",  
                   control=rpart.control(maxdepth = 15, cp = 0, xval = 5, minsplit = 10))  
    yGd0x.fit[indk,7] <- predict(btXyk, ktest)
    
    # 8) Tree (small tree)
    stXyk <- rpart(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                   data = ktrain, method = "anova", 
                   control=rpart.control(maxdepth = 3, cp = 0, xval = 0, minsplit = 10))  
    yGd0x.fit[indk,8] <- predict(stXyk, ktest) 
    
    # 9) Tree (cv)
    bestcp <- btXyk$cptable[which.min(btXyk$cptable[,"xerror"]),"CP"]
    cvXyk <- prune(btXyk , cp=bestcp)
    yGd0x.fit[indk,9] <- predict(cvXyk, ktest)    
    
  }
  
  RMSE.d[ii, ] <- sqrt(colMeans((usedata$treated - dGx.fit)^2))
  RMSE.y[ii, ] <- sqrt(colMeans((usedata$dy[usedata$treated == 0] - 
                                   yGd0x.fit[usedata$treated == 0, ])^2))
  
  # trim propensity scores of 1 to .95
  for(r in 1:9) {
      trimmed[ii,r] = sum(dGx.fit[ , r] > .95)
      dGx.fit[dGx.fit[ ,r] > .95,r] <- .95
  }
  
  att.num <- c(colMeans(((usedata$treated - dGx.fit)/((pd.fit%*%matrix(1,1,9))*(1-dGx.fit)))*
                        (usedata$dy-yGd0x.fit)) ,
               mean(((usedata$treated - dGx.fit[ ,which.min(RMSE.d[ii, ])])
                     /(pd.fit*(1-dGx.fit[ ,which.min(RMSE.d[ii, ])])))*
                          (usedata$dy-yGd0x.fit[ ,which.min(RMSE.y[ii, ])])))
  att.den <- mean(usedata$treated/pd.fit)
  
  att[ii, ] <- att.num/att.den
  
  phihat <- cbind(((usedata$treated - dGx.fit)/((pd.fit%*%matrix(1,1,9))*(1-dGx.fit)))*
                         (usedata$dy-yGd0x.fit) ,
              ((usedata$treated - dGx.fit[ ,which.min(RMSE.d[ii, ])])
                    /(pd.fit*(1-dGx.fit[ ,which.min(RMSE.d[ii, ])])))*
                     (usedata$dy-yGd0x.fit[ ,which.min(RMSE.y[ii, ])]))/att.den
  se.att[ii, ] <- sqrt(colMeans((phihat^2))/n)  
  
  
}

#----------------------------------------------------------------------------
# Display results
table1y <- matrix(0, 9, 4)
table1y <- t(RMSE.y)
colnames(table1y)<- c("2004","2005","2006","2007")
rownames(table1y)<- c("No Controls", "Basic", "Expansion",
                     "Lasso (CV)", "Ridge (CV)", 
                     "Random Forest","Deep Tree", 
                     "Shallow Tree", "Tree (CV)")
tab1y <- xtable(table1y, digits =c(0,rep(4,4)))
tab1y

table1d <- matrix(0, 9, 4)
table1d <- t(RMSE.d)
colnames(table1d)<- c("2004","2005","2006","2007")
rownames(table1d)<- c("No Controls", "Basic", "Expansion",
                     "Lasso (CV)", "Ridge (CV)", 
                     "Random Forest","Deep Tree", 
                     "Shallow Tree", "Tree (CV)")
tab1d <- xtable(table1d, digits =c(0,rep(4,4)))
tab1d

table2 <- matrix(0, 20, 4)
table2[seq(1,20,2),]   <- t(att)
table2[seq(2,20,2),]   <- t(se.att)
colnames(table2)<- c("2004","2005","2006","2007")
rownames(table2)<- c("No Controls","s.e.","Basic","s.e.",
                     "Expansion","s.e.","Lasso (CV)","s.e.",
                     "Ridge (CV)","s.e.","Random Forest","s.e.",
                     "Deep Tree","s.e.","Shallow Tree","s.e.",
                     "Tree (CV)","s.e.","Best","s.e.")
tab2<- xtable(table2, digits =c(0,rep(3,4)))
tab2

trimmed

#-----------------------------------------------------------------------------
# Assess pre-trends

# Create treatment and control data sets to use for estimating the 
# effect in 02
treat2 <- treat2[ , !(colnames(treat2) %in% c("lpop","lavg_pay","year","G","region"))]
treat2$treated <- 1  # Code these observations as treated

tdid02 <- merge(treat2, treatB, by = "id")
dy <- tdid02$lemp-tdid02$lemp.pre
tdid02$dy <- dy
tdid02 <- tdid02[ , !(colnames(tdid02) %in% c("id","lemp","lemp.pre"))]

cont2 <- cont2[ , !(colnames(cont2) %in% c("lpop","lavg_pay","year","G","region"))]

cdid02 <- merge(cont2, contB, by = "id")
dy <- cdid02$lemp-cdid02$lemp.pre
cdid02$dy <- dy
cdid02 <- cdid02[ , !(colnames(cdid02) %in% c("id","lemp","lemp.pre"))]

#-----------------------------------------------------------------------------
# Ready to start DML estimation here
attP <- matrix(NA,1,10)
se.attP <- matrix(NA,1,10)
RMSE.dP <- matrix(NA,1,9)
RMSE.yP <- matrix(NA,1,9)
trimmedP <- matrix(NA,1,9)
for(ii in 1){
  
  tdata <- get(paste("tdid0",(3-ii),sep=""))  # Treatment data
  cdata <- get(paste("cdid0",(3-ii),sep=""))  # Control data
  usedata <- rbind(tdata,cdata)
  
  #-----------------------------------------------------------------------------
  # Cross-fit setup
  n <- nrow(usedata)
  Kf <- 5 # Number of folds
  sampleframe <- rep(1:Kf, ceiling(n/Kf))
  cfgroup <- sample(sampleframe, size=n, replace = FALSE) # Cross-fitting groups
  
  # Initialize variables for CV predictions
  # For ATT, need nuisance function E[Y|D=0,X], E[D|X], P(D = 1)
  # For conditional objects, will consider constant; linear index; 
  # region specific linear index; 
  # l1, l2 penalization with expansions; random forest; trees (shallow, deep, cv)
  yGd0x.fit <- matrix(NA,n,9)  
  dGx.fit <- matrix(NA,n,9)
  pd.fit <-matrix(NA,n,1)
  
  #-----------------------------------------------------------------------------
  # Cross-fit loop
  for(k in 1:Kf) {
    cat("year: ",ii+1,"; fold: ",k,"\n")
    indk <- cfgroup == k
    
    ktrain <- usedata[!indk,]
    ktest <- usedata[indk,]
    
    # Build some matrices for later
    ytrain <- as.matrix(usedata[!indk,"dy"])
    ytest <- as.matrix(usedata[indk,"dy"])
    dtrain <- as.matrix(usedata[!indk,"treated"])
    dtest <- as.matrix(usedata[indk,"treated"])
    
    # Expansion for lasso/ridge (region specific cubic polynomial)
    X.expand <- model.matrix( ~ region*(polym(lemp.0 , lpop.0 , lavg_pay.0 ,
                                              degree = 3, raw = TRUE)), 
                              data = usedata)
    
    xtrain <- as.matrix(X.expand[!indk,])
    xtest <- as.matrix(X.expand[indk,])
    
    #-----------------------------------------------------------------------------   
    # P(D = 1)
    pd.fit[indk,1] <- mean(ktrain$treated)
    
    #-----------------------------------------------------------------------------
    # E[D|X]
    
    # 1) Constant
    dGx.fit[indk,1] <- mean(ktrain$treated)
    
    # 2) Baseline controls
    glmXdk <- glm(treated ~ region + lemp.0 + lpop.0 + lavg_pay.0, 
                  family = "binomial", data = ktrain)
    dGx.fit[indk,2] <- predict(glmXdk, newdata = ktest, type = "response")
    
    # 3) Region specific linear index
    glmRXdk <- glm(treated ~ region * (lemp.0 + lpop.0 + lavg_pay.0), 
                   family = "binomial", data = ktrain)
    dGx.fit[indk,3] <- predict(glmRXdk, newdata = ktest, type = "response")
    
    # 4) Lasso - expansion - default CV tuning
    lassoXdk <- cv.glmnet(xtrain , dtrain , family = "binomial", type.measure = "mse")
    dGx.fit[indk,4] <- predict(lassoXdk, newx = xtest, type = "response" ,
                               s = "lambda.min")
    
    # 5) Ridge - expansion - default CV tuning
    ridgeXdk <- cv.glmnet(xtrain , dtrain , family = "binomial", 
                          type.measure = "mse", alpha = 0)
    dGx.fit[indk,5] <- predict(ridgeXdk, newx = xtest, type = "response" ,
                               s = "lambda.min")
    
    # 6) Random forest
    rfXdk <- randomForest(as.factor(treated) ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                          data = ktrain , mtry = 4, ntree = 1000)
    dGx.fit[indk,6] <- predict(rfXdk, ktest, type = "prob")[, 2]
    
    # 7) Tree (start big)
    btXdk <- rpart(treated ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                   data = ktrain, method = "anova",  
                   control=rpart.control(maxdepth = 15, cp = 0, xval = 5, minsplit = 10))  
    # xval is the number of cross-validation splits. E.g. xval = 5 is five fold CV
    dGx.fit[indk,7] <- predict(btXdk, ktest)
    
    # 8) Tree (small tree)
    stXdk <- rpart(treated ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                   data = ktrain, method = "anova", 
                   control=rpart.control(maxdepth = 3, cp = 0, xval = 0, minsplit = 10))  
    # xval is the number of cross-validation splits. E.g. xval = 5 is five fold CV
    dGx.fit[indk,8] <- predict(stXdk, ktest) 
    
    # 9) Tree (cv)
    bestcp <- btXdk$cptable[which.min(btXdk$cptable[,"xerror"]),"CP"]
    cvXdk <- prune(btXdk , cp=bestcp)
    dGx.fit[indk,9] <- predict(cvXdk, ktest)
    
    #-----------------------------------------------------------------------------
    # E[Y|D=0,X]
    
    # subset to D = 0
    ktrain0 = ktrain[ktrain$treated == 0, ]
    
    ytrain0 = ytrain[ktrain$treated == 0, ]
    xtrain0 = xtrain[ktrain$treated == 0, ]
    
    # 1) Constant
    yGd0x.fit[indk,1] <- mean(ktrain0$dy)
    
    # 2) Baseline controls
    lmXyk <- lm(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0, data = ktrain0)
    yGd0x.fit[indk,2] <- predict(lmXyk, newdata = ktest)
    
    # 3) Region specific linear index
    lmRXyk <- lm(treated ~ region * (lemp.0 + lpop.0 + lavg_pay.0), 
                 data = ktrain)
    yGd0x.fit[indk,3] <- predict(lmRXyk, newdata = ktest)
    
    # 4) Lasso - expansion - default CV tuning
    lassoXyk <- cv.glmnet(xtrain0 , ytrain0)
    yGd0x.fit[indk,4] <- predict(lassoXyk, newx = xtest , s = "lambda.min")
    
    # 5) Ridge - expansion - default CV tuning
    ridgeXyk <- cv.glmnet(xtrain0 , ytrain0  , alpha = 0)
    yGd0x.fit[indk,5] <- predict(ridgeXyk, newx = xtest, s = "lambda.min")
    
    # 6) Random forest
    rfXyk <- randomForest(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                          data = ktrain0 , mtry = 4, ntree = 1000)
    yGd0x.fit[indk,6] <- predict(rfXyk, ktest)
    
    # 7) Tree (start big)
    btXyk <- rpart(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                   data = ktrain0, method = "anova",  
                   control=rpart.control(maxdepth = 15, cp = 0, xval = 5, minsplit = 10))  
    yGd0x.fit[indk,7] <- predict(btXyk, ktest)
    
    # 8) Tree (small tree)
    stXyk <- rpart(dy ~ region + lemp.0 + lpop.0 + lavg_pay.0 ,
                   data = ktrain, method = "anova", 
                   control=rpart.control(maxdepth = 3, cp = 0, xval = 0, minsplit = 10))  
    yGd0x.fit[indk,8] <- predict(stXyk, ktest) 
    
    # 9) Tree (cv)
    bestcp <- btXyk$cptable[which.min(btXyk$cptable[,"xerror"]),"CP"]
    cvXyk <- prune(btXyk , cp=bestcp)
    yGd0x.fit[indk,9] <- predict(cvXyk, ktest)    
    
  }
  
  RMSE.dP[ii, ] <- sqrt(colMeans((usedata$treated - dGx.fit)^2))
  RMSE.yP[ii, ] <- sqrt(colMeans((usedata$dy[usedata$treated == 0] - 
                                   yGd0x.fit[usedata$treated == 0, ])^2))
  
  # trim propensity scores of 1 to .95
  for(r in 1:9) {
    trimmedP[ii,r] = sum(dGx.fit[ , r] > .95)
    dGx.fit[dGx.fit[ ,r] > .95,r] <- .95
  }
  
  att.num <- c(colMeans(((usedata$treated - dGx.fit)/((pd.fit%*%matrix(1,1,9))*(1-dGx.fit)))*
                          (usedata$dy-yGd0x.fit)) ,
               mean(((usedata$treated - dGx.fit[ ,which.min(RMSE.d[ii, ])])
                     /(pd.fit*(1-dGx.fit[ ,which.min(RMSE.d[ii, ])])))*
                      (usedata$dy-yGd0x.fit[ ,which.min(RMSE.y[ii, ])])))
  att.den <- mean(usedata$treated/pd.fit)
  
  attP[ii, ] <- att.num/att.den
  
  phihat <- cbind(((usedata$treated - dGx.fit)/((pd.fit%*%matrix(1,1,9))*(1-dGx.fit)))*
                    (usedata$dy-yGd0x.fit) ,
                  ((usedata$treated - dGx.fit[ ,which.min(RMSE.d[ii, ])])
                   /(pd.fit*(1-dGx.fit[ ,which.min(RMSE.d[ii, ])])))*
                    (usedata$dy-yGd0x.fit[ ,which.min(RMSE.y[ii, ])]))/att.den
  se.attP[ii, ] <- sqrt(colMeans((phihat^2))/n)  
  
  
}

#----------------------------------------------------------------------------
# Display results
tableP <- matrix(0, 4, 10)
tableP[1,1:9] <- RMSE.yP
tableP[2,1:9] <- RMSE.dP
tableP[3,] <- attP
tableP[4,] <- se.attP
rownames(tableP)<- c("RMSE Y","RMSE D","ATET","s.e.")
colnames(tableP)<- c("No Controls", "Basic", "Expansion",
                      "Lasso (CV)", "Ridge (CV)", 
                      "Random Forest","Deep Tree", 
                      "Shallow Tree", "Tree (CV)" , "Best")
tableP = t(tableP)
tabP <- xtable(tableP, digits =c(0,rep(4,4)))
tabP

trimmedP

