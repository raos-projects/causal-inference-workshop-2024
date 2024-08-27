###############################################################################
## RD with discrete running variable
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

data <- read.csv("../Datasets/education.csv")
dim(data)
head(data)

data <- data[!is.na(data$nextGPA_nonorm),]

table(data$X)
length(table(data$X))
table(data$X[abs(data$X)<=0.1])

unique_values <- sort(unique(data$X))
bar_colors <- c(rep("blue",sum(unique_values<0)-1),rep("red",sum(unique_values>=0)+1))
hist(data$X,breaks=unique_values,col=bar_colors,main="Histogram of X")
abline(v=0,lty="dashed")

data_sub <- subset(data,abs(data$X)<=1)
unique_values <- sort(unique(data_sub$X))
bar_colors <- c(rep("blue",sum(unique_values<0)-1),rep("red",sum(unique_values>=0)+1))
hist(data_sub$X,breaks=unique_values,col=bar_colors,main="Histogram of X")
abline(v=0,lty="dashed")

## Outcome variable

names(data)[names(data)=="nextGPA_nonorm"] <- "Y"

plot(data$X[abs(data$X)<=0.1],data$Y[abs(data$X)<=0.1])

###############################################################################
## Continuity-based falsification analysis
###############################################################################

## Manipulation test

hist_breaks <- seq(-3,2,by=0.1)
bar_colors <- c(rep("blue",max(which(hist_breaks<0))),rep("red",min(which(hist_breaks>=0))))
hist(data$X,breaks=hist_breaks,freq=TRUE,col=bar_colors)
abline(v=0,lty="dashed")

rddens <- rddensity(data$X)
summary(rddens)
rdplotdensity(rddens,data$X)

## Placebo outcomes

rdplot(data$hsgrade_pct,data$X)
summary(rdrobust(data$hsgrade_pct,data$X))

rdplot(data$age_at_entry,data$X)
summary(rdrobust(data$age_at_entry,data$X))

rdplot(data$totcredits_year1,data$X)
summary(rdrobust(data$totcredits_year1,data$X))

###############################################################################
## Continuity-based TE estimation
###############################################################################

rdplot(data$Y,data$X)
summary(rdrobust(data$Y,data$X))

# Collapse the data

data_coll <- aggregate(data,by=list(data$X),FUN = mean)
dim(data_coll)
head(data_coll)

summary(rdrobust(data_coll$hsgrade_pct,data_coll$X))
summary(rdrobust(data_coll$age_at_entry,data_coll$X))
summary(rdrobust(data_coll$totcredits_year1,data_coll$X))

rdplot(data_coll$Y,data_coll$X)
summary(rdrobust(data_coll$Y,data_coll$X))

###############################################################################
## Local randomization falsification analysis
###############################################################################

## Manipulation test

table(data$X[abs(data$X)<=0.05])

data_sub <- subset(data,abs(data$X)<=0.5)
unique_values <- sort(unique(data_sub$X))
bar_colors <- c(rep("blue",sum(unique_values<0)-1),rep("red",sum(unique_values>=0)+1))
hist(data_sub$X,breaks=unique_values,col=bar_colors,main="Histogram of X")
abline(v=0,lty="dashed")

binom.test(67,208,p=0.5)

## Placebo outcomes

rdr <- rdrandinf(data$hsgrade_pct,data$X,wl=-.00005,wr=0.01)
rdr <- rdrandinf(data$age_at_entry,data$X,wl=-.00005,wr=0.01)
rdr <- rdrandinf(data$totcredits_year1,data$X,wl=-.00005,wr=0.01)

###############################################################################
## Local randomization window selection and TE estimation
###############################################################################

rdw <- rdwinselect(data$X,data[,c("hsgrade_pct","totcredits_year1","age_at_entry","male","bpl_north_america")],
                   wmasspoints=TRUE)

wl <- rdw$w_left - 0.001
wr <- rdw$w_right + 0.001

rdr <- rdrandinf(data$Y,data$X,wl=wl,wr=wr)

