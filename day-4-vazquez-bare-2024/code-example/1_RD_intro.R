###############################################################################
## Intro to RD
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

data <- read.csv("./vazquez-bare-2024/Datasets/headstart.csv")
dim(data)
head(data)

cutoff <- 59.1984

## Running variable

names(data)[names(data)=="povrate60"] <- "X"

## Outcome variable

names(data)[names(data)=="mort_age59_related_postHS"] <- "Y" 

## Scatter plot

plot(data$X,data$Y)
abline(v=cutoff,lty="dashed")

###############################################################################
## RD plots
###############################################################################

## Basic syntax

rdplot(data$Y,data$X,c=cutoff)

## Evenly-spaced bins

rdplot(data$Y,data$X,c=cutoff,binselect="es")
rdplot(data$Y,data$X,c=cutoff,binselect="esmv")

## Quantile-spaced bins

rdplot(data$Y,data$X,c=cutoff,binselect="qs")
rdplot(data$Y,data$X,c=cutoff,binselect="qsmv")

## Global 2nd order polynomial

rdplot(data$Y,data$X,c=cutoff,p=2)

## Select number of bins manually

rdplot(data$Y,data$X,c=cutoff,nbins=10)

## Add confidence intervals

rdplot(data$Y,data$X,c=cutoff,nbins=10,ci=95)
