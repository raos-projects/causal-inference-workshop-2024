rm(list = ls())

# Load necessary libraries
library(dplyr)
library(caret)
library(glmnet)
library(randomForest)
library(purrr)
library(e1071)

# Load data
data <- read.csv("../Data/processed_esti.csv")

# Recombine some dummies
data <- data %>%
  mutate(
    gender = gender_female + 2 * gender_male + 3 * gender_transgender,
    ethnicgrp = ethnicgrp_asian + 2 * ethnicgrp_black + 3 * ethnicgrp_mixed_multiple +
      4 * ethnicgrp_other + 5 * ethnicgrp_white
  )

# Set seed for reproducibility
set.seed(73023)

# Split data into a training (~50%) and test (~50%) set
data <- data %>%
  mutate(trrnd = runif(n())) %>%
  mutate(trdata = trrnd > median(trrnd))

# ATE in full sample and training sample
model_full <- lm(y ~ w, data = data)
model_train <- lm(y ~ w, data = filter(data, trdata == 1))

# Horvitz-Thompson transformation
p <- mean(data$w[data$trdata == 1])
data <- data %>%
  mutate(
    H = (w - p) / (p * (1 - p)),
    HY = H * y
  )

# Generate some candidate CATE estimates using the training data
data <- data %>%
  mutate(
    YNo = (1 - w) * y / (1 - p),
    YAll = w * y / p,
    Rrnd = runif(n()) * (trdata == 0)
  )

rmed <- median(data$Rrnd[data$trdata == 0], na.rm = TRUE)
data <- data %>%
  mutate(treatR = Rrnd > rmed & trdata == 0)

# Baseline regression
model_baseline <- lm(HY ~ gender + ethnicgrp + partners1 + postlaunch + msm + age + imd_decile, data = filter(data, trdata == 1))
data$cate1 <- predict(model_baseline, newdata = data)
data <- data %>%
  mutate(
    treat1 = cate1 > 0,
    Yt1 = treat1 * YAll + (1 - treat1) * YNo
  )

rmed <- median(data$cate1[data$trdata == 0], na.rm = TRUE)
data <- data %>%
  mutate(treat1R = cate1 > rmed & cate1 > 0 & trdata == 0)

# Lasso with full interaction
data <- data %>%
  mutate(age2 = age^2, age3 = age^3)

x_train <- model.matrix(HY ~ gender * ethnicgrp * partners1 * postlaunch * msm * imd_decile * age * age2 * age3, data = filter(data, trdata == 1))
y_train <- data$HY[data$trdata == 1]
model_lasso <- cv.glmnet(x_train, y_train, alpha = 1)

data$cate2 <- predict(model_lasso, newx = model.matrix(~ gender * ethnicgrp * partners1 * postlaunch * msm * imd_decile * age * age2 * age3, data = data), s = "lambda.min")
data <- data %>%
  mutate(
    treat2 = cate2 > 0,
    Yt2 = treat2 * YAll + (1 - treat2) * YNo
  )

rmed <- median(data$cate2[data$trdata == 0], na.rm = TRUE)
data <- data %>%
  mutate(treat2R = cate2 > rmed & cate2 > 0 & trdata == 0)

# Ridge with full interaction
model_ridge <- cv.glmnet(x_train, y_train, alpha = 0)

data$cate3 <- predict(model_ridge, newx = model.matrix(~ gender * ethnicgrp * partners1 * postlaunch * msm * imd_decile * age * age2 * age3, data = data), s = "lambda.min")
data <- data %>%
  mutate(
    treat3 = cate3 > 0,
    Yt3 = treat3 * YAll + (1 - treat3) * YNo
  )

rmed <- median(data$cate3[data$trdata == 0], na.rm = TRUE)
data <- data %>%
  mutate(treat3R = cate3 > rmed & cate3 > 0 & trdata == 0)

# Random forest
model_rf <- randomForest(HY ~ gender + ethnicgrp + partners1 + postlaunch + msm + age + imd_decile, data = filter(data, trdata == 1), ntree = 500, nodesize = 20, random_state = 731)
data$cate4 <- predict(model_rf, newdata = data)
data <- data %>%
  mutate(
    treat4 = cate4 > 0,
    Yt4 = treat4 * YAll + (1 - treat4) * YNo
  )

rmed <- median(data$cate4[data$trdata == 0], na.rm = TRUE)
data <- data %>%
  mutate(treat4R = cate4 > rmed & cate4 > 0 & trdata == 0)

# Random forest
model_rf2 <- randomForest(HY ~ gender + ethnicgrp + partners1 + postlaunch + msm + age + imd_decile, data = filter(data, trdata == 1), ntree = 500, nodesize = 1, random_state = 731)
data$cate5 <- predict(model_rf2, newdata = data)
data <- data %>%
  mutate(
    treat5 = cate5 > 0,
    Yt5 = treat5 * YAll + (1 - treat5) * YNo
  )

rmed <- median(data$cate5[data$trdata == 0], na.rm = TRUE)
data <- data %>%
  mutate(treat5R = cate5 > rmed & cate5 > 0 & trdata == 0)


# Expected outcome calculations
p0 <- mean(data$w[data$trdata == 0])
data <- data %>%
  mutate(
    YNo_out = (1 - w) * y / (1 - p0),
    YAll_out = w * y / p0,
    Yt1_out = treat1 * YAll_out + (1 - treat1) * YNo_out,
    Yt2_out = treat2 * YAll_out + (1 - treat2) * YNo_out,
    Yt3_out = treat3 * YAll_out + (1 - treat3) * YNo_out,
    Yt4_out = treat4 * YAll_out + (1 - treat4) * YNo_out,
    Yt5_out = treat5 * YAll_out + (1 - treat5) * YNo_out
  )

summary(data[data$trdata == 1, c("YNo", "YAll", "Yt1", "Yt2", "Yt3", "Yt4", "Yt5")])
summary(data[data$trdata == 0, c("YNo_out", "YAll_out", "Yt1_out", "Yt2_out", "Yt3_out", "Yt4_out", "Yt5_out")])

# Additional calculations and regressions

p0 <- mean(data$w[data$trdata == 0])
data <- data %>%
  mutate(
    mu1 = w * y,
    mu0 = (1 - w) * y,
    c_w = w - p0,
    mu1_t1 = treat1 * w * y,
    mu0_t1 = (1 - treat1) * (1 - w) * y,
    mu1_t2 = treat2 * w * y,
    mu0_t2 = (1 - treat2) * (1 - w) * y,
    mu1_t3 = treat3 * w * y,
    mu0_t3 = (1 - treat3) * (1 - w) * y,
    mu1_t4 = treat4 * w * y,
    mu0_t4 = (1 - treat4) * (1 - w) * y,
    mu1_t5 = treat5 * w * y,
    mu0_t5 = (1 - treat5) * (1 - w) * y
  )

m1 <- mean(data$mu1[data$trdata == 0])
m0 <- mean(data$mu0[data$trdata == 0])
m1_t1 <- mean(data$mu1_t1[data$trdata == 0])
m0_t1 <- mean(data$mu0_t1[data$trdata == 0])
m1_t2 <- mean(data$mu1_t2[data$trdata == 0])
m0_t2 <- mean(data$mu0_t2[data$trdata == 0])
m1_t3 <- mean(data$mu1_t3[data$trdata == 0])
m0_t3 <- mean(data$mu0_t3[data$trdata == 0])
m1_t4 <- mean(data$mu1_t4[data$trdata == 0])
m0_t4 <- mean(data$mu0_t4[data$trdata == 0])
m1_t5 <- mean(data$mu1_t5[data$trdata == 0])
m0_t5 <- mean(data$mu0_t5[data$trdata == 0])

data <- data %>%
  mutate(
    sc0 = mu0 / (1 - p0) + (m0 / ((1 - p0)^2)) * c_w,
    sc1 = mu1 / p0 - (m1 / (p0^2)) * c_w,
    sct1 = mu1_t1 / p0 + mu0_t1 / (1 - p0) + (m0_t1 / ((1 - p0)^2) - m1_t1 / (p0^2)) * c_w,
    sct2 = mu1_t2 / p0 + mu0_t2 / (1 - p0) + (m0_t2 / ((1 - p0)^2) - m1_t2 / (p0^2)) * c_w,
    sct3 = mu1_t3 / p0 + mu0_t3 / (1 - p0) + (m0_t3 / ((1 - p0)^2) - m1_t3 / (p0^2)) * c_w,
    sct4 = mu1_t4 / p0 + mu0_t4 / (1 - p0) + (m0_t4 / ((1 - p0)^2) - m1_t4 / (p0^2)) * c_w,
    sct5 = mu1_t5 / p0 + mu0_t5 / (1 - p0) + (m0_t5 / ((1 - p0)^2) - m1_t5 / (p0^2)) * c_w
  )

reg_sc0 <- lm(sc0 ~ 1, data = filter(data, trdata == 0))
reg_sc1 <- lm(sc1 ~ 1, data = filter(data, trdata == 0))
reg_sct1 <- lm(sct1 ~ 1, data = filter(data, trdata == 0))
reg_sct2 <- lm(sct2 ~ 1, data = filter(data, trdata == 0))
reg_sct3 <- lm(sct3 ~ 1, data = filter(data, trdata == 0))
reg_sct4 <- lm(sct4 ~ 1, data = filter(data, trdata == 0))
reg_sct5 <- lm(sct5 ~ 1, data = filter(data, trdata == 0))

summary(reg_sc0)
summary(reg_sc1)
summary(reg_sct1)
summary(reg_sct2)
summary(reg_sct3)
summary(reg_sct4)
summary(reg_sct5)

data <- data %>%
  mutate(
    d1 = sc1 - sc0,
    dt1 = sct1 - sc0,
    dt2 = sct2 - sc0,
    dt3 = sct3 - sc0,
    dt4 = sct4 - sc0,
    dt5 = sct5 - sc0
  )

reg_d1 <- lm(d1 ~ 1, data = filter(data, trdata == 0))
reg_dt1 <- lm(dt1 ~ 1, data = filter(data, trdata == 0))
reg_dt2 <- lm(dt2 ~ 1, data = filter(data, trdata == 0))
reg_dt3 <- lm(dt3 ~ 1, data = filter(data, trdata == 0))
reg_dt4 <- lm(dt4 ~ 1, data = filter(data, trdata == 0))
reg_dt5 <- lm(dt5 ~ 1, data = filter(data, trdata == 0))

summary(reg_d1)
summary(reg_dt1)
summary(reg_dt2)
summary(reg_dt3)
summary(reg_dt4)
summary(reg_dt5)

data <- data %>%
  mutate(
    YCR = treatR*YAll_out + (1-treatR)*YNo_out,
    Yt1R_out = treat1R * YAll_out + (1 - treat1R) * YNo_out,
    Yt2R_out = treat2R * YAll_out + (1 - treat2R) * YNo_out,
    Yt3R_out = treat3R * YAll_out + (1 - treat3R) * YNo_out,
    Yt4R_out = treat4R * YAll_out + (1 - treat4R) * YNo_out,
    Yt5R_out = treat5R * YAll_out + (1 - treat5R) * YNo_out
  )

summary(data[data$trdata == 0, c("YNo_out", "YCR", "Yt1R_out", "Yt2R_out", "Yt3R_out", "Yt4R_out", "Yt5R_out")])

data <- data %>%
  mutate(
    mu1_R = treatR * w * y,
    mu0_R = (1 - treatR) * (1 - w) * y,
    mu1_t1R = treat1R * w * y,
    mu0_t1R = (1 - treat1R) * (1 - w) * y,
    mu1_t2R = treat2R * w * y,
    mu0_t2R = (1 - treat2R) * (1 - w) * y,
    mu1_t3R = treat3R * w * y,
    mu0_t3R = (1 - treat3R) * (1 - w) * y,
    mu1_t4R = treat4R * w * y,
    mu0_t4R = (1 - treat4R) * (1 - w) * y,
    mu1_t5R = treat5R * w * y,
    mu0_t5R = (1 - treat5R) * (1 - w) * y,
  )

m1R <- mean(data$mu1_R[data$trdata == 0])
m0R <- mean(data$mu0_R[data$trdata == 0])
m1R_t1 <- mean(data$mu1_t1R[data$trdata == 0])
m0R_t1 <- mean(data$mu0_t1R[data$trdata == 0])
m1R_t2 <- mean(data$mu1_t2R[data$trdata == 0])
m0R_t2 <- mean(data$mu0_t2R[data$trdata == 0])
m1R_t3 <- mean(data$mu1_t3R[data$trdata == 0])
m0R_t3 <- mean(data$mu0_t3R[data$trdata == 0])
m1R_t4 <- mean(data$mu1_t4R[data$trdata == 0])
m0R_t4 <- mean(data$mu0_t4R[data$trdata == 0])
m1R_t5 <- mean(data$mu1_t5R[data$trdata == 0])
m0R_t5 <- mean(data$mu0_t5R[data$trdata == 0])

data <- data %>%
  mutate(
    scR = mu1_R / p0 + mu0_R / (1 - p0) + (m0R / ((1 - p0)^2) - m1R / (p0^2)) * c_w,
    sct1R = mu1_t1R / p0 + mu0_t1R / (1 - p0) + (m0R_t1 / ((1 - p0)^2) - m1R_t1 / (p0^2)) * c_w,
    sct2R = mu1_t2R / p0 + mu0_t2R / (1 - p0) + (m0R_t2 / ((1 - p0)^2) - m1R_t2 / (p0^2)) * c_w,
    sct3R = mu1_t3R / p0 + mu0_t3R / (1 - p0) + (m0R_t3 / ((1 - p0)^2) - m1R_t3 / (p0^2)) * c_w,
    sct4R = mu1_t4R / p0 + mu0_t4R / (1 - p0) + (m0R_t4 / ((1 - p0)^2) - m1R_t4 / (p0^2)) * c_w,
    sct5R = mu1_t5R / p0 + mu0_t5R / (1 - p0) + (m0R_t5 / ((1 - p0)^2) - m1R_t5 / (p0^2)) * c_w
  )

reg_scR <- lm(scR ~ 1, data = filter(data, trdata == 0))
reg_sct1R <- lm(sct1R ~ 1, data = filter(data, trdata == 0))
reg_sct2R <- lm(sct2R ~ 1, data = filter(data, trdata == 0))
reg_sct3R <- lm(sct3R ~ 1, data = filter(data, trdata == 0))
reg_sct4R <- lm(sct4R ~ 1, data = filter(data, trdata == 0))
reg_sct5R <- lm(sct5R ~ 1, data = filter(data, trdata == 0))

summary(reg_scR)
summary(reg_sct1R)
summary(reg_sct2R)
summary(reg_sct3R)
summary(reg_sct4R)
summary(reg_sct5R)

data <- data %>%
  mutate(
    dt1R = sct1R - scR,
    dt2R = sct2R - scR,
    dt3R = sct3R - scR,
    dt4R = sct4R - scR,
    dt5R = sct5R - scR
  )

reg_dt1R <- lm(dt1R ~ 1, data = filter(data, trdata == 0))
reg_dt2R <- lm(dt2R ~ 1, data = filter(data, trdata == 0))
reg_dt3R <- lm(dt3R ~ 1, data = filter(data, trdata == 0))
reg_dt4R <- lm(dt4R ~ 1, data = filter(data, trdata == 0))
reg_dt5R <- lm(dt5R ~ 1, data = filter(data, trdata == 0))

summary(reg_dt1R)
summary(reg_dt2R)
summary(reg_dt3R)
summary(reg_dt4R)
summary(reg_dt5R)

