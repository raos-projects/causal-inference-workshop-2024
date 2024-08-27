# Goal: Balancing control and treatment groups 
# *Written by Lorenzo Franchi in April 2023 
# *Day 1 Northwestern Main Causal Inference Workshop
# *Homework 1
# *Underlying Paper: Black, Espin-Sanchez, French, and Litvak, "The Long-term Effect of Health Insurance on Near-Elderly Health and Mortality", 3 American Journal of Health Economics 281-311 (2017) 

library(MatchIt)
library(WeightIt)
library(dplyr)
library(cobalt)
library(haven)
library(here)
library(readstata13)
library(ggplot2)
path = here::i_am()

black_hrs = read_dta("~/Dropbox/Causal Inference Workshops/Stata and R Materials/2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta")

black_hrs = black_hrs %>% 
  select(ob, age, gender, race, ends_with('wave_1'))
black_hrs$age = black_hrs$age -51
black_hrs$treatment = as.factor(black_hrs$no_insurance_wave_1)

ps = glm(no_insurance_wave_1~age + gender + as.factor(race)+ 
           diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
          lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
           as.factor(hh_logincome_quintile_wave_1) +
           partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
          
         data = black_hrs, family = binomial(link ='logit'))
black_hrs$ps = ps$fitted.values

ggplot(black_hrs, aes(x = ps, fill = treatment)) + geom_density(alpha = 0.6)


####### balance table

# fit IPW by hand
black_hrs$ipw = NA
black_hrs$ipw[black_hrs$treatment == 1] = 1/black_hrs$ps[black_hrs$treatment == 1]
black_hrs$ipw[black_hrs$treatment == 0] = 1/(1-black_hrs$ps[black_hrs$treatment == 0])

# fit it by formula
ipw = weightit(no_insurance_wave_1~age + gender + as.factor(race)+ 
                diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                as.factor(hh_logincome_quintile_wave_1) +
                partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
              data = black_hrs, method = 'ps', estimand = "ATE")
black_hrs_bal_tab = bal.tab(ipw, un = T)
love.plot(ipw, thresholds = 0.1)

#### Trim
black_hrs %>% 
  filter(ps < 0.95 & ps > 0.05) %>% 
  ggplot(aes(x = ps, fill = treatment)) + geom_density(alpha = 0.6)

trimmed_black_hrs = black_hrs %>% 
  filter(ps < 0.95 & ps > 0.05) 

#rerun all analysis with the trimmed data

trim_ps = glm(no_insurance_wave_1~age + gender + as.factor(race)+ 
           diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
           lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
           as.factor(hh_logincome_quintile_wave_1) +
           partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
         
         data = trimmed_black_hrs, family = binomial(link ='logit'))
trimmed_black_hrs$ps_trim = trim_ps$fitted.values

trimmed_black_hrs %>% 
  ggplot(aes(x = ps_trim, fill = treatment)) + geom_density(alpha = 0.6)


trim_ipw = weightit(no_insurance_wave_1~age + gender + as.factor(race)+ 
                 diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                 lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                 as.factor(hh_logincome_quintile_wave_1) +
                 partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
               
               data = trimmed_black_hrs, method = 'ps')
trim_black_hrs_bal_tab = bal.tab(trim_ipw, un = T)
love.plot(trim_ipw, thresholds = 0.1)

