# Goal: Balancing control and treatment groups 
# *Day 2 Northwestern Main Causal Inference Workshop
# *Homework 2
# *Underlying Paper: Black, Espin-Sanchez, French, and Litvak, "The Long-term Effect of Health Insurance on Near-Elderly Health and Mortality", 3 American Journal of Health Economics 281-311 (2017) 

library(MatchIt)
library(WeightIt)
library(dplyr)
library(cobalt)
library(haven)
library(readstata13)
library(ggplot2)

black_hrs = read_dta("~/Dropbox/Causal Inference Workshops/Stata and R Materials/2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta")

black_hrs = black_hrs %>% 
  select(ob, age, gender, race, ends_with('wave_1'), contains('death'))
black_hrs$age = black_hrs$age -51
black_hrs$treatment = as.factor(black_hrs$no_insurance_wave_1)

hrs_psm = matchit(no_insurance_wave_1~age + gender + as.factor(race)+ 
           diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
           lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
           as.factor(hh_logincome_quintile_wave_1) +
           partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
         
         data = black_hrs, method = "nearest", distance = "glm", replace = T, ratio = 1)
hrs_psm_data = get_matches(hrs_psm)
ggplot(hrs_psm_data, aes(x = distance, fill = as.factor(no_insurance_wave_1))) + geom_density(alpha = 0.6)

love.plot(hrs_psm)


# fit it by formula
ipw = weightit(no_insurance_wave_1~age + gender + as.factor(race)+ 
                 diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                 lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                 as.factor(hh_logincome_quintile_wave_1) +
                 partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
               data = black_hrs, method = 'ps', estimand = "ATE")
black_hrs_bal_tab = bal.tab(ipw, un = T)
love.plot(ipw, thresholds = 0.1)



hrs_mhm = matchit(no_insurance_wave_1~age + gender + as.factor(race)+ 
                    diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                    lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                    as.factor(hh_logincome_quintile_wave_1) +
                    partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
                  
                  data = black_hrs, method = "nearest", distance = "mahalanobis", replace = T, ratio = 1)
hrs_mhm_data = get_matches(hrs_mhm)



library(fixest)
colnames(black_hrs)
out_hrs = feols(death_by_wave_10 ~ no_insurance_wave_1 +age + gender + as.factor(race)+ 
                  diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                  lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                  as.factor(hh_logincome_quintile_wave_1) +
                  partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
                 data = black_hrs, vcov = 'HC1')
out_hrs_ps = feols(death_by_wave_10 ~ no_insurance_wave_1 +age + gender + as.factor(race)+ 
                  diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                  lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                  as.factor(hh_logincome_quintile_wave_1) +
                  partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
                data = hrs_psm_data, cluster = ~subclass,  weights = hrs_psm_data$weights)
out_hrs_ipw = feols(death_by_wave_10 ~ no_insurance_wave_1 +age + gender + as.factor(race)+ 
                     diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                     lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                     as.factor(hh_logincome_quintile_wave_1) +
                     partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
                   data = black_hrs, vcov = 'HC1', weights = ipw$weights)
out_hrs_mh = feols(death_by_wave_10 ~ no_insurance_wave_1 +age + gender + as.factor(race)+ 
                     diabetes_wave_1 + cancer_wave_1  + high_blood_pressure_wave_1 + 
                     lung_disease_wave_1 + heart_disease_wave_1 + stroke_wave_1 +
                     as.factor(hh_logincome_quintile_wave_1) +
                     partly_retired_wave_1 +fully_retired_wave_1 + unemployed_wave_1  + years_schooling_wave_1 + household_size_wave_1, 
                   data = hrs_mhm_data, cluster = ~subclass,  weights = hrs_mhm_data$weights)


etable(out_hrs, out_hrs_ps, out_hrs_ipw,out_hrs_mh, keep = 'no_insurance')
###############################################################

library(Matching)
library(tidyr)
black_hrs_m = black_hrs[is.na(black_hrs$death_by_wave_10) ==F,]
covs_hrs = black_hrs_m %>% 
  dplyr::select(-contains("death"), -treatment, -ipw, -contains('insur'), -contains("ADL")) %>% 
  drop_na()


NNMatch_hrs = Match(Y = black_hrs_m$death_by_wave_10, X = covs_hrs, 
                    Tr = black_hrs_m$no_insurance_wave_1, estimand = "ATT", 
                    BiasAdjust = T, replace = T)  
summary(NNMatch_hrs)


#### Add some advanced matching results


# ebalance, doubly robust methods, CBPS?, sbw?, genmatch? 
