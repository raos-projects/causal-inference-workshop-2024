# Goal: Balancing control and treatment groups 
# *Day 3 Northwestern Main Causal Inference Workshop
# *Homework 3
# *Underlying PaperL  Cheng  and  Hoekstra, Does Strengthening Self-Defense Law Deter Crime or Escalate Violence?  Evidence from Expansions to Castle Doctrine, Journal of Human Resources 48(3), 821-853 (2013).


library(fixest)
library(dplyr)
library(did) 
library(ggplot2)
library(haven)
library(tidyr)
library(DRDID)
cheng_hoekstra = read_dta("~/Dropbox/Causal Inference Workshops/Stata R Python Materials/2023 materials/Cheng-Hoekstra-castle-doctrine-simplified.dta")

table(is.na(cheng_hoekstra$treatment_date))
cheng_hoekstra$treat = 1
cheng_hoekstra$treat[is.na(cheng_hoekstra$treatment_date) == T] = 0

# CS Estimator
# a.	What is the overall ATT, across all treatment years?
# b.	How does this compare to the ATT estimated last week, using only the states that adopted rules in 2006?
# c. Generate the group-level ATTs (specific to each treatment year).  Which are significant/insignificant? Is there a noteworthy pattern?  It will be harder, of course, to find significance for a treatment year with fewer treated states. 


cheng_hoekstra$treatment_date[is.na(cheng_hoekstra$treatment_date) == T] = 0
cheng_hoekstra$log_homicide = log(cheng_hoekstra$homicide)





atts = att_gt(
  yname = "log_homicide", 
  tnam = "year", 
  idname = "state_id", 
  gname = "treatment_date", 
  data = cheng_hoekstra,
  clustervars = "state_id"
)
aggte(atts, type = "simple")

att_dyn = aggte(atts, type = "dynamic")
att_group = aggte(atts, type = "group")

# compare to OLS
cheng_hoekstra$treat_post = 0
cheng_hoekstra$treat_post[cheng_hoekstra$treat == 1 & cheng_hoekstra$treatment_date <= cheng_hoekstra$year] = 1

ggdid(att_dyn)
simp_att = feols(log_homicide ~ treat_post | state + year, data = cheng_hoekstra, 
                 cluster = "state")
summary(simp_att)



atts_simp = att_gt(
  yname = "log_homicide", 
  tnam = "year", 
  idname = "state_id", 
  gname = "treatment_date", 
  data = cheng_hoekstra,
  xformla = ~ police + unemployment_rate + white_m_15_24 , 
  clustervars = "state_id",
  control_group = "notyettreated"
)
aggte(atts_simp, type = "simple")

aggte(atts_simp, type = "dynamic")
aggte(atts_simp, type = "group")

ggdid(aggte(atts_simp, type = "dynamic"))



atts_cov = att_gt(
  yname = "log_homicide", 
  tnam = "year", 
  idname = "state_id", 
  gname = "treatment_date", 
  data = cheng_hoekstra,
  clustervars = "state_id", 
  xformla = ~ police + poverty + prisoner, 
  control_group = "nevertreated", 
  anticipation = 0
)
att_dyn_cov = aggte(atts_cov, type = "dynamic")
att_group_cov = aggte(atts_cov, type = "group")

ggdid(att_dyn_cov)

atts_cov_ipw = att_gt(
  yname = "log_homicide", 
  tnam = "year", 
  idname = "state_id", 
  gname = "treatment_date", 
  data = cheng_hoekstra,
  clustervars = "state_id", 
  xformla = ~ police + poverty + prisoner, 
  control_group = "nevertreated",
  est_method = 'ipw',
  anticipation = 0
)
att_dyn_cov_ipw = aggte(atts_cov_ipw, type = "dynamic")
att_group_cov_ipw = aggte(atts_cov_ipw, type = "group")

ggdid(att_dyn_cov_ipw)


#Sun and Abraham estiamtion, no covariates
att_sa = feols(log(homicide) ~ sunab(treatment_date, year) | state + year, data = cheng_hoekstra, 
                      cluster = "state")
iplot(att_sa)


# Bacon Decomposion! 


library(bacondecomp)
df_bacon <- bacon(log_homicide ~ treat_post,
                  data = cheng_hoekstra,
                  id_var = "state",
                  time_var = "year")
coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))


ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()
