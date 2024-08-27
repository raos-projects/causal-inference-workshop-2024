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

cheng_hoekstra_limited = cheng_hoekstra %>% 
  filter(treatment_date == 2006|treat == 0)
table(cheng_hoekstra_limited$treat, 
      cheng_hoekstra_limited$state)
cheng_hoekstra_limited$post = NA
cheng_hoekstra_limited$post[cheng_hoekstra_limited$year <= 2005 ] = 0
cheng_hoekstra_limited$post[cheng_hoekstra_limited$year >  2005 ] = 1
table(cheng_hoekstra_limited$treat, 
      cheng_hoekstra_limited$post)


# Manual DID
mean11 = mean(log(cheng_hoekstra_limited$homicide[cheng_hoekstra_limited$treat == 1 & 
                                                 cheng_hoekstra_limited$post == 1  ]))

mean10 = mean(log(cheng_hoekstra_limited$homicide[cheng_hoekstra_limited$treat == 1 & 
                                                    cheng_hoekstra_limited$post == 0  ]))

mean01 = mean(log(cheng_hoekstra_limited$homicide[cheng_hoekstra_limited$treat == 0 & 
                                                    cheng_hoekstra_limited$post == 1  ]))

mean00 = mean(log(cheng_hoekstra_limited$homicide[cheng_hoekstra_limited$treat == 0 & 
                                                    cheng_hoekstra_limited$post == 0  ]))
sum((mean11 -mean01) -(mean10 - mean00) )

#Simple Regression DiD

did1 = feols(log(homicide) ~ post*treat, data = cheng_hoekstra_limited, 
             cluster = "state")
summary(did1)

# Event Study DiD


did2 = feols(log(homicide) ~ i(year, treat, 2005) | state + year, data = cheng_hoekstra_limited, 
             cluster = "state")
iplot(did2)

did3 = feols(homicide ~ i(year, treat, 2005) | state  + year, data = cheng_hoekstra_limited, 
             cluster = "state")
iplot(did3)

# Covariates

did3 = feols(log(homicide) ~ i(year, treat, 2005)  + police + unemployment_rate + white_m_15_24 + black_m_15_24 +
               poverty + prisoner + population  | state + year, data = cheng_hoekstra_limited, 
             cluster = "state")
iplot(did3)
did4 = feols(log(homicide) ~ post*treat+ police + unemployment_rate + white_m_15_24 + black_m_15_24 +
               poverty + prisoner + population, data = cheng_hoekstra_limited, 
             cluster = "state")
summary(did4)

# doubly robust DID
cheng_hoekstra_limited$treat_post = 0
cheng_hoekstra_limited$treat_post[cheng_hoekstra_limited$treat == 1 & cheng_hoekstra_limited$post == 1] = 1
cheng_hoekstra_limited$unit_id = as.numeric(paste0(cheng_hoekstra_limited$state_id, cheng_hoekstra_limited$year))
cheng_hoekstra_limited$log_homicide = log(cheng_hoekstra_limited$homicide)
drdid_1 = drdid(yname = "log_homicide", 
                tname = "post", 
                idname = "unit_id",
                dname = "treat", 
                xformla = ~ police + unemployment_rate + white_m_15_24 + black_m_15_24 +
                  poverty + prisoner + population, 
                data = cheng_hoekstra_limited, panel = F)




### For fun, dynamic DID

# Callaway and Sant'Anna
# Sun and Abraham 
# Stacked DiD 

cheng_hoekstra$treatment_date[is.na(cheng_hoekstra$treatment_date) == T] = 0
cheng_hoekstra$log_homicide = log(cheng_hoekstra$homicide)
cheng_hoekstra$treat_post = 0
cheng_hoekstra$treat_post[cheng_hoekstra$treat == 1 & cheng_hoekstra$treatment_date <= cheng_hoekstra$year] = 1

simp_att = feols(log_homicide ~ treat_post | state + year, data = cheng_hoekstra, 
             cluster = "state")
summary(simp_att)


atts = att_gt(
  yname = "log_homicide", 
  tnam = "year", 
  idname = "state_id", 
  gname = "treatment_date", 
  data = cheng_hoekstra,
  clustervars = "state_id"
)
att_dyn = aggte(atts, type = "dynamic")
att_group = aggte(atts, type = "group")

ggdid(att_dyn)


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
