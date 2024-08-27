# Day 4

library(rdd)
library(rdrobust)
library(rddensity) 
library(fixest)
library(dplyr)
# load the data from github

df <- haven::read_dta("~/Dropbox/Causal Inference Workshops/Stata R Python Materials/2023 materials/2023-Hansen-DUI-RDD-2023-06-23-lf.dta")
# write.csv(df, "~/Dropbox/Causal Inference Workshops/Stata and R Materials/2015-Hansen-DUI-RDD-2023-06-23-lf.csv", row.names = F)
df = df %>% 
  filter(bac1 > 0.03 & bac1 < 0.13)


# 1.a. create dui treatment variable for bac1>=0.08
df$dui = (df$bac1 > 0.08)

# 1.b. Re-center our running variable at bac1=0.08
df$bac1_orig = df$bac1
df$bac1 = df$bac1 - 0.08

# 1.c. Find evidence for manipulation or heaping using histograms
ggplot(df) + 
  geom_histogram(
    aes(x = bac1_orig), binwidth = 0.001,#binwidth = 0.002 generates funky plot
    alpha = 0.8, color = "steelblue"
  ) + 
  labs(
    x = "Blood Alcohol Content",
    y = "Frequency",
    title = "Replicating Figure 1 of Hansen AER 2015"
  ) + 
  theme_bw() + 
  geom_vline(xintercept = 0.08)

# # Use rddensity from Cattnaeo, Titunik and Farrell papers
 rd_density_test = rddensity::rddensity(X = df$bac1_orig, c = 0.08) 
 summary(rd_density_test)
 plot1 <- rdplotdensity(rd_density_test, df$bac1_orig, type = 'both')


# 2. Are the covariates balanced at the cutoff? 
# Use two separate bandwidths (0.03 to 0.13; 0.055 to 0.105)
# yi = Xi′γ + α1 DUIi + α2 BACi + α3 BACi × DUIi + ui
feols(
  c(white, male, age) ~ dui + bac1 + i(dui, bac1), 
  df[df$bac1_orig >= 0.03 & df$bac1_orig <= 0.13, ], vcov = "hc1"
) %>% 
  etable()

feols(
  c(white, male, age) ~ dui + bac1 + i(dui, bac1), 
  df[df$bac1_orig >= 0.055 & df$bac1_orig <= 0.105, ], vcov = "hc1"
) %>% 
  etable()


# 3. Estimate RD of DUI on Recidivism
rdrobust(
  y = df$recidivism, x = df$bac1, c = 0
) %>% 
  summary()
rdplot(
  y = df$recidivism, x = df$bac1, c = 0
)
