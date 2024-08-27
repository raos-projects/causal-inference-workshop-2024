import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import numpy as np
from statsmodels.genmod.families import Binomial
from scipy.spatial.distance import cdist
from statsmodels.iolib.summary2 import summary_col
from statsmodels.stats.weightstats import DescrStatsW

# ----------------- BLOCK 1 -------------------
# Convert the Stata file to CSV for convenience
if True:
    # !!! REPLACE WITH ACTUAL FILE PATH
    df = pd.read_stata('/Users/2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta')
    # !!! REPLACE WITH ACTUAL FILE PATH
    df.to_csv('/Users/output.csv', index=False)


# ----------------- BLOCK 2 -------------------
# Question 1

# Read the CSV file with column headers
# !!! REPLACE WITH ACTUAL FILE PATH
df = pd.read_csv('/Users/output.csv')


df['age'] = df['age'] - 51
gender_mapping = {value: index for index, value in enumerate(df['gender'].unique())}
race_mapping = {value: index for index, value in enumerate(df['race'].unique())}
df['gender'] = df['gender'].map(gender_mapping)
df['race'] = df['race'].map(race_mapping)

df["two_power_age"] = df['age'] ** 2
df["gender#gender"] = df["gender"] ** 2
df["gender#race"] = df["gender"] * df["race"]
df["race#gender"] = df["race"] * df["gender"]
df["race#race"] = df["race"] ** 2

comorbidities = [
    'diabetes_wave_1', 'cancer_wave_1', 'high_blood_pressure_wave_1',
    'lung_disease_wave_1', 'heart_disease_wave_1', 'stroke_wave_1',
    'psychiatric_disease_wave_1', 'arthritis_wave_1', 'ulcer_wave_1', 'CESD_wave_1'
]
money = ['hh_logincome_quintile_wave_1', 'hh_earning_quintile_wave_1']
labor = [
    'not_in_laborforce_wave_1', 'partly_retired_wave_1', 'fully_retired_wave_1',
    'unemployed_wave_1', 'employed_pt_wave_1', 'employed_ft_wave_1', 'veteran_wave_1'
]
other = ['gender', 'race']

X = df[["two_power_age"] + comorbidities + money + labor + other + [f'{x}#{y}' for x in other for y in other]]
y = df['death_by_wave_2']
X = sm.add_constant(X)

logit_model = sm.GLM(y, X, family=Binomial())
logit_result = logit_model.fit()
propensity_scores = logit_result.predict(X)
def propensity_score_matching(treatment, control, caliper=0.05):
    distance_matrix = cdist(treatment.values.reshape(-1, 1), control.values.reshape(-1, 1), 'mahalanobis')
    matches = []
    for i in range(distance_matrix.shape[0]):
        matched_idx = np.where(distance_matrix[i] <= caliper)[0]
        if len(matched_idx) > 0:
            matches.append((i, matched_idx[0]))
    return matches

treated_scores = propensity_scores[df['no_insurance_wave_1'] == 1]
control_scores = propensity_scores[df['no_insurance_wave_1'] == 0]
matched_pairs = propensity_score_matching(treated_scores, control_scores)
matched_treated = df[df['no_insurance_wave_1'] == 1].iloc[[pair[0] for pair in matched_pairs]]
matched_control = df[df['no_insurance_wave_1'] == 0].iloc[[pair[1] for pair in matched_pairs]]

