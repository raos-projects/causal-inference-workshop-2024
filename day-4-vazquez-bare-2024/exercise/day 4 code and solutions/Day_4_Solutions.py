import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
import warnings
from statsmodels.formula.api import ols

warnings.filterwarnings("ignore")

# REPLACE WITH YOUR INPUT DATA FILE PATH
csv_file = '2023-Hansen-rounded-DUI-RDD-2023-07-26-lf.csv'
df = pd.read_csv(csv_file)


# Question 1
df['BAC'] = df[['bac1', 'bac2']].min(axis=1)
df.drop(columns=['bac2'], inplace=True)
df['dui'] = 0
df.loc[df['BAC'] > 0.080, 'dui'] = 1
df['BAC_c'] = df['BAC'] - 0.080


# Question 2
df = df[(df['BAC'] >= 0.03) & (df['BAC'] <= 0.13)]
df['BAC'].plot(kind='hist', bins=1000, edgecolor='black')
plt.title('BAC Histogram')
plt.xlabel('BAC')
plt.ylabel('Density')
plt.show()

# Question 3
outcomes = ["male", "white", "age", "car_accident"]
df_filtered = df[(df["BAC"] >= 0.03) & (df["BAC"] <= 0.13)]
results_df = pd.DataFrame(columns=["Dependent Variable", "Coefficient (DUI, BAC, DUI*BAC)", "S.E. (DUI, BAC, DUI*BAC)"])

for outcome in outcomes:
    model_formula = f"{outcome} ~ C(dui) + BAC_c + C(dui):BAC_c"
    model = smf.ols(formula=model_formula, data=df_filtered).fit(cov_type='HC3')
    coefs = model.params
    std_errors = model.bse
    results_df = results_df.append({
        "Dependent Variable": outcome,
        "Coefficient (DUI, BAC, DUI*BAC)": f"{coefs['C(dui)[T.1]']:.4f}, {coefs['BAC_c']:.4f}, {coefs['C(dui)[T.1]:BAC_c']:.4f}",
        "S.E. (DUI, BAC, DUI*BAC)": f"({std_errors['C(dui)[T.1]']:.4f}), ({std_errors['BAC_c']:.4f}), ({std_errors['C(dui)[T.1]:BAC_c']:.4f})",

    }, ignore_index=True)

print(results_df)



# Question 4

df['age'] = df['age'] - 21
df['BAC_c_squared'] = df['BAC_c'] ** 2
df['dui_squared'] = df['dui'] ** 2

# Local linear regressions with linear BAC
model_formula = "recidivism ~ dui + BAC_c + dui:BAC_c"
df_filtered = df[(df['BAC_c'] >= 0.03) & (df['BAC_c'] <= 0.13)]
model = smf.ols(formula=model_formula, data=df_filtered).fit(cov_type='HC3')
coefs = model.params
std_errors = model.bse
cur_df = {
    "Dependent Variable": "recidivism",
    "Coefficient (DUI, BAC, DUI*BAC)": f"{coefs['dui']:.4f}, {coefs['BAC_c']:.4f}, {coefs['dui:BAC_c']:.4f}",
    "S.E. (DUI, BAC, DUI*BAC)": f"({std_errors['dui']:.4f}), ({std_errors['BAC_c']:.4f}), ({std_errors['dui:BAC_c']:.4f})",
}
print(cur_df)

# Local linear regression with quadratic BAC
model_formula = "recidivism ~ dui + BAC_c + BAC_c_squared + dui:BAC_c + dui:BAC_c_squared + dui_squared:BAC_c_squared"
df_filtered = df[(df['BAC_c'] >= 0.03) & (df['BAC_c'] <= 0.13)]
model = smf.ols(formula=model_formula, data=df_filtered).fit(cov_type='HC3')
coefs = model.params
std_errors = model.bse
cur_df = {
    "Dependent Variable": "recidivism",
    "Coefficient (DUI, BAC, BAC^2, DUI*BAC, DUI*BAC^2, DUI^2*BAC^2)": f"{coefs['dui']:.4f}, {coefs['BAC_c']:.4f}, {coefs['BAC_c_squared']:.4f}, {coefs['dui:BAC_c']:.4f}, {coefs['dui:BAC_c_squared']:.4f}, {coefs['dui_squared:BAC_c_squared']:.4f}",
    "S.E. (DUI, BAC, BAC^2, DUI*BAC, DUI*BAC^2, DUI^2*BAC^2)": f"({std_errors['dui']:.4f}), ({std_errors['BAC_c']:.4f}), ({std_errors['BAC_c_squared']:.4f}), ({std_errors['dui:BAC_c']:.4f}), ({std_errors['dui:BAC_c_squared']:.4f}), ({std_errors['dui_squared:BAC_c_squared']:.4f})",
}
print(cur_df)

# Local linear regressions with linear BAC and narrower bandwidth
model_formula = "recidivism ~ dui + BAC_c + dui:BAC_c"
df_filtered = df[(df['BAC_c'] >= 0.055) & (df['BAC_c'] <= 0.105)]
model = smf.ols(formula=model_formula, data=df_filtered).fit(cov_type='HC3')
coefs = model.params
std_errors = model.bse
cur_df = {
    "Dependent Variable": "recidivism",
    "Coefficient (DUI, BAC, DUI*BAC)": f"{coefs['dui']:.4f}, {coefs['BAC_c']:.4f}, {coefs['dui:BAC_c']:.4f}",
    "S.E. (DUI, BAC, DUI*BAC)": f"({std_errors['dui']:.4f}), ({std_errors['BAC_c']:.4f}), ({std_errors['dui:BAC_c']:.4f})",
}
print(cur_df)

# Local linear regression with quadratic BAC and narrower bandwidth
model_formula = "recidivism ~ dui + BAC_c + BAC_c_squared + dui:BAC_c + dui:BAC_c_squared + dui_squared:BAC_c_squared"
df_filtered = df[(df['BAC_c'] >= 0.055) & (df['BAC_c'] <= 0.105)]
model = smf.ols(formula=model_formula, data=df_filtered).fit(cov_type='HC3')
coefs = model.params
std_errors = model.bse
cur_df = {
    "Dependent Variable": "recidivism",
    "Coefficient (DUI, BAC, BAC^2, DUI*BAC, DUI*BAC^2, DUI^2*BAC^2)": f"{coefs['dui']:.4f}, {coefs['BAC_c']:.4f}, {coefs['BAC_c_squared']:.4f}, {coefs['dui:BAC_c']:.4f}, {coefs['dui:BAC_c_squared']:.4f}, {coefs['dui_squared:BAC_c_squared']:.4f}",
    "S.E. (DUI, BAC, BAC^2, DUI*BAC, DUI*BAC^2, DUI^2*BAC^2)": f"({std_errors['dui']:.4f}), ({std_errors['BAC_c']:.4f}), ({std_errors['BAC_c_squared']:.4f}), ({std_errors['dui:BAC_c']:.4f}), ({std_errors['dui:BAC_c_squared']:.4f}), ({std_errors['dui_squared:BAC_c_squared']:.4f})",
}
print(cur_df)