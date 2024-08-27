import pandas as pd
import numpy as np
import statsmodels.api as sm

# REPLACE WITH YOUR ACTUAL PATH!!!
input_file = "/Users/Cheng-Hoekstra-castle-doctrine-simplified.dta"

# Convert to CSV for convenience of viewing
df = pd.read_stata(input_file)
# REPLACE WITH YOUR ACTUAL PATH!!!
df.to_csv('/Users/input.csv', index=False)

# Question 1
df['homicide_log'] = np.log(df['homicide'])
df = df[~df['treatment_date'].isin([2005, 2007, 2008, 2009])]
df = df[df['year'] != 2010]
df['post'] = np.where(df['year'] >= 2006, 1, 0)
df['treat'] = np.where(df['treatment_date'] == 2006, 1, 0)
df['post_treat'] = df['post'] * df['treat']
model = sm.OLS(df['homicide_log'], sm.add_constant(df[['post', 'treat', 'post_treat']]))
result = model.fit(cov_type='cluster', cov_kwds={'groups': df['state']})
model_weighted = sm.WLS(df['homicide_log'], sm.add_constant(df[['post', 'treat', 'post_treat']]), weights=df['population'])
result_weighted = model_weighted.fit(cov_type='cluster', cov_kwds={'groups': df['state']})
y11 = df.loc[(df['post'] == 1) & (df['treat'] == 1), 'homicide_log'].mean()
y10 = df.loc[(df['post'] == 0) & (df['treat'] == 1), 'homicide_log'].mean()
y01 = df.loc[(df['post'] == 1) & (df['treat'] == 0), 'homicide_log'].mean()
y00 = df.loc[(df['post'] == 0) & (df['treat'] == 0), 'homicide_log'].mean()
did = (y11 - y10) - (y01 - y00)
print("Difference in Differences (DiD) Summary:")
print(f"DiD Estimate: {did}")

# Question 3
for year in range(2000, 2010):
    if year != 2005:
        df[f'year_{year}'] = (df['year'] == year).astype(int)
for year_col in ["year_2000", "year_2001", "year_2002", "year_2003", "year_2004", "year_2006", "year_2007", "year_2008", "year_2009"]:
    df[f'treat:{year_col}'] = df['treat'] * df[year_col]
df['homicide_log'] = np.log(df['homicide'])

X = df[['treat'] + ["year_2000", "year_2001", "year_2002", "year_2003", "year_2004", "year_2006", "year_2007", "year_2008", "year_2009"] + ['treat:year_2000', 'treat:year_2001', 'treat:year_2002', 'treat:year_2003', 'treat:year_2004', 'treat:year_2006', 'treat:year_2007', 'treat:year_2008', 'treat:year_2009']]
y = df['homicide_log']

model = sm.OLS(y, sm.add_constant(X))
result = model.fit(cov_type='cluster', cov_kwds={'groups': df['state']})
print(result.summary())