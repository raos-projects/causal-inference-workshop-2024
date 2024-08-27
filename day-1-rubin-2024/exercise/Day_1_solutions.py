import pandas as pd

# ----------------- BLOCK 1 -------------------
# Convert the Stata file to CSV for convenience
if True:
    # !!! REPLACE WITH ACTUAL FILE PATH
    data = pd.read_stata('/Users/Desktop/2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta')
    # !!! REPLACE WITH ACTUAL FILE PATH
    data.to_csv('/Users/Desktop/output.csv', index=False)


# ----------------- BLOCK 2 -------------------
# Question 1

# Read the CSV file with column headers
# !!! REPLACE WITH ACTUAL FILE PATH
data = pd.read_csv('/Users/Desktop/output.csv')

# Create local variables for regressions
two_power_age = ['age']  # quadratic in age
data['age'] = data['age'] ** 2
comorbidities = ['diabetes_wave_1', 'cancer_wave_1', 'high_blood_pressure_wave_1', 'lung_disease_wave_1', 'heart_disease_wave_1', 'stroke_wave_1', 'psychiatric_disease_wave_1', 'arthritis_wave_1', 'ulcer_wave_1', 'CESD_wave_1']
money = ['hh_logincome_quintile_wave_1', 'hh_earning_quintile_wave_1']  # quintiles of logincome and earnings
labor = ['not_in_laborforce_wave_1', 'partly_retired_wave_1', 'fully_retired_wave_1', 'unemployed_wave_1', 'employed_pt_wave_1', 'employed_ft_wave_1', 'veteran_wave_1']

tabulated_data = data['no_insurance_wave_1'].value_counts()
print(tabulated_data)

# Use relative age values to avoid scale problems
data['age'] = data['age'] - 51  # age now starts at 0 and ends at 10


import statsmodels.api as sm


# Define the independent variables
independent_vars = ['age', 'gender', 'race'] + money + labor + comorbidities

data['gender'] = data['gender'].map({'Male': 0, 'Female': 1})
data['race'] = data['race'].map({'non-Hispanic White': 0, 'non-Hispanic Black': 1, 'non-Hispanic Other': 2, 'Hispanic': 3})


# Generate interaction terms
interaction_terms = ['gender:race', 'gender:age', 'race:age']
for term in interaction_terms:
    var1, var2 = term.split(':')
    data[term] = data[var1] * data[var2]

# Drop rows with missing values
data.dropna(subset=independent_vars + interaction_terms + ['no_insurance_wave_1'], inplace=True)


# Add constant term to the independent variables
X = sm.add_constant(data[independent_vars + interaction_terms])

# Define the dependent variable
y = data['no_insurance_wave_1']

# Perform logistic regression
logit_model = sm.Logit(y, X)
result = logit_model.fit()

# Print regression summary
print(result.summary())


import numpy as np

# Calculate propensity score
data['pscore'] = result.predict(X)

# Generate inverse propensity weights
data['ipw'] = np.where(data['no_insurance_wave_1'] == 1, 1 / data['pscore'], 1 / (1 - data['pscore']))

# Replace missing ipw values with 0
data['ipw'].fillna(0, inplace=True)



# Question 2

# Generate dummy variables for 'race'
dummy_races = pd.get_dummies(data['race'], prefix='race')

# Rename the columns to 'race_1' to 'race_4'
dummy_races.columns = ['race_1', 'race_2', 'race_3', 'race_4']

# Concatenate the original DataFrame with the dummy variables
data = pd.concat([data, dummy_races], axis=1)

# Bring back age to its initial value
data['age'] = data['age'] + 51

# Generate weight equal to 1 for unweighted observations
data['unwgtd'] = 1

# Define selected variables and weights
weights = ['unwgtd', 'ipw']
vars = ['age', 'gender', 'race_1', 'race_2', 'race_3', 'race_4', 'log_income_wave_1', 'diabetes_wave_1', 'cancer_wave_1', 'employed_ft_wave_1', 'veteran_wave_1']

# Loop to calculate summary stats table
summary_stats = []
for weight in weights:
    for var in vars:
        # Calculate mean and standard deviation for the insured
        mean_insured = data.loc[data['no_insurance_wave_1'] == 0, var].mean()
        sd_insured = data.loc[data['no_insurance_wave_1'] == 0, var].std()
        sample_insured = data.loc[data['no_insurance_wave_1'] == 0, var].count()

        # Calculate mean and standard deviation for the uninsured
        mean_uninsured = data.loc[data['no_insurance_wave_1'] == 1, var].mean()
        sd_uninsured = data.loc[data['no_insurance_wave_1'] == 1, var].std()
        sample_uninsured = data.loc[data['no_insurance_wave_1'] == 1, var].count()

        # Calculate t-test and normalized difference
        ttest = (mean_uninsured - mean_insured) / np.sqrt((sd_uninsured**2 / sample_uninsured) + (sd_insured**2 / sample_insured))
        ndiff = (mean_uninsured - mean_insured) / np.sqrt(((sd_uninsured**2 + sd_insured**2) / 2))

        summary_stats.append((var, weight, mean_insured, sd_insured, sample_insured, mean_uninsured, sd_uninsured, sample_uninsured, ttest, ndiff))

# Create summary stats DataFrame
summary_stats_df = pd.DataFrame(summary_stats, columns=['Variable', 'Weight', 'Mean_Insured', 'SD_Insured', 'Sample_Size_Insured', 'Mean_Uninsured', 'SD_Uninsured', 'Sample_Size_Uninsured', 'T-Test', 'Normalized_Difference'])

from tabulate import tabulate


# Create a list of lists to hold the data for tabulate
table_data = []

# Iterate over each row in the DataFrame and extract the values
for _, row in summary_stats_df.iterrows():
    table_data.append([
        row['Variable'],
        row['Mean_Uninsured'],
        row['Mean_Insured'],
        row['SD_Uninsured'],
        row['SD_Insured'],
        row['Sample_Size_Uninsured'],
        row['Sample_Size_Insured'],
        row['T-Test'],
        row['Normalized_Difference']
    ])

# Define the table headers
headers = ['Covariate', 'Unweighted - Uninsured', 'Unweighted - Insured', 'SD - Uninsured', 'SD - Insured',
           'Sample Size - Uninsured', 'Sample Size - Insured', 'T-Test on Difference', 'Normalized Difference']

# Generate the table using tabulate
table = tabulate(table_data, headers, tablefmt='pipe')

# Print the table
print(table)


# Export the summary statistics to Excel
# !!! REPLACE WITH ACTUAL FILE PATH
summary_stats_df.to_excel('/Users/Desktop/summary_stats.xlsx', index=False)



# STEP 3 and 4
import seaborn as sns
import matplotlib.pyplot as plt

# Create separate dataframes for insured and uninsured individuals
df_uninsured = data[data['no_insurance_wave_1'] == 1]
df_insured = data[data['no_insurance_wave_1'] == 0]

# Plot the kernel density for propensities
sns.kdeplot(data=df_uninsured, x='pscore', color='red', linestyle='--', label='Uninsured')
sns.kdeplot(data=df_insured, x='pscore', color='blue', label='Insured')

# Set plot labels and title
plt.xlabel('Probability of being uninsured')
plt.ylabel('Density')
plt.title('Logit Propensity Score')

# Customize tick labels and grid
plt.xticks(ticks=[0, 0.2, 0.4, 0.6, 0.8, 1])
plt.yticks(ticks=[0, 2, 4, 6, 8, 10])
plt.gca().set_yticklabels(['0', '2', '4', '6', '8', '10'])
plt.gca().set_yticks([0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10])
plt.grid(True)

# Add legend
plt.legend()

# Show the plot
plt.show()
