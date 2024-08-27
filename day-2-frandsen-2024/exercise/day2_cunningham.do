* day2_cunningham.do
/*
DESCRIPTION:
Purpose of this exercise is to illustrate the "analysis" (the stage after "design") where we estimate the causal effects. This uses the same dataset we used yesterday (Bernie's health insurance and mortality dataset). Remember this is a panel of "near elderly" folks aged 50-60(ish). Highly imbalanced on variables but which we discussed you could reweight with the IPW.
*/

cd "/Users/scunning/Library/CloudStorage/Dropbox-MixtapeConsulting/scott cunningham/Causal Inference Workshops/Stata R Python Materials/2024 materials/day 1 code and solutions"

clear all
set more off
use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Locals for regressions 
local two_power_age = "age c.age#c.age"
local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"

** Illustrate the syntax for five estimators: IPW, NNM without bias adjustment, NNM with bias adjustment, Propensity score matching, and regression adjustment.  We will focus on just the 7th wave to illustrate.  This is looking at health insurance (no health insurance) on 7th wave mortality.


* 0. OLS
reg death_by_wave_7 no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')


* 1. Inverse Probability Weighting (IPW)
teffects ipw (death_by_wave_7) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), ate vce(robust)
teffects ipw (death_by_wave_7) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), atet vce(robust)

* 2. Nearest Neighbor Matching (NNM) without bias adjustment
teffects nnmatch (death_by_wave_7 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), ate nneighbor(1) vce(robust)
teffects nnmatch (death_by_wave_7 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), atet nneighbor(1) vce(robust)

* 3. Nearest Neighbor Matching (NNM) with bias adjustment
teffects nnmatch (death_by_wave_7 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), ate nneighbor(1) vce(robust) biasadj(`two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'))
teffects nnmatch (death_by_wave_7 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), atet nneighbor(1) vce(robust) biasadj(`two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'))

* 4. Propensity Score Matching (PSM)
teffects psmatch (death_by_wave_7) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), ate nn(1) vce(robust)
teffects psmatch (death_by_wave_7) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), atet nn(1) vce(robust)

* 5. Regression Adjustment (RA)
teffects ra (death_by_wave_7 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), ate vce(robust)
teffects ra (death_by_wave_7 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), atet vce(robust)



** Now start the looping.  Start with OLS

** OLS Regression
clear all
set more off
use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Locals for regressions 
local two_power_age = "age c.age#c.age"
local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"

* Initialize matrix to store OLS results
matrix OLS_results = J(10, 3, .)

* Loop through waves 2 to 11 for OLS
forvalues y = 2/11 {
    * Estimate OLS
    reg death_by_wave_`y' no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'), robust
    
    matrix OLS_results[`y'-1, 1] = `y'
    matrix OLS_results[`y'-1, 2] = _b[no_insurance_wave_1]
    matrix OLS_results[`y'-1, 3] = _se[no_insurance_wave_1]
}

* Convert OLS matrix to dataset
clear
svmat OLS_results, names(col)
rename (c1 c2 c3) (wave OLS SE_OLS)
gen OLS_lower = OLS - 1.96*SE_OLS
gen OLS_upper = OLS + 1.96*SE_OLS
save "OLS_results.dta", replace

* Create OLS graph
use "OLS_results.dta", clear
twoway (connected OLS wave, lcolor(blue) mcolor(blue)) ///
       (rcap OLS_lower OLS_upper wave, lcolor(blue)), ///
       ytitle("Coefficient") xtitle("Wave") ///
       title("OLS Estimates Over Time") ///
       subtitle("Effect of No Insurance on Mortality") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(OLS_graph, replace)

graph export "OLS_estimates.png", replace



** IPW

clear all
set more off
use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Locals for regressions 
local two_power_age = "age c.age#c.age"
local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"


* Initialize matrices to store results
matrix ATE_results = J(10, 3, .)
matrix ATT_results = J(10, 3, .)

* Loop through waves 2 to 11
forvalues y = 2/11 {
    * Estimate ATE
    teffects ipw (death_by_wave_`y') (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), ate nolog vce(robust)
    
    matrix ATE_results[`y'-1, 1] = `y'
    matrix ATE_results[`y'-1, 2] = _b[ATE:r1vs0.no_insurance_wave_1]
    matrix ATE_results[`y'-1, 3] = _se[ATE:r1vs0.no_insurance_wave_1]
    
    * Estimate ATT
    teffects ipw (death_by_wave_`y') (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), atet nolog vce(robust)
    
    matrix ATT_results[`y'-1, 1] = `y'
    matrix ATT_results[`y'-1, 2] = _b[ATET:r1vs0.no_insurance_wave_1]
    matrix ATT_results[`y'-1, 3] = _se[ATET:r1vs0.no_insurance_wave_1]
}

* Convert matrices to datasets
clear
svmat ATE_results, names(col)
rename (c1 c2 c3) (wave ATE SE_ATE)
gen ATE_lower = ATE - 1.96*SE_ATE
gen ATE_upper = ATE + 1.96*SE_ATE
save "ATE_results.dta", replace

clear
svmat ATT_results, names(col)
rename (c1 c2 c3) (wave ATT SE_ATT)
gen ATT_lower = ATT - 1.96*SE_ATT
gen ATT_upper = ATT + 1.96*SE_ATT
save "ATT_results.dta", replace

* Determine the overall min and max for y-axis scaling
use "ATE_results.dta", clear
append using "ATT_results.dta"
summarize ATE_lower ATT_lower ATE_upper ATT_upper, detail
local ymin = r(min)
local ymax = r(max)

* Create ATE graph
use "ATE_results.dta", clear
twoway (connected ATE wave, lcolor(blue) mcolor(blue)) ///
       (rcap ATE_lower ATE_upper wave, lcolor(blue)), ///
       ytitle("ATE") xtitle("Wave") ///
       title("Average Treatment Effect") ///
       subtitle("Effect of No Insurance on Mortality") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATE_graph, replace)

* Create ATT graph
use "ATT_results.dta", clear
twoway (connected ATT wave, lcolor(red) mcolor(red)) ///
       (rcap ATT_lower ATT_upper wave, lcolor(red)), ///
       ytitle("ATT") xtitle("Wave") ///
       title("Average Treatment Effect on Treated") ///
       subtitle("Effect of No Insurance on Mortality for Uninsured") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATT_graph, replace)

* Combine graphs
graph combine ATE_graph ATT_graph, ///
       title("IPW Estimates of ATE and ATT Over Time") ///
       subtitle("Effect of No Insurance on Mortality") ///
       note("Data source: Health and Retirement Study")

graph export "ATE_ATT_combined.png", replace


** Nearest neighbor matching without bias adjustment

clear all
set more off
use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Locals for regressions 
local two_power_age = "age c.age#c.age"
local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"


* Initialize matrices to store NNM results
matrix ATE_NNM_results = J(10, 3, .)
matrix ATT_NNM_results = J(10, 3, .)

* Loop through waves 2 to 11 for NNM
forvalues y = 2/11 {
    * Estimate ATE using NNM
    teffects nnmatch (death_by_wave_`y' `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), ate nneighbor(1) vce(robust)
    
    matrix ATE_NNM_results[`y'-1, 1] = `y'
    matrix ATE_NNM_results[`y'-1, 2] = _b[ATE:r1vs0.no_insurance_wave_1]
    matrix ATE_NNM_results[`y'-1, 3] = _se[ATE:r1vs0.no_insurance_wave_1]
    
    * Estimate ATT using NNM
    teffects nnmatch (death_by_wave_`y' `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), atet nneighbor(1) vce(robust)
    
    matrix ATT_NNM_results[`y'-1, 1] = `y'
    matrix ATT_NNM_results[`y'-1, 2] = _b[ATET:r1vs0.no_insurance_wave_1]
    matrix ATT_NNM_results[`y'-1, 3] = _se[ATET:r1vs0.no_insurance_wave_1]
}

* Convert NNM matrices to datasets
clear
svmat ATE_NNM_results, names(col)
rename (c1 c2 c3) (wave ATE_NNM SE_ATE_NNM)
gen ATE_NNM_lower = ATE_NNM - 1.96*SE_ATE_NNM
gen ATE_NNM_upper = ATE_NNM + 1.96*SE_ATE_NNM
save "ATE_NNM_results.dta", replace

clear
svmat ATT_NNM_results, names(col)
rename (c1 c2 c3) (wave ATT_NNM SE_ATT_NNM)
gen ATT_NNM_lower = ATT_NNM - 1.96*SE_ATT_NNM
gen ATT_NNM_upper = ATT_NNM + 1.96*SE_ATT_NNM
save "ATT_NNM_results.dta", replace

* Create ATE NNM graph
use "ATE_NNM_results.dta", clear
twoway (connected ATE_NNM wave, lcolor(green) mcolor(green)) ///
       (rcap ATE_NNM_lower ATE_NNM_upper wave, lcolor(green)), ///
       ytitle("ATE") xtitle("Wave") ///
       title("Average Treatment Effect (NNM)") ///
       subtitle("Effect of No Insurance on Mortality") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATE_NNM_graph, replace)

* Create ATT NNM graph
use "ATT_NNM_results.dta", clear
twoway (connected ATT_NNM wave, lcolor(orange) mcolor(orange)) ///
       (rcap ATT_NNM_lower ATT_NNM_upper wave, lcolor(orange)), ///
       ytitle("ATT") xtitle("Wave") ///
       title("Average Treatment Effect on Treated (NNM)") ///
       subtitle("Effect of No Insurance on Mortality for Uninsured") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATT_NNM_graph, replace)

* Combine all graphs
graph combine  ATE_NNM_graph ATT_NNM_graph, ///
       title("NNM Estimates of ATE and ATT Over Time") ///
       subtitle("Effect of No Insurance on Mortality") ///
       note("Data source: Health and Retirement Study")

graph export "ATE_ATT_NNM_combined.png", replace




** Nearest neighbor matching WITH bias adjustment

clear all
set more off
use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Locals for regressions 
local two_power_age = "age c.age#c.age"
local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"


* Initialize matrices to store NNM results
matrix ATE_NNM_results = J(10, 3, .)
matrix ATT_NNM_results = J(10, 3, .)

* Loop through waves 2 to 11 for NNM
forvalues y = 2/11 {
    * Estimate ATE using NNM
    teffects nnmatch (death_by_wave_`y' `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), ate nneighbor(1) vce(robust) biasadj(`two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'))
    
    matrix ATE_NNM_results[`y'-1, 1] = `y'
    matrix ATE_NNM_results[`y'-1, 2] = _b[ATE:r1vs0.no_insurance_wave_1]
    matrix ATE_NNM_results[`y'-1, 3] = _se[ATE:r1vs0.no_insurance_wave_1]
    
    * Estimate ATT using NNM
    teffects nnmatch (death_by_wave_`y' `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), atet nneighbor(1) vce(robust) biasadj(`two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'))
    
    matrix ATT_NNM_results[`y'-1, 1] = `y'
    matrix ATT_NNM_results[`y'-1, 2] = _b[ATET:r1vs0.no_insurance_wave_1]
    matrix ATT_NNM_results[`y'-1, 3] = _se[ATET:r1vs0.no_insurance_wave_1]
}

* Convert NNM matrices to datasets
clear
svmat ATE_NNM_results, names(col)
rename (c1 c2 c3) (wave ATE_NNM SE_ATE_NNM)
gen ATE_NNM_lower = ATE_NNM - 1.96*SE_ATE_NNM
gen ATE_NNM_upper = ATE_NNM + 1.96*SE_ATE_NNM
save "ATE_NNM_results.dta", replace

clear
svmat ATT_NNM_results, names(col)
rename (c1 c2 c3) (wave ATT_NNM SE_ATT_NNM)
gen ATT_NNM_lower = ATT_NNM - 1.96*SE_ATT_NNM
gen ATT_NNM_upper = ATT_NNM + 1.96*SE_ATT_NNM
save "ATT_NNM_results.dta", replace

* Create ATE NNM graph
use "ATE_NNM_results.dta", clear
twoway (connected ATE_NNM wave, lcolor(green) mcolor(green)) ///
       (rcap ATE_NNM_lower ATE_NNM_upper wave, lcolor(green)), ///
       ytitle("ATE") xtitle("Wave") ///
       title("Average Treatment Effect (NNM)") ///
       subtitle("Effect of No Insurance on Mortality") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATE_NNM_graph, replace)

* Create ATT NNM graph
use "ATT_NNM_results.dta", clear
twoway (connected ATT_NNM wave, lcolor(orange) mcolor(orange)) ///
       (rcap ATT_NNM_lower ATT_NNM_upper wave, lcolor(orange)), ///
       ytitle("ATT") xtitle("Wave") ///
       title("Average Treatment Effect on Treated (NNM)") ///
       subtitle("Effect of No Insurance on Mortality for Uninsured") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATT_NNM_graph, replace)

* Combine all graphs
graph combine  ATE_NNM_graph ATT_NNM_graph, ///
       title("NNM Estimates of ATE and ATT Over Time with bias adjustment") ///
       subtitle("Effect of No Insurance on Mortality") ///
       note("Data source: Health and Retirement Study")

graph export "ATE_ATT_NNMbiasdadj_combined.png", replace


** Propensity score matching


clear all
set more off
use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Locals for regressions 
local two_power_age = "age c.age#c.age"
local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"

* Initialize matrices to store PSM results
matrix ATE_PSM_results = J(10, 3, .)
matrix ATT_PSM_results = J(10, 3, .)

* Loop through waves 2 to 11 for PSM
forvalues y = 2/11 {
    * Estimate ATE using PSM
    teffects psmatch (death_by_wave_`y') (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), ate nn(1) vce(robust)
    
    matrix ATE_PSM_results[`y'-1, 1] = `y'
    matrix ATE_PSM_results[`y'-1, 2] = _b[ATE:r1vs0.no_insurance_wave_1]
    matrix ATE_PSM_results[`y'-1, 3] = _se[ATE:r1vs0.no_insurance_wave_1]
    
    * Estimate ATT using PSM
    teffects psmatch (death_by_wave_`y') (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), atet nn(1) vce(robust)
    
    matrix ATT_PSM_results[`y'-1, 1] = `y'
    matrix ATT_PSM_results[`y'-1, 2] = _b[ATET:r1vs0.no_insurance_wave_1]
    matrix ATT_PSM_results[`y'-1, 3] = _se[ATET:r1vs0.no_insurance_wave_1]
}

* Convert PSM matrices to datasets
clear
svmat ATE_PSM_results, names(col)
rename (c1 c2 c3) (wave ATE_PSM SE_ATE_PSM)
gen ATE_PSM_lower = ATE_PSM - 1.96*SE_ATE_PSM
gen ATE_PSM_upper = ATE_PSM + 1.96*SE_ATE_PSM
save "ATE_PSM_results.dta", replace

clear
svmat ATT_PSM_results, names(col)
rename (c1 c2 c3) (wave ATT_PSM SE_ATT_PSM)
gen ATT_PSM_lower = ATT_PSM - 1.96*SE_ATT_PSM
gen ATT_PSM_upper = ATT_PSM + 1.96*SE_ATT_PSM
save "ATT_PSM_results.dta", replace

* Create ATE PSM graph
use "ATE_PSM_results.dta", clear
twoway (connected ATE_PSM wave, lcolor(blue) mcolor(blue)) ///
       (rcap ATE_PSM_lower ATE_PSM_upper wave, lcolor(blue)), ///
       ytitle("ATE") xtitle("Wave") ///
       title("Average Treatment Effect (PSM)") ///
       subtitle("Effect of No Insurance on Mortality") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATE_PSM_graph, replace)

* Create ATT PSM graph
use "ATT_PSM_results.dta", clear
twoway (connected ATT_PSM wave, lcolor(red) mcolor(red)) ///
       (rcap ATT_PSM_lower ATT_PSM_upper wave, lcolor(red)), ///
       ytitle("ATT") xtitle("Wave") ///
       title("Average Treatment Effect on Treated (PSM)") ///
       subtitle("Effect of No Insurance on Mortality for Uninsured") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATT_PSM_graph, replace)

* Combine PSM graphs
graph combine ATE_PSM_graph ATT_PSM_graph, ///
       title("PSM Estimates of ATE and ATT Over Time") ///
       subtitle("Effect of No Insurance on Mortality") ///
       note("Data source: Health and Retirement Study")

graph export "ATE_ATT_PSM_combined.png", replace


** Regression Adjustment
clear all
set more off
use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Locals for regressions 
local two_power_age = "age c.age#c.age"
local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"

* Initialize matrices to store RA results
matrix ATE_RA_results = J(10, 3, .)
matrix ATT_RA_results = J(10, 3, .)

* Loop through waves 2 to 11 for RA
forvalues y = 2/11 {
    * Estimate ATE using RA
    teffects ra (death_by_wave_`y' `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), ate vce(robust)
    
    matrix ATE_RA_results[`y'-1, 1] = `y'
    matrix ATE_RA_results[`y'-1, 2] = _b[ATE:r1vs0.no_insurance_wave_1]
    matrix ATE_RA_results[`y'-1, 3] = _se[ATE:r1vs0.no_insurance_wave_1]
    
    * Estimate ATT using RA
    teffects ra (death_by_wave_`y' `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), atet vce(robust)
    
    matrix ATT_RA_results[`y'-1, 1] = `y'
    matrix ATT_RA_results[`y'-1, 2] = _b[ATET:r1vs0.no_insurance_wave_1]
    matrix ATT_RA_results[`y'-1, 3] = _se[ATET:r1vs0.no_insurance_wave_1]
}

* Convert RA matrices to datasets
clear
svmat ATE_RA_results, names(col)
rename (c1 c2 c3) (wave ATE_RA SE_ATE_RA)
gen ATE_RA_lower = ATE_RA - 1.96*SE_ATE_RA
gen ATE_RA_upper = ATE_RA + 1.96*SE_ATE_RA
save "ATE_RA_results.dta", replace

clear
svmat ATT_RA_results, names(col)
rename (c1 c2 c3) (wave ATT_RA SE_ATT_RA)
gen ATT_RA_lower = ATT_RA - 1.96*SE_ATT_RA
gen ATT_RA_upper = ATT_RA + 1.96*SE_ATT_RA
save "ATT_RA_results.dta", replace

* Create ATE RA graph
use "ATE_RA_results.dta", clear
twoway (connected ATE_RA wave, lcolor(blue) mcolor(blue)) ///
       (rcap ATE_RA_lower ATE_RA_upper wave, lcolor(blue)), ///
       ytitle("ATE") xtitle("Wave") ///
       title("Average Treatment Effect (RA)") ///
       subtitle("Effect of No Insurance on Mortality") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATE_RA_graph, replace)

* Create ATT RA graph
use "ATT_RA_results.dta", clear
twoway (connected ATT_RA wave, lcolor(red) mcolor(red)) ///
       (rcap ATT_RA_lower ATT_RA_upper wave, lcolor(red)), ///
       ytitle("ATT") xtitle("Wave") ///
       title("Average Treatment Effect on Treated (RA)") ///
       subtitle("Effect of No Insurance on Mortality for Uninsured") ///
       ylabel(0(0.02)0.10, format(%9.3f)) ///
       yscale(range(0 0.10)) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       name(ATT_RA_graph, replace)

* Combine RA graphs
graph combine ATE_RA_graph ATT_RA_graph, ///
       title("RA Estimates of ATE and ATT Over Time") ///
       subtitle("Effect of No Insurance on Mortality") ///
       note("Data source: Health and Retirement Study")

graph export "ATE_ATT_RA_combined.png", replace




