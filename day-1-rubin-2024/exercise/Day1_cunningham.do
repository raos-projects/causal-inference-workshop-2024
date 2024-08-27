* day1_cunningham.do

/*
DESCRIPTION:

Purpose of this exercise is to illustrate the "design" stage.  We won't be using outcome data because in the design stage, you do not.  You are "throwing away" the outcome data for now.

We will be studying the effect of health insurance on near elderly mortality using a paper by Bernie Black, et al. (2017) in Journal of Health Economics.  See the pdf "Day1_cunningham.pdf".  

First we will show that the treatment group (no health insurance) and control group (health insurance) have different mean values of the variables under consideration. This will be done in table 1.tex as well as the figure we look at later in the love plot.

Second, we will estimate the propensity score and show in a kernel density how these propensity scores differ by treatment and control.

Third, we will reweight the original variables from table 1 using the inverse probability weight (with different weighting measurements for treatment versus control). And that'll produce table2 and a nice picture showing both before and after reweighting.  And that is it!

*/

cd "/Users/scunning/Library/CloudStorage/Dropbox-MixtapeConsulting/scott cunningham/Causal Inference Workshops/Stata R Python Materials/2024 materials/day 1 code and solutions"

clear all
set more off

use "../2018-Black-HRS-w11-simplified_2023-04-07a-lf.dta", clear

* Define variables
replace CESD_wave_1 = CESD_wave_1+8

* Define variables
local vars "age gender log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1"
local race_vars "1.race 2.race 3.race 4.race"
local pscore_vars "age c.age#c.age i.gender i.race i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1 i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1 i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1 i.CESD_wave_1 i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"

* Function to calculate standardized difference
capture program drop std_diff
program define std_diff, rclass
    args var treat weight
    if "`weight'" != "" {
        local wt [aw=`weight']
    }
    quietly summarize `var' if `treat' == 1 `wt'
    local m1 = r(mean)
    local v1 = r(Var)
    quietly summarize `var' if `treat' == 0 `wt'
    local m0 = r(mean)
    local v0 = r(Var)
    local std_diff = (`m1' - `m0') / sqrt((`v1' + `v0') / 2)
    return scalar std_diff = `std_diff'
end

* Estimate propensity score and construct IPW
logit no_insurance_wave_1 `pscore_vars'
predict pscore, pr

* Generate IPW
gen ipw = 1/pscore if no_insurance_wave_1 == 1
replace ipw = 1/(1-pscore) if no_insurance_wave_1 == 0

* Show initial imbalance and weighted balance
tempname memhold
postfile `memhold' str32 varname double std_diff_unw std_diff_w using balance_stats, replace

foreach var of local vars {
    * Unweighted standardized difference
    std_diff `var' no_insurance_wave_1
    local std_diff_unw = r(std_diff)
    
    * Weighted standardized difference
    std_diff `var' no_insurance_wave_1 ipw
    local std_diff_w = r(std_diff)
    
    post `memhold' ("`var'") (`std_diff_unw') (`std_diff_w')
}

* Handle race variables separately
foreach rvar in `race_vars' {
    local rnum = substr("`rvar'", 1, 1)
    gen race_`rnum' = (race == `rnum')
    
    * Unweighted standardized difference
    std_diff race_`rnum' no_insurance_wave_1
    local std_diff_unw = r(std_diff)
    
    * Weighted standardized difference
    std_diff race_`rnum' no_insurance_wave_1 ipw
    local std_diff_w = r(std_diff)
    
    post `memhold' ("race_`rnum'") (`std_diff_unw') (`std_diff_w')
}

postclose `memhold'

* Plot propensity score distribution
twoway (kdensity pscore if no_insurance_wave_1 == 1, lpattern(dash) lcolor(red)) ///
       (kdensity pscore if no_insurance_wave_1 == 0, lcolor(blue)), ///
    legend(order(1 "Uninsured" 2 "Insured")) ///
    xtitle("Propensity Score") ytitle("Density") ///
    title("Propensity Score Distribution") ///
    name(pscore_dist, replace)
graph export "pscore_distribution.png", replace

preserve

* Create balance plot
use balance_stats, clear
gen id = _n

* Create labels for x-axis
local xlabel ""
forvalues i = 1/`=_N' {
    local varname = varname[`i']
    local xlabel `"`xlabel' `i' "`varname'" "'
}

twoway (scatter std_diff_unw id, msymbol(O) mcolor(navy)) ///
       (scatter std_diff_w id, msymbol(D) mcolor(maroon)), ///
    xline(0, lpattern(dash) lcolor(gray)) ///
    yline(0, lpattern(dash) lcolor(gray)) ///
    ylabel(-1(.25)1) ///
    title("Standardized Differences Before and After Weighting") ///
    xtitle("") ytitle("Standardized Difference") ///
    legend(order(1 "Unweighted" 2 "Weighted") rows(1)) ///
    xlabel(`xlabel', angle(90) labsize(tiny)) ///
    ylabel(, angle(0)) ///
    name(balance_plot, replace)
graph export "balance_plot.png", replace

* Display all graphs
graph combine pscore_dist balance_plot, cols(2) iscale(.5)
graph export "combined_plots.png", replace
restore

* Define variables
local vars "age gender log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1 race_1 race_2 race_3 race_4"

* Create Table 1 (Before weighting)
estpost summarize `vars' if no_insurance_wave_1 == 1
eststo treated

estpost summarize `vars' if no_insurance_wave_1 == 0
eststo control

esttab treated control using table1.tex, ///
    cells("mean(fmt(%9.2f)) sd(fmt(%9.2f))") ///
    noobs nonumber nomtitles ///
    collabels("Mean" "SD" "Mean" "SD") ///
    title("Summary Statistics Before Weighting") ///
    prehead("\begin{table}[htbp]\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{@title}" "\begin{tabular}{l*{2}{cc}}" "\toprule" "\textbf{Variables} & \multicolumn{2}{c}{Treatment} & \multicolumn{2}{c}{Control} \\") ///
    replace style(tex) booktabs

* Create Table 2 (After weighting)
estpost summarize `vars' [aw=ipw] if no_insurance_wave_1 == 1
eststo treated_w

estpost summarize `vars' [aw=ipw] if no_insurance_wave_1 == 0
eststo control_w

esttab treated_w control_w using table2.tex, ///
    cells("mean(fmt(%9.2f)) sd(fmt(%9.2f))") ///
    noobs nonumber nomtitles ///
    collabels("Mean" "SD" "Mean" "SD") ///
    title("Summary Statistics After Weighting") ///
    prehead("\begin{table}[htbp]\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{@title}" "\begin{tabular}{l*{2}{cc}}" "\toprule" "\textbf{Variables} & \multicolumn{2}{c}{Treatment} & \multicolumn{2}{c}{Control} \\") ///
    replace style(tex) booktabs

* Clear stored estimates
eststo clear

