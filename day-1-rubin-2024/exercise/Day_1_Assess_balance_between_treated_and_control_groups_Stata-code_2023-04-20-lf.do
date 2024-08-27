*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	clear all 
	
	*Goal: Balancing control and treatment groups 
	*Written by Lorenzo Franchi in April 2023 
	*Day 1 Northwestern Main Causal Inference Workshop
	*Homework 1
	*Underlying Paper: Black, Espin-Sanchez, French, and Litvak, "The Long-term Effect of Health Insurance on Near-Elderly Health and Mortality", 3 American Journal of Health Economics 281-311 (2017) 

	cd "/Users/scunning/Library/CloudStorage/Dropbox-MixtapeConsulting/scott cunningham/Causal Inference Workshops/Stata R Python Materials/2024 materials/day 1 code and solutions"
	
	*Global 
	gl CIWorkshop = "C:\Users\Lorenzo\Dropbox\Work\Causal Inference Workshops\Stata and R Materials"
	gl desktop = "C:\Users\Lorenzo\Desktop\"

	*Local 
	local date = "2023-04-12-lf"
		
	*Load data 
	use "$CIWorkshop\2018-Black-HRS-w11-simplified_2023-04-07a-lf", clear 
	
	*Locals for regressions 
	local two_power_age = "age c.age#c.age"
	local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1 i.CESD_wave_1"
	local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
	local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
	local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"
		
	*Set seed 
	set seed 12354
		
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
	
	*Propensity Score using Logit
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
	
	*Tabulate no_insurance_wave_1
	tab no_insurance_wave_1
	
	*Drop if not wave_1 
	keep ob age gender race *_wave_1
	
	*Can't have negative values in logit for categoricals
	replace CESD_wave_1=CESD_wave_1+8
	
	*Replace age to avoid scale problems 
	replace age=age-51
	
	*Use any insurance 
	qui logit no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'), robust 
	
	*Partial effect 
	margins, dydx(age i.gender i.race i.hh_logincome_quintile_wave_1)
	
	*Information Criteria
	estat ic
	
	*Predict propensity score 
	predict pscore, pr
		
	*Generate weights
	gen ipw=1/pscore if no_insurance_wave_1==1
	replace ipw=1/(1-pscore) if no_insurance_wave_1==0
	
	*Use kernel density for propensities 
	twoway (kdensity pscore if no_insurance_wave_1==1, lpattern(dash) lcol(red)) (kdensity pscore if no_insurance_wave_1==0, lcol(blue)), /*
	*/ legend(label( 1 "Uninsured") label( 2 "Insured")) /*
	*/ xtitle("Probability of being uninsured") /*
	*/ ytitle("Density") /*
	*/ title("Logit Propensity Score") /*
	*/ ytick(0(1)11, grid) ymtick(0(0.5)11, grid) ylab(0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10")/*
	*/ xmtick(0(0.1)1) xlabel(0 "0" 0.2 "0.2" 0.4 "0.4" 0.6 "0.6" 0.8 "0.8" 1 "1")
	
	*Raw check of the balance 
		
	*Make table of summary stats using unweighted controls and treated
	forval k=0/1{
		preserve 
		keep if no_insurance_wave_1==`k'
		replace age=age+51
		sum age gender i.race veteran
		restore 
	}
	
	*Make table of summary stats using weights  
	forval k=0/1{
		preserve 
		keep if no_insurance_wave_1==`k'
		replace age=age+51
		sum age gender i.race veteran [aweight=ipw] //analitic weight: https://www.statalist.org/forums/forum/general-stata-discussion/general/1302852-implementing-propensity-score-weights
		restore 
	}

////////////////////////////////////////////////////////////////////////////////

	*First Summary Stats Table manually built

////////////////////////////////////////////////////////////////////////////////
	
	*Generate single dummies for race 
	tab race, gen(race_)
		
	*Bring back age to its initial value 
	replace age=age+51
		
	*Generate weight equal 1 for unweighted 
	gen unwgtd=1
	
	*Calculate ttest and normalized difference for control and treatment for selected variables
	local weights = "unwgtd ipw"
	local vars = "age gender race_1 race_2 race_3 race_4 log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1"
	
	*Loop
	foreach y of local weights {
	foreach j of local vars {
		
	*Calculate the mean and standard deviation for the insured, and save to local variables
	quietly sum `j' if no_insurance_wave_1==0 [aw=`y']
	local mean_insured = r(mean) //save mean 
	local sd_insured = r(sd) //save sd 
	local sample_insured = r(N) //save sample size 

	*Calculate the mean and standard deviation for the uninsured, and save to local variables
	quietly sum `j' if no_insurance_wave_1==1 [aw=`y']
	local mean_uninsured = r(mean) //save mean 
	local sd_uninsured = r(sd) //save sd
	local sample_uninsured = r(N) //save sample size 

	*Construct the normalized difference, unweighted
	gen ttest_`j'_`y' = (`mean_uninsured' - `mean_insured')/(((`sd_uninsured'^2/`sample_uninsured' + `sd_insured'^2/`sample_insured'))^(1/2))
	gen ndiff_`j'_`y' = (`mean_uninsured' - `mean_insured')/(((`sd_uninsured'^2 + `sd_insured'^2)/2) ^(1/2))
	gen vr_`j'_`y' = (`sd_insured'^2/`sd_uninsured'^2)
	lab var ttest_`j'_`y' "t-test on `j' mean difference, `y'"	
	lab var ndiff_`j'_`y' "Normalized difference on `j', `y'"	
	lab var vr_`j'_`y' "Variance Ratio on `j', `y'"
		}
	}
	
	*Generate summary stats
	foreach y of local weights{
	
	preserve 
	
	*Collapse mean 
	collapse (mean) age gender race_* log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1 ttest_*`y' ndiff_*`y' vr_*`y' [aw=`y'], by(no_insurance_wave_1)
	
	*Cleaning
	replace gender=gender*100 
	replace race_1=race_1*100
	replace race_2=race_2*100
	replace race_3=race_3*100
	replace race_4=race_4*100
	replace diabetes_wave_1=diabetes_wave_1*100
	replace cancer_wave_1=cancer_wave_1*100
	replace employed_ft_wave_1=employed_ft_wave_1*100 
	replace veteran_wave_1=veteran_wave_1*100
	
	*Rename 
	rename no_insurance_wave_1 insured
	
	*Export
	export excel "$desktop\summary_stats_`date'", sheet("`y' no trim", replace) firstrow(variables)
	
	restore 
	
	}	
	
	*Drop ttest values as we need to calculate them later again 
	drop ttest_* ndiff*  vr_*
	
////////////////////////////////////////////////////////////////////////////////

	*First Summary Stats Table using covbal

////////////////////////////////////////////////////////////////////////////////
	
	ssc install covbal
	
	local vars = "no_insurance_wave_1 age gender race_1 race_2 race_3 race_4 log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1"
	
	*Covariates balancing 
	covbal `vars', wt(ipw) format(%9.3f) //without saving. 
	covbal `vars', wt(unwgtd) format(%9.3f) //without saving. Move around 9.3 to 9.4 or 9.5 to obtain more digits.

////////////////////////////////////////////////////////////////////////////////	
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
	
	*Trim persons with propensity to be uninsured outside [0.05, 0.95]
	drop if pscore<0.05 | pscore>0.95
	
	*Sample left 
	tab no_insurance_wave_1 
	
	*IPW after trimming
	*Use kernel density for propensities 
	twoway (kdensity pscore if no_insurance_wave_1==1, lpattern(dash) lcol(red)) (kdensity pscore if no_insurance_wave_1==0, lcol(blue)), /*
	*/ legend(label( 1 "Uninsured") label( 2 "Insured")) /*
	*/ xtitle("Probability of being uninsured") /*
	*/ ytitle("Density") /*
	*/ title("Logit Propensity Score after first trim [0.05,0.95]") /*
	*/ ytick(0(1)11, grid) ymtick(0(0.5)11, grid) ylab(0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10")/*
	*/ xmtick(0(0.1)1) xlabel(0 "0" 0.2 "0.2" 0.4 "0.4" 0.6 "0.6" 0.8 "0.8" 1 "1") /*
	*/ xline(0.05, lcol(black) lpattern(shortdash)) xline(0.95, lcol(black) lpattern(shortdash))
	
	*Drop 
	drop pscore ipw	
	
	*Reduce age 
	replace age=age-51
	
	*Use any insurance 
	qui logit no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'), robust 
	
	*Partial effects 
	margins, dydx(age i.gender i.race i.hh_logincome_quintile_wave_1)
	
	*Information Criteria
	estat ic
	
	*Predict propensity score 
	predict pscore, pr
			
	*Generate weights
	gen ipw=1/pscore if no_insurance_wave_1==1
	replace ipw=1/(1-pscore) if no_insurance_wave_1==0
		
	*Sample left 
	tab no_insurance_wave_1 
	
	*Trim persons with propensity to be uninsured outside [0.04, 0.934]
	drop if pscore<0.04 | pscore>0.934
	
	*Sample left 
	tab no_insurance_wave_1 
		
	*Propensity Score after trimming
	twoway (kdensity pscore if no_insurance_wave_1==1, lpattern(dash) lcol(red)) (kdensity pscore if no_insurance_wave_1==0, lcol(blue)), /*
	*/ legend(label( 1 "Uninsured") label( 2 "Insured")) /*
	*/ xtitle("Probability of being uninsured") /*
	*/ ytitle("Density") /*
	*/ title("Logit Propensity Score after re-estimation and second trim [0.04,0.934]", size(medium)) /*
	*/ ytick(0(1)11, grid) ymtick(0(0.5)11, grid) ylab(0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10")/*
	*/ xmtick(0(0.1)1) xlabel(0 "0" 0.2 "0.2" 0.4 "0.4" 0.6 "0.6" 0.8 "0.8" 1 "1") /*
	*/ xline(0.04, lcol(black) lpattern(shortdash)) xline(0.934, lcol(black) lpattern(shortdash))
	
////////////////////////////////////////////////////////////////////////////////

	*Second Summary Stats Table

////////////////////////////////////////////////////////////////////////////////	
	
	*Calculate ttest and normalized difference for control and treatment groups for selected variables
	local weights = "unwgtd ipw"
	local vars = "age gender race_1 race_2 race_3 race_4 log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1"
	
	*Bring back age to its initial value 
	replace age=age+51
	
	*Loop
	foreach y of local weights {
	foreach j of local vars {
		
	*Calculate the mean and standard deviation for the insured, and save to local variables
	quietly sum `j' if no_insurance_wave_1==0 [aw=`y']
	local mean_insured = r(mean) //save mean 
	local sd_insured = r(sd) //save sd  
	local sample_insured = r(N) //save sample size  

	*Calculate the mean and standard deviation for the uninsured, and save to local variables
	quietly sum `j' if no_insurance_wave_1==1 [aw=`y']
	local mean_uninsured = r(mean) //save mean 
	local sd_uninsured = r(sd) //save sd  
	local sample_uninsured = r(N) //save sample size  

	*Construct ttest on mean difference 
	gen ttest_`j'_`y' = (`mean_uninsured' - `mean_insured')/(((`sd_uninsured'^2/`sample_uninsured' + `sd_insured'^2/`sample_insured'))^(1/2))
	gen ndiff_`j'_`y' = (`mean_uninsured' - `mean_insured')/(((`sd_uninsured'^2 + `sd_insured'^2)/2) ^(1/2))
	gen vr_`j'_`y' = (`sd_insured'^2/`sd_uninsured'^2)
	lab var ttest_`j'_`y' "t-test on `j' mean difference, `y'"	
	lab var ndiff_`j'_`y' "Normalized difference on `j', `y'"	
	lab var vr_`j'_`y' "Variance ratio on `j', `y'"
		
	}
	}
	
	*Generate summary stats
	foreach y of local weights{
	
	preserve 
	
	*Collapse mean 
	collapse (mean) age gender race_* log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1 ttest_*`y' ndiff_*`y' vr_*`y' [aw=`y'], by(no_insurance_wave_1)
	
	*Cleaning
	replace gender=gender*100 
	replace race_1=race_1*100
	replace race_2=race_2*100
	replace race_3=race_3*100
	replace race_4=race_4*100
	replace diabetes_wave_1=diabetes_wave_1*100
	replace cancer_wave_1=cancer_wave_1*100
	replace employed_ft_wave_1=employed_ft_wave_1*100 
	replace veteran_wave_1=veteran_wave_1*100
	
	*Rename 
	rename no_insurance_wave_1 insured
	
	*Export
	export excel "$desktop\summary_stats_`date'", sheet("`y'", replace) firstrow(variables)
	
	restore 
	
	}
	
////////////////////////////////////////////////////////////////////////////////

	*Second Summary Stats Table using covbal

////////////////////////////////////////////////////////////////////////////////
	
	local vars = "no_insurance_wave_1 age gender race_1 race_2 race_3 race_4 log_income_wave_1 diabetes_wave_1 cancer_wave_1 employed_ft_wave_1 veteran_wave_1"
	
	*Covariates balancing 
	covbal `vars', wt(ipw) for(%9.4f) //this saves a dta file with the statistics inside. 


////////////////////////////////////////////////////////////////////////////////	
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	