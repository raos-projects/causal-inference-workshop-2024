*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	clear all 
	
	*Balance control and treatment groups and estimate health insurance effect of mortality over time.
	*Written by Lorenzo Franchi in May 2023 
	*Day 2 Northwestern Main Causal Inference Workshop
	*Homework 2
	*Underlying Paper: Black, Espin-Sanchez, French, and Litvak, "The Long-term Effect of Health Insurance on Near-Elderly Health and Mortality", 3 American Journal of Health Economics 281-311 (2017) 
		
	*Global 
	gl CIWorkshop = "C:\Users\Lorenzo\Dropbox\Work\Causal Inference Workshops (All Years)\Stata and R Materials"
	gl desktop = "C:\Users\Lorenzo\Desktop\"

	*Local 
	local date = "2023-06-20-lf"
		
	*Load data 
	use "$CIWorkshop\2018-Black-HRS-w11-simplified_2023-04-07a-lf", clear 
	
	*Locals for regressions 
	local two_power_age = "age c.age#c.age"
	local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
	local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
	local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
	local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"
		
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
	
	*Balancing using different methods
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
	
	*Replace age to avoid scale problems 
	replace age=age-51 
	/*		
********************************************************************************

	*Propensity Score Matching 

********************************************************************************	
	
	*Psmatch2 vs teffects: 1-to-1 matching 
	
	*Use ties in psmatch2: Abadie, A., & Imbens, G. W. (2006). Large Sample Properties of Matching Estimators for Average Treatment Effects. Econometrica, 74(1), 235–267. https://www.stata.com/statalist/archive/2014-03/msg00088.html
	
	*Use ai standard errors in psmatch2, Abadie, A., & Imbens, G. W. (2006)
	
	*Rubust AI standard errors is default in teffects, must specify in psmatch2 with command ai(*number of matches)
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+PSMATCH2*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
	
	*Set seed 
	set seed 12354
	
	*Psmatch2: if option ate is inserted, psmatch2 generates weights for ATE. If nothing is specified, it computes weights for ATT.
	psmatch2 no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'), logit ai(1) outcome(death_by_wave_2) n(1) ties ate 
	
	*Rename matching weights 
	rename (_weight _treated) (matching_wg matching_treated) 
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+END OF PSMATCH2*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*	
		
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+TEFFECTS PSMATCH*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	*Teffects psmatch for treated 
	set seed 12354
	teffects psmatch (death_by_wave_2) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'), logit), nolog atet gen(psm_match) vce(robust)
		
	*Teffects psmatch for average treatment effect 
	set seed 12354
	teffects psmatch (death_by_wave_2) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'), logit), nolog ate vce(robust)
		
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+END OF TEFFECTS*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*	

*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+TEFFECTS NNMATCH*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
	
	*Teffects nnmatch for treated, and generate matches.  
	set seed 12354
	teffects nnmatch (death_by_wave_2 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), nolog atet gen(nn_match) vce(robust)
		
	*Teffects nnmatch for average treatment effect 
	set seed 12354
	teffects nnmatch (death_by_wave_2 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), nolog ate vce(robust)
		
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*TEFFECTS IPW*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

	*Teffects ipw for treated 
	set seed 12354
	teffects ipw (death_by_wave_2) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), atet nolog vce(robust)
		
	*Teffects ipw for average treatment effect 
	set seed 12354
	teffects ipw (death_by_wave_2) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), ate nolog vce(robust)
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*TEFFECTS IPW*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*

	*Teffects ipw for treated 
	set seed 12354
	teffects ipwra (death_by_wave_2) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), atet nolog vce(robust)
		
	*Teffects ipw for average treatment effect 
	set seed 12354
	teffects ipwra (death_by_wave_2) (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), ate nolog vce(robust)
		
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+END OF IPW*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
	
////////////////////////////////////////////////////////////////////////////////

	*/	
	*Locals for regressions 
	local two_power_age = "age c.age#c.age"
	local comorbidities = "i.diabetes_wave_1 i.cancer_wave_1 i.high_blood_pressure_wave_1 i.lung_disease_wave_1 i.heart_disease_wave_1 i.stroke_wave_1 i.psychiatric_disease_wave_1 i.arthritis_wave_1 i.ulcer_wave_1"
	local money = "i.hh_logincome_quintile_wave_1 i.hh_earning_quintile_wave_1"
	local labor = "i.not_in_laborforce_wave_1 i.partly_retired_wave_1 i.fully_retired_wave_1 i.unemployed_wave_1 i.employed_pt_wave_1 i.employed_ft_wave_1 i.veteran_wave_1"
	local other = "i.selfreported_health_wave_1 i.mobility_limit_wave_1 i.years_schooling_wave_1 i.household_size_wave_1"	
			
	*Erase previous files 
	cap erase "$desktop\\ATT_psmatch.xls"
	cap erase "$desktop\\ATT_nnmatch.xls"
	cap erase "$desktop\\ATT_ipw.xls"
			
	*Calculate ATT over time 
			
	forval y=2/11{
				
*+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+*+*+
	
	di "***************************ATET_wave_`y'********************************"
	
	di "********************Teffects psmatch for ATT****************************"
	
	*Teffects psmatch for ATT
	set seed 12354
	teffects psmatch (death_by_wave_`y') (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age'), logit), atet vce(robust)
		
	outreg2 using "$desktop\\ATT_psmatch.xls", excel append ctitle("wave`y'") noaster e(n1 vcetype treated cmd subcmd stat)
	
	di "*******************Teffects nnmatch for ATT****************************"
	
	*Teffects nnmatch for average treatment effect 
	set seed 12354
	teffects nnmatch (death_by_wave_`y' `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')) (no_insurance_wave_1), atet vce(robust)
	
	outreg2 using "$desktop\\ATT_nnmatch.xls", excel append ctitle("wave`y'") noaster e(n1 vcetype treated cmd subcmd stat)
		
	di "*********************Teffects IPW for ATT******************************"	
	
	*Teffects ipw for treated 
	set seed 12354
	teffects ipw (death_by_wave_`y') (no_insurance_wave_1 `two_power_age' i.gender i.race `money' `labor' `comorbidities' `other' i.gender#i.race i.gender#c.(`two_power_age') i.race#c.(`two_power_age')), atet nolog vce(robust)	
		
	outreg2 using "$desktop\\ATT_ipw.xls", excel append ctitle("ipw wave`y'") noaster e(n1 vcetype treated cmd subcmd stat)			
		
	}
	
	cap erase "$desktop\\ATT_psmatch.txt"
	cap erase "$desktop\\ATT_nnmatch.txt"
	cap erase "$desktop\\ATT_ipw.txt"
	
*+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+*+*+
	
	*Change the excel file from xls to xlsx. For IPW, delete logit coefficients and simplify to just have treatment effect, like in psmatch and nnmatch. 

*+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+**+*+*+*+*+	
	
	*Import results and plot 
	local methods = "psmatch nnmatch ipw"
	foreach k of local methods {
	import excel "$desktop\ATT_`k'.xlsx", sheet("Sheet1") clear
	
	*Rename variables 
	rename (B C D E F G H I J K) (wave2 wave3 wave4 wave5 wave6 wave7 wave8 wave9 wave10 wave11)
	
	*Drop outreg2 lines we dont need
	drop in 1/3
	drop in 3/10
	
	*Drop first column 
	drop A
	
	*Eliminate the parenthesis standard errors are reported with
	forval y=2/11{
		
		replace wave`y'=subinstr(wave`y', "(", "",.) if _n==2
		replace wave`y'=subinstr(wave`y', ")", "",.) if _n==2
					
	}
	
	*Destring treatment effects 
	destring wave*, replace force
	
	*Generate a separate standard error variable 
	gen id=_n
	reshape long wave, i(id) j(wave_number)
	
	*Put standard error aside of ATET 
	sort wave_number id
	gen std=.
	by wave_number id: replace std=wave if id==2
	gsort wave_number -id
	carryforward std if std==., replace
	drop if id==2
	drop id
	
	*Calculate CI 
	gen CI_low=wave-1.96*std 
	gen CI_top=wave+1.96*std
	
	*Labels 
	rename (wave) (ATET_`k')
	label var ATET_`k' "ATT"
	label var CI_low "CI"
	
	*Plot 
	twoway scatter ATET_`k' wave_number, connect(l) msymbol(none) lcol(purple) lwidth(medthick) /*
	*/ || scatter CI_low wave_number, connect(l) lpattern(shortdash) lcol(red) lwidth(medthick) msymbol(none) /*
	*/ || scatter CI_top wave_number, connect(l) lpatter(shortdash) lcol(red) lwidth(medthick) msymbol(none) /*
	*/ yline(0, lcolor(black) lwidth(thin) lpattern(shortdash_dot)) /*
	*/ xtitle("Wave", size(medium)) /*
	*/ xlabel(2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11") /*
	*/ legend(order(1 2) col(2) size(medium)) /*
	*/ ytitle("Treatment effect", size(medium) height(5)) /*
	*/ ymtick(-0.05(0.01)0.15) /*
	*/ ylabel(-0.05 "-0.05" 0 "0" 0.05 "0.05" 0.1 "0.10" 0.15 "0.15", grid) /*
	*/ title("ATT, `k'", size(medium)) subtitle("") 
	graph export "$desktop\`k'.png", as(png) replace width(3050) height(1350)   
	}
	
	
	
	