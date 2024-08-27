/*************************************************

	HANDBOOK OF THE ECONOMICS OF EDUCATION: 
	METHODS FOR MEASURING SCHOOL EFFECTIVENESS
	
	
	FIGURE 1- HYPOTHESIS TEST: EQUALITY OF DISTRIBUTION OF POTENTIAL OUTCOMES BY RACE
	
	This do-file contains code to run a hypothesis test that the distributions
	of (untreated or treated) potential outcomes for any 2 given racial groups
	are equal.  
	
	NOTES: 
	1. This do-file does not plot the densities presented in figure 1. 
	To produce the plots, please see the file figure1.do.
	
	2. This do-file requires manual installation of the dm79 package for 
	some matrix operations. Please type "search dm79" in your Stata window to 
	obtain further instructions on the installation. 
	
	3. Please make sure your dataset does not have any variables named D or Z- 
	the analysis will be incorrect if there is a variable named D which is not
	a dummy for treatment, or if there is a variable named Z which is not 
	a dummy for an offer. D and Z are defined in the course of this do-file. 
	
	Before running this file, please make sure your dataset has the following variables: 
	1. A variable with standardized test scores (the outcome)
	2. A dummy variable that indicates treatment 
	3. A dummy variable for the instrument (e.g. if an offer was made after the lottery)
	4. A numeric variable that captures grades 
	5. Dummy variables that capture risk sets 
	
	
	To run this file, please follow the steps below: 
	1. Provide the necessary local paths in the "globals for directories" section below
	2. Provide the requested variable names in the "globals for variables" section below
	
	
************************************************/


	clear all 
	

* globals for directories (PROVIDE LOCAL PATHS)
	global data = "D:\handbook_chapter\nidhaan\replication_runthrough" // path to data file 
	global intermediate = "D:\handbook_chapter\nidhaan\replication_runthrough" // path where you wish to save intermediate data files produced by this do-file
	global output = "D:\handbook_chapter\nidhaan\replication_runthrough"  // path where you wish to save the resulting graphs
	global data_file = "figure1_test" // name of data file 

* globals for variables (PROVIDE VARIABLE NAMES)
	global race1 = "blackbaseline" // a dummy var for the first racial group 
	global race2 = "whitebaseline" // a dummy var for the second racial group  
	global treatment = "D" // treatment dummy (= 1 if treatment received)
	global lottery_offer = "offer" // lottery instrument dummy (= 1 if offer received)
	global outcome = "c_state_mrawsc" // outcome variable (usually standardiszed test scores)
	global grade_var = "grade" // numeric variable that captures grades
	global grade = 7 // grade for which you wish to compare the distributions of potential outcomes 
	global covariates = "femalebaseline blackbaseline hispbaseline asianbaseline otherracebaseline spedbaseline lepbaseline foodfredbaseline fem_minbaseline yobdum* yeardum* baselinedum* baseline_m baseline_e_adjusted missing_english_baseline" // (e.g. "special_lunch_status female")
	global risk_sets = "d_*" // variable list of dummies that capture risk sets 
	
	
************************ 

/***********************************************

We define a program that takes on 5 arguments: 
1. outcome (argument `1')
2. grade (argument `2')
3. dummy variable for racial group 1 
4. dummy variable for racial group 2 
4. Number of bootstrap samples to be obtained 

***********************************************/


program define dist_test_across_race

	clear all
	set seed 7687

********* SETUP STEPS BEGIN HERE ***********************************************

	use "$data\\$data_file", replace
	
	* generate necessary variables 
		* (1) Y, outcome of interest
			cap gen Y = `1' // `1' refers to the first argument in our program, which will typically be a math score

		* (2) D, treatment dummy (enrolled in the year of application)
			cap gen D = $treatment
			gen D0 = 1 - D

		* (3) Z, instrument
			cap gen Z = $lottery_offer 


	* keep only the required grade
		keep if $grade_var == `2'
		

********SETUP ENDS *************************************************************

********ESTIMATION STEPS BEGIN HERE ********************************************

	* Select y values for estimating densities (i.e. create nodes)
		local nvals=50
		sum Y, detail
		local ymin = r(min)
		local ymax = r(max)
		local yinc = (`ymax' - `ymin')/`nvals'
		local y_start = `ymin' + `yinc'
		local y = `y_start'
		
	* Create matrices to store relevant results
		matrix diff_y1 = J(`nvals' - 1, 1, .)
		matrix diff_y0 = J(`nvals' - 1, 1, .) // these store the differences between racial groups at different y (i.e. outcome) nodes for the full sample

		matrix diff_y0_abs = J(`nvals' - 1, 1, .)
		matrix diff_y1_abs = J(`nvals' - 1, 1, .) // these two store the absolute value of the differences, needed for the full-sample test statistic

	* obtain first stage results, by race
		qui reg D Z $covariates $risk_sets if `3' == 1
		predict D_first_stage_b

		qui reg D0 Z $covariates $risk_sets if `3' == 1
		predict D0_first_stage_b

		qui reg D Z $covariates $risk_sets if `4' == 1
		predict D_first_stage_w

		qui reg D0 Z $covariates $risk_sets if `4' == 1
		predict D0_first_stage_w

	* estimate Y0 and Y1 CDFs
		local y = `y_start'
		local r = 1
		local count = 1

		while `y' < `ymax' {

			gen indicator_Y = (Y <= `y')
			gen Y1 = indicator_Y * D
			gen Y0 = indicator_Y * D0 // 
			
			
			* get CDFs for students of first racial group 
			preserve
				qui	keep if `3' == 1
				
				* get Y1 distribution
				qui reg Y1 D_first_stage_b $covariates $risk_sets
				local beta_y1_b = _b[D_first_stage_b]

				* get Y0 distribution
				qui reg Y0 D0_first_stage_b $covariates $risk_sets
				local beta_y0_b = _b[D0_first_stage_b]

			restore

			* get CDFs for students of second racial group 

			preserve

				qui keep if `4' == 1
				* get Y1 distribution
				qui reg Y1 D_first_stage_w $covariates $risk_sets
				local beta_y1_w = _b[D_first_stage_w]

				* get Y0 distribution
				qui reg Y0 D0_first_stage_w $covariates $risk_sets
				local beta_y0_w = _b[D0_first_stage_w]

			restore

			* store the various differences/magnitudes in the relevant matrices
			local diff_y1 = `beta_y1_b' - `beta_y1_w'
			matrix diff_y1[`r', 1] = `diff_y1'

			local diff_y0 = `beta_y0_b' - `beta_y0_w'
			matrix diff_y0[`r', 1] = `diff_y0'

			local diff_y1_abs = abs(`diff_y1')
			matrix diff_y1_abs[`r', 1] = `diff_y1_abs'

			local diff_y0_abs = abs(`diff_y0')
			matrix diff_y0_abs[`r', 1] = `diff_y0_abs'

			di `count'
			local ++r
			local ++count
			local y = `y' + `yinc '
			drop indicator_Y Y0 Y1
		}

	* obtain the full-sample statistic (given by the max. difference (in absolute values))
		mata: st_local("full_sample_stat_y0", strofreal(max(st_matrix("diff_y0_abs"))))
		mata: st_local("full_sample_stat_y1", strofreal(max(st_matrix("diff_y1_abs"))))

	* bootstrap computations
		local greater_than_full_y0 = 0
		local greater_than_full_y1 = 0 // these 2 locals will capture the number of bootstrap samples that yield sample statistics greater than the corresponding full sample statistic

		local b = 1 // this local will keep track of bootstrap progress (no. of bootstrap samples already taken)

		while `b' <= `5' { 

			* create matrices to store differences
			matrix boot_diff_y1 = J(`nvals' - 1, 1, .)
			matrix boot_diff_y0 = J(`nvals' - 1, 1, .) // these store the differences between black and white students at different y nodes

			matrix boot_y1_b = J(`nvals' - 1, 1, .)
			matrix boot_y0_b = J(`nvals' - 1, 1, .)

			matrix boot_y1_w = J(`nvals' - 1, 1, .)
			matrix boot_y0_w = J(`nvals' - 1, 1, .)

			* generate and assign the weights
			sort sasid grade, stable
			gen w = -log(runiform())

			local y = `y_start'
			local r = 1
			local count = 1

			* Obtain first stage results, by race (applying weights)
			cap qui reg D Z $covariates $risk_sets [aw = w] if `3' == 1
			predict D_first_stage_b_bootstrap

			cap qui reg D0 Z $covariates $risk_sets [aw = w] if `3' == 1
			predict D0_first_stage_b_bootstrap

			cap qui reg D Z $covariates $risk_sets [aw = w] if `4' == 1
			predict D_first_stage_w_bootstrap

			cap qui reg D0 Z $covariates $risk_sets [aw = w] if `4' == 1
			predict D0_first_stage_w_bootstrap
			
			while `y' < `ymax' {

				gen indicator_Y = (Y <= `y')
				gen Y0 = indicator_Y*D0
				gen Y1 = indicator_Y*D

				* get CDFs for students from first racial group 
				preserve

					qui keep if `3' == 1
					* get Y1 distribution
					cap qui reg Y1 D_first_stage_b_bootstrap $covariates $risk_sets [aw = w]
					local boot_beta_y1_b = _b[D_first_stage_b_bootstrap]

					* get Y0 distribution
					cap qui reg Y0 D0_first_stage_b_bootstrap $covariates $risk_sets [aw = w]
					local boot_beta_y0_b = _b[D0_first_stage_b_bootstrap]

				restore

			* get CDFs for students from second racial group 

				preserve
				
					qui keep if `4' == 1
					
					* get Y1 distribution
					cap qui reg Y1 D_first_stage_w_bootstrap $covariates $risk_sets [aw = w]
					local boot_beta_y1_w = _b[D_first_stage_w_bootstrap]

					* get Y0 distribution
					cap qui reg Y0 D0_first_stage_w_bootstrap $covariates $risk_sets [aw = w]
					local boot_beta_y0_w = _b[D0_first_stage_w_bootstrap]

				restore

				local boot_diff_y0 = `boot_beta_y0_b' - `boot_beta_y0_w'
				matrix boot_diff_y0[`r', 1] = `boot_diff_y0'
				matrix boot_y0_b[`r', 1] = `boot_beta_y0_b'
				matrix boot_y0_w[`r', 1] = `boot_beta_y0_w'

				local boot_diff_y1 = `boot_beta_y1_b' - `boot_beta_y1_w'
				matrix boot_diff_y1[`r', 1] = `boot_diff_y1'
				matrix boot_y1_b[`r', 1] = `boot_beta_y1_b'
				matrix boot_y1_w[`r', 1] = `boot_beta_y1_w'

				* preapre for the next iteration with the next increment of the y node
				drop indicator_Y Y1 Y0
				local ++count
				local ++r
				local y = `y' + `yinc'
				di in red "count: `count', b = `b'"

			}
			* obtain re-centered differences using the full sample stat
			matrix recentered_y0 = boot_diff_y0 -  diff_y0
			matrix recentered_y1 = boot_diff_y1 -  diff_y1

			* obtain max. value of the recentered differences
			matewmf recentered_y0 abs_recentered_y0, f(abs) // requires manual installation of the "dm79" package - it applies the absolute value function in an element-wise fashion to the first argument and overwrites with a new matrix, given by the second argument
			matewmf recentered_y1 abs_recentered_y1, f(abs)

			* obtain the boostrap stat as the max. of the abs values of the recentered differences
			mata: st_local("bootstrap_sample_stat_y0", strofreal(max(st_matrix("abs_recentered_y0")))) // obtains the bootstrap sample stat as a local
			mata: st_local("bootstrap_sample_stat_y1", strofreal(max(st_matrix("abs_recentered_y1")))) // obtains the bootstrap sample stat as a local

			* record if the bootstrap sample stats are greater than the full sample stat
			if `bootstrap_sample_stat_y0' > `full_sample_stat_y0' {
				local greater_than_full_y0 = `greater_than_full_y0' + 1
			}

			if `bootstrap_sample_stat_y1' > `full_sample_stat_y1' {
				local greater_than_full_y1 = `greater_than_full_y1' + 1
			}

			* prepare for the next iteration of the bootstrap loop
			local ++b
			drop w
			drop D0_first_stage_w_bootstrap D0_first_stage_b_bootstrap D_first_stage_w_bootstrap D_first_stage_b_bootstrap

	}

	* compute the p-values
		local p_value_y0 = `greater_than_full_y0'/`5' // p-value for equality of distributions of the untreated potential outcome 
		local p_value_y1 = `greater_than_full_y1'/`5' // p-value for equality of distributions of the treated potential outcome 

********* ESTIMATION ENDS ******************************************************

	di "Y0 p-value: `p_value_y0'" 
	di "Y1 p-value: `p_value_y1'" // 


end

* run the user-defined program for the outcome, grade, and racial groups defined in the globals at the start 
dist_test_across_race $outcome $grade $race1 $race2 500 // this last line runs the program for the specified outcome variable, with 500 bootstrap samples. 



	
	