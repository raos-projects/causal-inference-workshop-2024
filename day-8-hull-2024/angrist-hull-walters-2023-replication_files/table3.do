/*************************************************

	HANDBOOK OF THE ECONOMICS OF EDUCATION: 
	METHODS FOR MEASURING SCHOOL EFFECTIVENESS
	
	TABLE 3: MEAN BASELINE CHARACTERISTICS FOR COMPLIERS (TREATED AND UNTREATED), AND MEAN OUTCOMES FOR ALWAYS-TAKERS AND NEVER-TAKERS 
	
	NOTE: Please make sure your dataset does not contain variables named D or Z. 
	D and Z are defined as variables in the course of this do-file. The analysis will 
	be incorrect if there is already a variable named D which is NOT a dummy for
	treatment, or if there is a variable named Z which is NOT the dummy for an offer. 
	
	Before running this file, please make your dataset is at the individual level, with one observation per student. The following variables should be available: 
	1. A dummy variable that indicates if an offer was received
	2. A dummy variable that indicates if the student attended a charter school
	3. Covariates  
	4. Dummy variables that capture risk sets 
	
	NOTE: please note that the dataset should not have any variables named Z, D, nD, or nZ, as these are assigned in the course of the script. 
	
	
	To run this file, please follow the steps below: 
	1. Provide the necessary local paths in the "globals for directories" section below
	2. Provide the requested variable names in the "globals for variables" section below
	
	
************************************************/


	clear all 
	
* install necessary packages 
	cap ssc install ivreg2 

* globals for directories (PROVIDE LOCAL PATHS)
	global data = "" // path to data file 
	global output = ""  // path where you wish to save the resulting graphs
	global data_file = "" // name of data file 

* globals for variables (PROVIDE VARIABLE NAMES)
	global lottery_offer = "" // dummy that takes a value of 1 if an offer was received 
	global treatment = "" // dummy variable =1 if attended charter 
	global covariates = "" // (e.g. "special_lunch_status female")
	global risk_sets = "" // variable list of dummies that capture risk sets   


****************

	clear all 

	use "$data\\$data_file", replace
	
	*define D, (1-D), Z, and (1-Z) as variables 
	cap gen D = $treatment 
	cap gen Z = $lottery_offer 
	cap gen nD = 1 - D
	cap gen nZ = 1- Z
	
	*create a matrix to store results, and assign row names that capture the covariates being considered 
	local total_rows = 0 
	foreach cov in $covariates { // this loop obtains the number of covariates 
		local total_rows = `total_rows' + 1 
	}
	
	local total_rows = `total_rows' * 2 + 1 // two rows per covariate (one for beta, one for SE), + 1 row to store the share of compliers, always-takers and never-takers 
	
	matrix baseline_cov_means = J(`total_rows', 5, .)
	
	foreach x in $covariates { // obtain a local with the rownames to be assigned to the results matrix 
		local row_names "`row_names' `x'"
		local row_names "`row_names' `x'"
	}

	local row_names "`row_names' share" 
	mat rownames baseline_cov_means = `row_names' // assign the row names 
	

	* Get baseline covariate values
	local r = 1

	foreach x in $covariates {

		* non-treated complier mean
		gen `x'_nD = `x' * nD
		qui	ivreg2 `x'_nD (nD = Z) $risk_sets, partial($risk_sets) robust
		local beta0 = round(_b[nD], 0.001)
		local se0 = round(_se[nD], 0.001)
		matrix baseline_cov_means[`r' ,1] = `beta0'
		matrix baseline_cov_means[`r' + 1, 1] = `se0'

		* treated complier mean
		gen `x'_D = `x' * D
		qui	ivreg2 `x'_D (D = Z) d_*, partial(d_* ) robust
		local beta1 = round(_b[D], 0.001)
		local se1 = round(_se[D], 0.001)
		matrix baseline_cov_means[`r' ,2] = `beta1'
		matrix baseline_cov_means[`r' + 1, 2] = `se1'
	
		matlist baseline_cov_means
		
		
		* pooled complier mean
		preserve
			expand 2, gen(copy) // duplicate the entire sample
			gen Y = `x'* D *(1 - copy) + `x'* nD * copy
			gen T = D*(1 - copy) + nD*copy

			foreach var of varlist T Z $risk_sets {
				gen `var'_0 = `var'*(1-copy)
				gen `var'_1 = `var'*copy
			}

			qui ivreg2 Y (T = Z_0 Z_1) $risk_sets copy, cluster(sasid) partial($risk_sets copy) robust 
			local complier_mean = round(_b[T], 0.001)
			local complier_se = round(_se[T], 0.001)
			matrix baseline_cov_means[`r', 3] = `complier_mean'
			matrix baseline_cov_means[`r'+1, 3] = `complier_se'
		restore

		* Always taker mean
		gen `x'_at = `x' * D * (1-Z)
		gen at_regressor = D *(1-Z)
		qui	reg `x'_at at_regressor $risk_sets, r
		local at_mean = round(_b[at_regressor], 0.001)
		local at_se = round(_se[at_regressor], 0.001)
		matrix baseline_cov_means[`r', 4] = `at_mean'
		matrix baseline_cov_means[`r'+1, 4]  = `at_se'

		* Never taker mean
		gen `x'_nt = `x'*(1-D)*Z
		gen nt_regressor = (1-D)*Z
		qui	reg `x'_nt nt_regressor $risk_sets, r
		local nt_mean = round(_b[nt_regressor], 0.001)
		local nt_se = round(_se[nt_regressor], 0.001)
		matrix baseline_cov_means[`r', 5] = `nt_mean'
		matrix baseline_cov_means[`r'+1, 5]  = `nt_se'


		local r = `r' + 2

		drop at_regressor nt_regressor `x'_at `x'_nt
	}


	* share of compliers
	reg D Z $risk_sets
	local share_c = round(_b[Z], 0.001)
	matrix baseline_cov_means[`total_rows', 3] = `share_c'

	* share of always-takers
	gen at_lhs = D*nZ
	reg at_lhs nZ $risk_sets
	local share_at = round(_b[nZ], 0.001)
	matrix baseline_cov_means[`total_rows', 4] = `share_at'

	* share of never-takers
	gen nt_lhs = nD * Z
	reg nt_lhs Z $risk_sets
	local share_nt = round(_b[Z], 0.001)
	matrix baseline_cov_means[`total_rows', 5] = `share_nt'

	di `share_c' + `share_at' + `share_nt' // should add up to 1 

	matlist baseline_cov_means // print the matrix in Stata 

