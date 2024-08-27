/*************************************************

	HANDBOOK OF THE ECONOMICS OF EDUCATION: 
	METHODS FOR MEASURING SCHOOL EFFECTIVENESS
	
	
	TABLE 1: BALANCE CHECKS  
	
	This do-file contains code to conduct balance checks, 
	as reported in table 1 of the handbook chapter.  
	
	Before running this file, please make your dataset is at the individual level, with one observation per student. The following variables should be available: 
	1. A dummy variable that indicates if an offer was received  
	2. Covariates  
	3. Dummy variables that capture risk sets 
	4. A dummy variable that takes a value of 1 if the outcome variable is non-missing
	
	
	To run this file, please follow the steps below: 
	1. Provide the necessary local paths in the "globals for directories" section below
	2. Provide the requested variable names in the "globals for variables" section below
	
	
************************************************/


	clear all 
	
* install necessary packages 
	cap ssc install ivreg2 

* globals for directories (PROVIDE LOCAL PATHS)
	global data = "" // path to data file 
	global intermediate = "" // path where you wish to save intermediate data files produced by this do-file
	global output = ""  // path where you wish to save the resulting graphs
	global data_file = "" // name of data file 

* globals for variables (PROVIDE VARIABLE NAMES)
	global lottery_offer = "" // dummy that takes a value of 1 if an offer was received
	global covariates = "" // (e.g. "special_lunch_status female")
	global risk_sets = "" // variable list of dummies that capture risk sets 
	global outcome_non_missing = "" // dummy variable that takes a value of 1 if the outcome variable is non-missing  

	
*********
	
	use "$data\\$data_file", replace
	
	local row_count = 0
	
	foreach var in $covariates {
		local row_count = `row_count' + 2
	}
	
	local row_count = `row_count' + 7

	mat results = J(`row_count',2,.) // matrix to save results 
	local row=1

	local row_names ""
	di "`row_names'"

	foreach x in $covariates {
		local row_names "`row_names' `x'"
		local row_names "`row_names' `x'"
	}

	local row_names "`row_names' N(offered)"
	local row_names "`row_names' p_value"
	local row_names "`row_names' panel_b"
	local row_names "`row_names' beta"
	local row_names "`row_names' se"
	local row_names "`row_names' mean"
	local row_names "`row_names' N"


	mat rownames results = `row_names'


	foreach x in $covariates {
		reg `x' $lottery_offer $risk_sets , r 
		local beta = round(_b[$lottery_offer], 0.001) // these return the balance coefficients
		local se = round(_se[$lottery_offer], 0.001) // SEs of the balance coefficient
		matrix results[`row', 2] = `beta'
		matrix results[`row'+1, 2] = `se'

		sum `x'
		local mean = round(r(mean), 0.001)
		matrix results[`row', 1] = `mean'

		local row=`row'+2
	}

	count if $lottery_offer==1
	matrix results[`row', 2]=`r(N)'

	mvreg $covariates = $lottery_offer $risk_sets 
	test $lottery_offer

	local row = `row'+1
	local p= round(r(p), 0.001)
	matrix results[`row',2]=`p'
	local row = `row'+2

	* check for attrition
	reg $outcome_non_missing $lottery_offer $risk_sets $covariates, r
	local beta = round(_b[$lottery_offer], 0.001) // this is the balance coefficient 
	local se = round(_se[$lottery_offer], 0.001) // this is the SE of the balance coefficinet 
	matrix results[`row', 1] =`beta' 
	matrix results[`row'+1, 1] = `se'
	matrix results[`row'+3, 1] = `e(N)' // estimation sample size 

	sum $lottery_offer
	local mean = round(r(mean), 0.001)
	matrix results[`row'+2, 1] = `mean'
	
	matlist results // print the matrix with the results- this can be put into a tex file using "esttab matrix(results)"
