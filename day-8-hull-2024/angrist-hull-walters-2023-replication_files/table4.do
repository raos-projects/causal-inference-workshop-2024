/*************************************************

	HANDBOOK OF THE ECONOMICS OF EDUCATION: 
	METHODS FOR MEASURING SCHOOL EFFECTIVENESS
	
	TABLE 4: COUNTERFACTUAL DESTINY SCHOOLS FOR COMPLIERS  
	
	Before running this file, please make sure your dataset is at the individual level, with one observation per student. The following variables should be available: 
	1. A dummy variable that indicates if an offer was received
	2. A dummy variable that indicates if the student attended a charter school
	3. Covariates  
	4. Dummy variables that capture risk sets (see writeup for a description of risk sets)
	5. Dummy variables that capture attendance at various kinds of schools (e.g. attended
	charter school, attended traditional public school, etc.) 
	
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
	global lottery_offer = "" // dummy that takes a value of 1 if an offer was received for the type of school for which we would like the destinies  
	global risk_sets = "" // variable list of dummies that capture risk sets   
	global school_applied = "" // dummy = 1 if student applied for lottery for the school for which we want destinies 
	global school_attended = "" // dummy = 1 if student attended the school type for which we want destinies 
	global destiny_school_types = "" // a list of variables that contain the types of schools we're interested in 
	global destiny_attended = "" // in the global name, please replace "destiny" with the school type you're interested in, and then provide a dummy variable that takes a value 1 if the student attended this school. So, for example, if the destiny school types are "traditional" and "exam", then the global destiny_school_types should be set to "traditional exam" and there should be one global each for the attendance dummies (i.e. traditional_attended and exam_attended). The tags "traditional" and "exam" must enter the destiny_school_types global in the same way as the corresponding attendance dummies are named.  


	
	* obtain count of destiny school types to create row labels in the results matrix
	local row_count = 0 
	
	foreach school in $destiny_school_types {
		local row_count = `row_count' + 1
	}
	
	local row_count = `row_count' * 2

	use "$data\\$data_file", clear	

	* prepare matrix for results
	matrix results = J(`row_count', 1, .)
	
	
	foreach x in $destiny_school_types { // obtain a local with the rownames to be assigned to the results matrix 
		local row_names "`row_names' `x'"
		local row_names "`row_names' `x'_se"
	}
	
	matrix rownames results = `row_names' // assign row names 


	* define treatment and instrument
	preserve 
		keep if $school_applied == 1
		cap gen D = $school_attended
		cap gen nD = 1 - D
		cap gen Z = $lottery_offer
		
		foreach target in $destiny_school_types {
			gen `target'_nD = `target'_attended * nD
		}
			
		local r = 1
		
		foreach destiny_school in $destiny_school_types {
			ivreg2 `destiny_school'_nD (nD = Z) $risk_sets, robust partial($risk_sets )
			local beta = round(_b[nD], 0.001)
			local se = round(_se[nD], 0.001)
		
			matrix results[`r', 1] = `beta'
			matrix results[`r' + 1, 1] = `se'
				
			local r = `r' + 2
		
		} 
				
		drop D nD *_nD Z
	restore 
			

	matlist results 

 