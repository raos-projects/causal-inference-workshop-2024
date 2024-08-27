/*************************************************

	HANDBOOK OF THE ECONOMICS OF EDUCATION: 
	METHODS FOR MEASURING SCHOOL EFFECTIVENESS
	
	TABLE 2: 2SLS ESTIMATES  
	
	NOTE: This do-file contains code to obtain the OLS and 2SLS estimates 
	reported in table 2 of the handbook chapter. 
	
	Before running this file, please make sure your dataset is at the individual level, with one observation per student. The following variables should be available: 
	1. A dummy variable that indicates if an offer was received
	2. A dummy variable that indicates if the student attended a charter school
	3. Covariates  
	4. Dummy variables that capture risk sets (see writeup for a description of risk sets)
	
	NOTE: please make the necessary sample restrictions before running this file. For example, if you wish to run 
	analysis akin to column (1) of table 2, please restrict the sample to only include observations from the year of 
	application. For columns (2) and (3), include all available years. 
	
	
	To run this file, please follow the steps below: 
	1. Provide the necessary local paths in the "globals for directories" section below
	2. Provide the requested variable names in the "globals for variables" section below
	
	
************************************************/


	clear all 
	
* install necessary packages 
	cap ssc install ivreg2 
	cap ssc install reghdfe
	

* globals for directories (PROVIDE LOCAL PATHS)
	global data = "D:\handbook_chapter\CFX AEJ archive\Data\Analysis data" // path to data file 
	global output = ""  // path where you wish to save the resulting graphs
	global data_file = "analysisfile_middle_lotto 2011 12 15" // name of data file 

* globals for variables (PROVIDE VARIABLE NAMES)
	global lottery_offer = "offer" // dummy = 1 if an offer was received for the type of school for which we would like the destinies
	global treatment = "charteryears" // either a treatment dummy = 1 if a charter was attended, or a numeric variable that captures the number of years of charter school attendance 
	global outcome = "c_state_mrawsc" // outcome variable of interest 
	global risk_sets = "d_*" // variable list of dummies that capture risk sets 
	global cluster =  1 // set to 1 if you have multiple observations per student and would like to cluster standard errors 
	global id = "sasid" // variable that contains individual student-level unique IDs. Only required if you intend to run a regression with multiple years of outcomes per student 

*****************************

	use "$data\\$data_file", clear 
	
	if $cluster == 0 {

		* regression with one observation per student (e.g. with observations from the academic year applied for)
		ivreg2 $outcome ($treatment = $lottery_offer) $covariates $risk_sets, partial ($covariates $risk_sets) robust 
		reghdfe $outcome $treatment $covariates, absorb($risk_sets) vce(robust) 
		
	}
	
	else {
		
		* regression with multiple observations per student 
		ivreg2 $outcome ($treatment = $lottery_offer) $covariates $risk_sets, partial ($covariates $risk_sets) cluster($id) 
		reghdfe $outcome $treatment $covariates, absorb($risk_sets) vce(cluster $id)
	
	}

	
