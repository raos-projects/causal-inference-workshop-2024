********************************************************************************
* name: castle.do
* author: scott cunningham
* description: northwestern example of did
********************************************************************************

clear
capture log close

use "/Users/scunning/Library/CloudStorage/Dropbox-MixtapeConsulting/scott cunningham/Causal Inference Workshops/Stata R Python Materials/2023 materials/Cheng-Hoekstra-castle-doctrine-simplified.dta", clear

*Generate log-variables 
	gen homicide_log=log(homicide)
	
*Drop all states that were treated in 2005, 2007, 2008 and 2009
	drop if treatment_date==2005 | treatment_date==2007 | treatment_date==2008 | treatment_date==2009 
	drop if year==2010
	
*Drop generate post treatment indicator
	gen post = 0
	replace post = 1 if year>=2006 
	
*Generate a dummy variable called treat = 0 if never-treated and 1 if treated in 2006.  
	gen treat=0 
	replace treat=1 if treatment_date==2006 
		
*Estimate treatment effect
	reg homicide_log post##treat, cluster(state)
	reg homicide_log post##treat [aweight=population], cluster(state)
	
	reg homicide_log i.post i.treat i.post#i.treat, cluster(state)
	reg homicide_log i.post i.treat i.post#i.treat [aweight=population], cluster(state) // more like Cheng and Hoekstra with population weights.
	