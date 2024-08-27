********************************************************************************	
	
	*Adv. Difference in differences: practice
	*Written by Lorenzo Franchi in June 2024
	*Day 7 Northwestern Main Causal Inference Workshop
	*Homework 5
	*Underlying Paper: Cheng Cheng and Mark Hoekstra, Does Strengthening Self-Defense Law Deter Crime or Escalate Violence?  Evidence from Expansions to Castle Doctrine, Journal of Human Resources 48(3), 821-853 (2013).
		
********************************************************************************		
	
	clear all 
	set type double 
	
	*Global 
	gl CIWorkshop = "C:\Users\Lorenzo\Dropbox\Work\Causal Inference Workshops (All Years)\Stata R Python Materials\2023 materials"
	gl desktop = "C:\Users\Lorenzo\Desktop\"

	*Local 
	local date = "2023-06-22-lf"
	
********************************************************************************	
	
	*Load data 
	use "https://github.com/scunning1975/mixtape/raw/master/castle.dta", clear

********************************************************************************	
	
	*Callaway Sant'Anna DiD, 2021 
	* 1) Callaway, Brantly and Sant'Anna, Pedro H. C. 2021.  "Difference-in-Differences with multiple time periods.", Journal of Econometrics, 225(2):200-230.
	* 2) Sant'Anna, Pedro H. C., and Jun Zhao. 2020.  "Doubly Robust Difference-in-Differences Estimators." Journal of Econometrics 219 (1): 101–22.
	
********************************************************************************	
	
	*Install packages
	ssc install csdid, replace
	ssc install drdid, replace
	
	*Generate log-variable
	gen homicide_log=log(homicide)
		
	*The variable "treatment_date" measures the year in which the Stand Your Ground law was implemented in a state.
	*Set treatment_date equal zero if treatment is missing(control group). Groups that are never treated should be coded as zero. Any positive value indicates which year a group was initially treated. Once a group is treated, it always remains treated.
	replace treatment_date=0 if treatment_date==.
	tab treatment_date
	
	*Construct Event-time variable (year-treatment_year). First time of treatment is 2006. 
	gen control_group=1 if treatment_date==0
	gen event_time = year-treatment_date //this is going to be 200x-0 if control, and 200x-200y for treated. 
	tab event_time
	replace event_time=-1 if control_group==1 //set to -1 for control units (event_time=200x-0). 
	
	*We can't have negative years for event-time variable, so since we are working with data from 2000 to 2009, we add 9. 
	sum event_time //min -9, max 5. 
	replace event_time=event_time+9 
	
	*Variable to denote when treatment is active 
	gen treatment_active= (year >= treatment_date)
		
	*Callaway Sant'Anna Group-level DID estimator, Control group: never treated
	csdid homicide_log, ivar(sid) time(year) treatment(treatment_active) gvar(treatment_date) agg(event) asinr 
	estat all 
	
	*Plot Dynamic ATTs, Control Group: Never treated 
	csdid_plot, /*
	*/ xlab(-8(1)5)/*
	*/ xtitle("Time to Treatment", height(5)) /*
	*/ ylabel(-0.6 "-0.6" -.2 "-0.2" 0.2 "0.2" 0.6 "0.6") /*
	*/ ymtick(-0.8(0.1)0.8) /*
	*/ ytitle("Marginal effect", height(5)) /*
	*/ title ("Event-Study Plot: Dynamic ATT") subtitle("Control Group: Never Treated")
	 graph export "/Users/shihaoyan/Downloads/att_csdid_never_treated.png", as(png) replace width(3050) height(1350) 
		
	*Callaway Sant'Anna DID estimator, Control group: Not Yet + Never Treated
	replace control_group = 1 if treatment_date == 0 | treatment_date > 2006
	csdid homicide_log, ivar(sid) time(year) treatment(treatment_active) gvar(treatment_date) agg(event) asinr long2
	estat all 	
	
	*Plot Dynamic ATTs, Control Group: Not Yet Treated
	csdid_plot, /*
	*/ xlab(-8(1)5)/*
	*/ xtitle("Time to Treatment", height(5)) /*
	*/ ylabel(-0.6 "-0.6" -.2 "-0.2" 0.2 "0.2" 0.6 "0.6") /*
	*/ ymtick(-0.8(0.1)0.8) /*
	*/ ytitle("Marginal effect", height(5)) /*
	*/ title ("Event-Study Plot: Dynamic ATT") subtitle("Control Group: Not Yet Treated")
	 graph export "/Users/shihaoyan/Downloads/att_csdid_notyet_treated.png", as(png) replace width(3050) height(1350) 
	
********************************************************************************
	
	*TWFE Regression
	
********************************************************************************
		
	*Install packages
	ssc install reghdfe, replace
	ssc install ftools, replace
		
	*Period 8 is event_time = -1, so we want to have 8 as reference time
	reghdfe homicide_log ib8.event_time, absorb(sid year) cluster(sid)
	
	*Overall ATT 
	//gen post=treatment_active
	reghdfe homicide_log i.treatment_active, absorb(sid year) cluster(sid)
		
********************************************************************************

	*Sun and Abraham, Staggered DiD
	* 1) Sun, Liyang, and Sarah Abraham (2021), Estimating dynamic treatment effects in event studies with heterogeneous treatment effects, Journal of Econometrics 225(2): 175-199.
	
********************************************************************************	
	
	*Install package 	
	ssc install eventstudyinteract, replace
	ssc install avar, replace 
	
	*Restore original values of event_time  
	replace event_time=event_time-9
	 
	*Generate relative time indicators
	sum event_time 
	
	*Lags, drop -1
	forvalues k = `r(min)'(1)-2 {
		
		local k = -`k'
		gen event_time_lag_`k' = event_time == -`k'
		lab var event_time_lag_`k' "-`k'"
	
	}
	
	*Leads
	forvalues k = 0(1)`r(max)' {
		
		gen event_time_lead_`k' = event_time == `k'
		lab var event_time_lead_`k' "`k'"
		
	}
	
	*Generate never treated 
	gen never_treated = treatment_date == 0
	 
	*Replace values in event_time to be positive 
	replace event_time=event_time+9
	
	/*
	*Problems with path and ado files, in case eventstudyinteract does not work. 
	capture mata: mata drop m_calckw()
	capture mata: mata drop m_omega()
	capture mata: mata drop ms_vcvorthog()
	capture mata: mata drop s_vkernel()
	mata: mata mlib index	 
	*/		
		
	*Staggered DiD 
	eventstudyinteract homicide_log event_time_lag_* event_time_lead_*, cohort(event_time) control_cohort(never_treated) absorb(sid year) vce(cluster sid) 
	
	*event study plot
	ssc install coefplot
    matrix C = e(b_iw)
    mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
    matrix C = C \ A'
    matrix list C
	
	matrix new_col = (0, 0)'
	matrix C = C, new_col
	* Get current column names
	local colnames : colnames C

	* Replace the last column name with event_ti~g_1
	local colnames = subinstr("`colnames'", "c1", "-1", .)

	* Apply new column names to matrix C
	matrix colnames C = `colnames'
	matrix C_part1 = C[1..., 1..8]
	matrix C_minus1 = C[1..., colsof(C)]
	matrix C_part2 = C[1..., 9..(colsof(C)-1)]
	matrix C_new = (C_part1, C_minus1, C_part2)
	matrix C = C_new
	matrix list C


	coefplot matrix(C[1]), se(C[2]) vertical lab /*
	*/ xtitle("Years to Treatment", height(5)) /*
	*/ ytitle("Marginal effect", height(5)) ylab(-0.6 "-0.6" -0.4 "-0.4" -0.2 "-0.2" 0 "0" 0.2 "0.2") ymtick(-0.6(0.1)0.2) /*
	*/ xlab(,angle(45)) /*
	*/ title("Event-Study Plot: Dynamic ATT") subtitle("Sun and Abraham") /*
	*/ yline(0, lcol(black) lpattern(dash)) xline(9.5, lcol(red) lpattern(dash))
	graph export "/Users/shihaoyan/Downloads/event_study_plot.png", as(png) replace

********************************************************************************

	*Goodman-Bacon decomposition
	*Goodman-Bacon, Andrew (2021), "Differences-in-differences with variation in treatment timing", Journal of Econometrics 225(2): 254-277

********************************************************************************	
	
	*Install package 
	net install ddtiming, from(https://raw.githubusercontent.com/tgoldring/ddtiming/master)
	
	*Decomposition
// 	areg homicide_log i.year treatment_active, a(sid) cluster(sid) 
// 	ddtiming homicide_log treatment_active, i(sid) t(year) 
	use "https://github.com/scunning1975/mixtape/raw/master/castle.dta", clear
	gen homicide_log=log(homicide)
	gen treatment_active= (year >= treatment_date)
	xtdidregress (homicide_log) (treatment_active), group(sid) time(year)
	estat bdecomp, graph


		
********************************************************************************	 
	 
	 *Stacking Estimator 
	 
********************************************************************************

	*Step 1: Loop through by using only a group and the never treated and assigning it a unique stack_id
	
	*Restore original values of event_time  
	replace event_time=event_time-9
	
	*We are aiming for a panel with event_time -5 to 5

    forval i = 2005(1)2010 {
		
		preserve
		
		*Generate stack_id 
		gen stack_id = `i'

		*Keep if event_time == `i', never-treated, or untreated observations 
		keep if treatment_date == `i' | treatment_date == 0 | year < treatment_date

		* keep rel_year -5 through 5
		keep if event_time >= -5 & event_time <= 5

		save "/Users/shihaoyan/Downloads/stack_`i'", replace
		
		restore
		
	}

	*Step 2: Now append the datasets into one single stacked dataset.
    use "/Users/shihaoyan/Downloads/stack_2005", clear
    cap erase "/Users/shihaoyan/Downloads/stack_2005.dta"
   
	forval i = 2006(1)2010 {
		
	  append using "/Users/shihaoyan/Downloads/stack_`i'"
	  cap erase "/Users/shihaoyan/Downloads/stack_`i'.dta"
	  
	}

	*Step 3: Estimation with dataset interaction fixed effects and relative event time fixed effects, clustering on unique state*dataset identifiers
	replace event_time=event_time+9
	egen state_x_stack_id = group(sid stack_id)
	egen year_x_stack_id = group(year stack_id)
	reghdfe homicide_log ib8.event_time, absorb(state_x_stack_id year_x_stack_id) cluster(state_x_stack_id)	 
	 
********************************************************************************
