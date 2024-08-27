*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	clear all 
	
	*Difference in differences: practice
	*Written by Lorenzo Franchi in July 2023 
	*Day 3 Northwestern Main Causal Inference Workshop
	*Homework 3
	*Underlying Paper: Cheng Cheng and Mark Hoekstra, Does Strengthening Self-Defense Law Deter Crime or Escalate Violence?  Evidence from Expansions to Castle Doctrine, Journal of Human Resources 48(3), 821-853 (2013).
		
********************************************************************************		
		
	*Global 
	gl CIWorkshop = "C:\Users\Lorenzo\Dropbox\Work\Causal Inference Workshops (All Years)\Stata R Python Materials\2023 materials\"
	gl desktop = "C:\Users\Lorenzo\Desktop\"

	*Local 
	local date = "2023-06-22-lf"
		
********************************************************************************	
		
	*Load data 
	use "$CIWorkshop\Cheng-Hoekstra-castle-doctrine-simplified.dta", replace 
	
	*Generate log-variables 
	gen homicide_log=log(homicide)
	
	*Drop all states that were treated in 2005, 2007, 2008 and 2009
	drop if treatment_date==2005 | treatment_date==2007 | treatment_date==2008 | treatment_date==2009  | treatment_date==2010
	
	*Drop generate post treatment indicator
	gen post = 0
	replace post = 1 if year>=2006 
	
	*Generate a dummy variable called treat = 0 if never-treated and 1 if treated in 2006.  
	gen treat=0 
	replace treat=1 if treatment_date==2006 
	
	*Order 
	order state state_id year treat treatment_date post 
	keep state state_id year treat treatment_date post homicide_log population
	
	*Estimate treatment effect
	reg homicide_log i.post i.treat i.post#i.treat, cluster(state)
	reg homicide_log i.post i.treat i.post#i.treat [aweight=population], cluster(state) // more like Cheng and Hoekstra with population weights.
	
********************************************************************************	
	
	*Manual estimation 
	
	preserve 
	
		*Generate averages for treated
		egen y11 = mean(homicide_log) if post==1 & treat==1
		egen y10 = mean(homicide_log) if post==0 & treat==1
		
		*Next steps remove the missing values of y11 and y10 (for the control states)
		egen ey11 = max(y11)
		egen ey10 = max(y10)
		
		*Generate averages for control
		egen y01 = mean(homicide_log) if post==1 & treat==0
		egen y00 = mean(homicide_log) if post==0 & treat==0
		
		*Next steps remove the missing values of y11 and y10 (for the treated states)
		egen ey01 = max(y01)
		egen ey00 = max(y00)

		*Generate Difference
		gen did = (ey11 - ey10) - (ey01 - ey00)
		sum did
		
	restore 
	
********************************************************************************	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	*DRDID: Sant'Anna, Pedro H. C., and Jun Zhao. 2020. "Doubly Robust Difference-in-Differences Estimators." Journal of Econometrics 219 (1): 101–22.
	*https://econpapers.repec.org/software/bocbocode/s458977.htm
	ssc install drdid, replace
	ssc install outreg2, replace
	
	*Estimate ATT using drdid using repeated cross-section (RC) estimators
	drdid homicide_log, time(post) treatment(treat) cluster(state_id)

*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
********************************************************************************
	
	*Event study

********************************************************************************
	
	reg homicide_log treat##ib2005.year, cluster(state)
	outreg2 using "$desktop\regression.xls", bdec(10) sdec(10) ctitle("w/out Analytical Weight") replace 
	
	*Delete the txt file from outreg2 
	cap erase "$desktop\regression.txt"
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
	
	*Plot event study coefficients
	
	preserve 
	
	*Import back the coefficients. Plot year FE * treatment
	import excel "$desktop\regression.xlsx", clear 
	
	*Rename 
	rename (A B) (year estimate)
	keep year estimate
	
	*Drop omitted categories and vars we dont use 
	drop in 1/47
	drop in 11/12
	drop in 21/27
	
	*Cleaning 
	replace year=subinstr(year, "1.treat#","",.)
	replace year=subinstr(year, ".year","",.)
	destring year, replace force
	carryforward year if year==., replace 
		
	*Eliminate the parenthesis standard errors are reported with, and the stars
		
		replace estimate=subinstr(estimate, "(", "",.) 
		replace estimate=subinstr(estimate, ")", "",.) 
		replace estimate=subinstr(estimate, "***", "",.) 
		replace estimate=subinstr(estimate, "**", "",.) 			
		replace estimate=subinstr(estimate, "*", "",.) 
	
	*Destring estimates 
	destring estimate, replace force
		
	*Generate parallel separate columns for se and coefficients 
	gen obs=_n
	gen odd=mod(obs,2)
	gen coef=estimate if odd==1
	carryforward coef if coef==., replace
	keep if odd==0
	rename (estimate) (se)
	drop obs odd
	order year 
	
	*Generate confidence intervals 
	gen ci_up=coef+1.96*se
	gen ci_low=coef-1.96*se
	
	*Generate 2005 null value
	expand 2 if year==2004
	sort year 
	
	*Generate year identifier 
	gen id=_n
	replace se=. if id==6
	replace coef=0 if id==6
	replace ci_low=0 if id==6
	replace ci_up=0 if id==6
	replace year=2005 if id==6
	
	*Labels 
	label var ci_up "CI"
	label var ci_low "CI"
	label var coef "ATT"
	label var id "Year"
		
	*Plot 
	twoway scatter coef id, connect(l) msymbol(triangle) lcol(purple) lwidth(medthick) /*
	*/ || scatter ci_low id, connect(l)  lpattern(shortdash_dot) lcol(red) lwidth(medthick) msymbol(none) /*
	*/ || scatter ci_up id, connect(l) lpatter(shortdash_dot) lcol(red) lwidth(medthick) msymbol(none) /*
	*/ yline(0, lcolor(black) lwidth(thin) lpattern(shortdash_dot)) /*
	*/ xline(6.5, lcolor(black) lwidth(thin) lpattern(line)) /*
	*/ xtitle("Year", size(medium)) /*
	*/ xlabel(1 "2000" 2 "2001" 3 "2002" 4 "2003" 5 "2004" 6 "2005" 7 "2006" 8 "2007" 9 "2008" 10 "2009" 11 "2010") /*
	*/ legend(order(1 2) col(2) size(medium)) /*
	*/ ymtick(0.25(0.025)-0.25) /*
	*/ ylabel(0.3 "0.3" 0.2 "0.2" 0.1 "0.1" 0 "0" -0.1 "-0.1" -0.2 "-0.2", grid) /*
	*/ ytitle("Marginal effect", size(medium) height(5)) /*
	*/ title("ATT on Log(Homicide)", size(medium)) subtitle("") 
	graph export "$desktop\graph.png", as(png) replace width(3050) height(1350)   
	restore 
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
		
	*Parallel trends pre and post for controls and treated by year
	preserve 	
		
	forval yearx=2000/2010{
	
		*+*+*+
		di `yearx'
		*+*+*+
		
		*Generate mean for treated
		egen y1_`yearx' = mean(homicide_log) if treat==1 & year==`yearx'
		egen ey1_`yearx' = max(y1_`yearx')
		drop y1* 
		
		*Generate mean for controls 
		egen y0_`yearx' = mean(homicide_log) if treat==0 & year==`yearx'
		egen ey0_`yearx' = max(y0_`yearx')
		drop y0*
								
	}
	
	*Keep only one line for each year*treatment group
	keep state ey1* ey0* 
	duplicates drop state ey1* ey0*, force 
	
	*Rename and reshape. 
	rename (ey1_* ey0_*) (ey1* ey0*)
	reshape long ey1 ey0, i(state) j(year)
	duplicates drop ey1 ey0, force 
	drop state
	
	*Labels 
	lab var ey1 "Treated"
	lab var ey0 "Control"
	
	*Plot 
	twoway scatter ey1 year, connect(l) msymbol(triangle) lcol(purple) lwidth(medthick) /*
	*/ || scatter ey0 year, connect(l)  msymbol(diamond) lcol(red) lwidth(medthick) /*
	*/ yline(0, lcolor(black) lwidth(thin) lpattern(shortdash_dot)) /*
	*/ xline(2005.5, lcolor(black) lwidth(thin) lpattern(line)) /*
	*/ xtitle("Year", size(medium)) /*
	*/ xlabel(2000 "2000" 2001 "2001" 2002 "2002" 2003 "2003" 2004 "2004" 2005 "2005" 2006 "2006" 2007 "2007" 2008 "2008" 2009 "2009" 2010 "2010") /*
	*/ legend(order(1 2) col(2) size(medium)) /*
	*/ ytitle("Average", size(medium) height(5)) /*
	*/ title("Log(Homicide) average for treated and control groups overtime", size(medium)) subtitle("") 
	graph export "$desktop\graph_trend_for_loghomicide_by_group_year.png", as(png) replace width(3050) height(1350)   
	
	restore 
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+