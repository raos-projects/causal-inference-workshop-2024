*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	*Regression discontinuity
	*Written by Lorenzo Franchi in July 2023 
	*Day 4 Northwestern Main Causal Inference Workshop
	*Homework 4
	*Underlying Paper: Benjamin Hansen, Punishment and Deterrence: Evidence from Drunk Driving, American Economic Review 105(4), 1581-1617 (2015).

*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
	
	*Install the following packages:
	*ssc install rdrobust, replace
	*ssc install rddensity, replace 
	*ssc install lpdensity, replace 
	*ssc install cmogram, replace 
		
	clear all 
	
	*Globals 
	gl data "C:\Users\Lorenzo\Dropbox\Work\Causal Inference Workshops (All Years)\Stata and R Materials\"
	gl desktop "C:\Users\Lorenzo\Desktop"
	
	*Load data 
	use "$data\2015-Hansen-rounded-DUI-RDD-2023-07-27-lf.dta", clear
	
	*Generate Washington State (WA) DUI threshold. 
	*It is the minimum score of the two BAC tests that determines guilt regarding DUI (or aggravated DUI). 
	replace bac1=bac2 if bac2<bac1 
	rename bac1 BAC 
	drop bac2
	label var BAC "Blood Alcohol Content, Minimum of the two tests"
	
	*Generate DUI flag 
	gen	dui = 0
	replace dui = 1 if BAC>0.080 // DUI threshold. If >0.15 is aggravated DUI.
	
	*Generate recentered BAC and its squared value 
	gen BAC_c = BAC - 0.080 // recentered blood alcohol content variable
	
	*Drop values of BAC outside the threshold
	drop if BAC>0.130 | BAC<0.030
	
	*Histogram of discrete density with zoom at 0.08 (0.05 to 0.11)
	histogram BAC, discrete width(0.001) color(gray) /*
	*/ addplot(pci 0 0.08 20 0.08 , lcol(black)) /*
	*/ xlabel(0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12", labsize(medium)) /*
	*/ xtitle("BAC", size(large) height(5)) /*
	*/ ytitle("Density", size(large) height(5)) /*
	*/ title("BAC Histogram", size(large)) /*
	*/ legend(off) 
	graph export "$desktop\histogram_density_discrete_BAC.png", as(png) width(4840) height(3160) replace 
	
		
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
		
	*Assess manipulation using OLS 
	local outcomes = "male white age car_accident"
    cap erase "$desktop\OLS_covariate_balancing_for_RD.xls"
    cap erase "$desktop\OLS_covariate_balancing_for_RD.txt"
	
	         
	foreach y of local outcomes {
	
	*OLS    
	reg `y' i.dui BAC_c i.dui#c.BAC_c if BAC>=0.03 & BAC<=0.13, robust
	outreg2 using "$desktop\OLS_covariate_balancing_for_RD.xls", bdec(4) sdec(4) ctitle("`y', [0.03, 0.13]") append  
	
	}
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
	
	*Nonparametric means of covariates across the running variable with cmogram.
	local outcomes = "male white age car_accident"
	
	foreach y of local outcomes {
		
	*Rdplot 	
	rdplot `y' BAC if BAC>=0.03 & BAC<=0.13, p(1) h(0.05 0.05) c(0.08) /*
	*/ graph_options(xtitle("BAC", height(5) size(large)) /*
	*/ ytitle("Mean `y'", height(5) size(large)) /*
	*/ title("Regression Discontinuity Estimator", size(large)) /*
	*/ xlabel(0.03 "0.03" 0.05 "0.05" 0.07 "0.07" 0.09 "0.09" 0.11 "0.11" 0.13 "0.13")/* 
	*/ xmtick(0.02(0.01)0.14)) 
	graph export "$desktop\graph_means_linear_fit_`y'.png", as(png) replace width(4840) height(3160)
	
	}

*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
	
	*Replace age with age-21 for regressions 
	replace age=age-21
      
	*Erase previous version
	cap erase "$desktop\OLS_DUI_recidivism.xls"
	cap erase "$desktop\OLS_DUI_recidivism.txt"
	
	*Local for further regressors 
	local regressors = "age male white car_accident"
	
	*Local linear regressions (OLS model)
	reg recidivism i.dui BAC_c i.dui#c.BAC_c `regressors' if BAC>=0.03 & BAC<=0.13 , robust 
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("OLS, Linear BAC, [0.03; 0.13]") append  
	
	*Local linear regression with triangular kernel (OLS model)
	reg recidivism i.dui BAC_c c.BAC_c#c.BAC_c i.dui#c.BAC_c i.dui#c.BAC_c#c.BAC_c `regressors' if BAC>=0.03 & BAC<=0.13, robust
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("OLS, Quadratic BAC, [0.03; 0.13]") append  
	
	*Local linear regression (OLS model) with narrower bandwidth
	reg recidivism i.dui BAC_c i.dui#c.BAC_c `regressors' if BAC>=0.055 & BAC<=0.105 , robust 
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("OLS, Linear BAC, [0.055; 0.105]") append  
	
	*Local linear regression with rectangular kernel (OLS model) with narrower bandwidth
	regress recidivism i.dui BAC_c c.BAC_c#c.BAC_c i.dui#c.BAC_c i.dui#c.BAC_c#c.BAC_c `regressors' if BAC>=0.055 & BAC<=0.105 , robust
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("OLS, Quadratic BAC, [0.055; 0.105]") append  
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	
	
	*Estimate average effect of arrest for DUI on recidivism using rdrobust and triangular kernel 
		
	*Linear on point estimation, and quadratic on bias correction 
	rdrobust recidivism BAC, c(0.080) p(1) h(0.05 0.05) all covs(`regressors') 
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("rdrobust, Linear BAC, [0.03; 0.13]") append  
	
	*Quadratic on point estimation, and cubic on bias correction 
	rdrobust recidivism BAC, p(2) c(0.080) h(0.05 0.05) all covs(`regressors') 
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("rdrobust, Quadratic BAC, [0.03; 0.13]") append  
	
	*Linear on point estimation, and quadratic on bias correction 
	rdrobust recidivism BAC, c(0.080) p(1) h(0.025 0.025) all covs(`regressors') 
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("rdrobust, Linear BAC, [0.055; 0.105]") append  
	
	*Quadratic on point estimation, and cubic on bias correction 
	rdrobust recidivism BAC, p(2) c(0.080) h(0.025 0.025) all covs(`regressors') 
	outreg2 using "$desktop\OLS_DUI_recidivism.xls", bdec(4) sdec(4) ctitle("rdrobust, Quadratic BAC, [0.055; 0.105]") append  

*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
		
	*Rdplot, Linear fit
	rdplot recidivism BAC if BAC>=0.03 & BAC<=0.13, c(0.080) p(1) h(0.05 0.05) all covs(`regressors') /*
	*/ graph_options(xtitle("BAC", height(5) size(large)) /*
	*/ ytitle("Mean Recidivism", height(5) size(large)) /*
	*/ title("Regression Discontinuity Estimator", size(large)) /*
	*/ subtitle("Linear model") /*
	*/ ymtick(0(0.01)0.2) /* 
	*/ ylabel(0 "0.00" 0.04 "0.04" 0.08 "0.08" 0.12 "0.12" 0.16 "0.16" 0.20 "0.20", grid labsize(large)) /*
	*/ xlabel(0.03 "0.03" 0.05 "0.05" 0.07 "0.07" 0.09 "0.09" 0.11 "0.11" 0.13 "0.13", labsize(large))/* 
	*/ xmtick(0.02(0.01)0.14)) 
	graph export "$desktop\graph_recidivism_bac_linear_fit.png", as(png) replace width(4840) height(3160)
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

	*Clean directory of txt files 
	cap erase "$desktop\OLS_DUI_recidivism.txt"
	cap erase "$desktop\OLS_covariate_balancing_for_RD.txt"
	
*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
	

	