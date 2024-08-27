*** Code to produce overfitting and ddml simulation figures
*** Uses results precomputed by Achim Ahrens in github.com/aahrens1/ddml_simulations

* Load results, assumes I am in my code subfolder
use "..\Data\sim_temp_all.dta", clear 

keep if obs ==1000  & dgp ==3

label var oracle_bias "Infeasible Oracle"

* Create figures
twoway (hist oracle_bias   , width(0.01) start(-0.4) color(midgreen%30)  ) ///
		(hist ncf_14_bias  , width(0.01) start(-0.4) color(navy%40)) , ///
		scheme(tab1) ///
		legend(order(1 "Oracle" 2 "Neural net") ///
			size(medsmall)) ///
		xlabel(-.3(0.1).3,labsize(medsmall)) xsc(r(-.3 .3)  ) ///
		ylabel(,labsize(medsmall))  ///
		ytitle("",size(medsmall)) ///
		xtitle("Bias",size(medsmall))  
graph export ..\Slides\figures\Stata_OverfitSim.png , replace
graph close
 
twoway (hist oracle_bias   , width(0.01) start(-0.4) color(midgreen%30)  ) ///
		(hist ddml_14_bias  , width(0.01) start(-0.4) color(red%40)) ///
		(hist ncf_14_bias  , width(0.01) start(-0.4) color(navy%60))  , ///
		scheme(tab1) ///
		legend(order(1 "Oracle" 2 "DML + Neural net" 3 "Neural net without sample splitting") ///
			size(medsmall)) ///
		xlabel(-.3(0.1).3,labsize(medsmall)) xsc(r(-.3 .3)  ) ///
		ylabel(,labsize(medsmall))  ///
		ytitle("",size(medsmall)) ///
		xtitle("Bias",size(medsmall))  
graph export ..\Slides\figures\Stata_ddmlSim.png , replace
graph close
