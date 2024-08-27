*** Code to produce model choice simulation figures
*** Uses results precomputed by Achim Ahrens in github.com/aahrens1/ddml_simulations

* Load results, assumes I am in my code subfolder
use "..\Data\sim_temp_all.dta", clear 

keep if obs ==1000  & dgp0 == 5

label var oracle_bias "Infeasible Oracle"

* Create figures
twoway (hist oracle_bias   , width(0.01) start(-0.4) color(midgreen%30)  ) ///
		(hist ddml_2_bias  , width(0.01) start(-0.4) color(red%40)) ///
		(hist ddml_14_bias  , width(0.01) start(-0.4) color(navy%60))  , ///
		scheme(tab1) ///
		legend(order(1 "Oracle" 2 "CV-Lasso" 3 "Neural net")  size(medsmall)) ///
		xlabel(-.2(0.1).3,labsize(medsmall)) xsc(r(-.4 .2)  ) ///
		ylabel(,labsize(medsmall))  ///
		ytitle("",size(medsmall)) ///
		xtitle("Bias",size(medsmall))  
graph export ..\Slides\figures\Stata_learnchoice1.png , replace
graph close

use "..\Data\sim_temp_all.dta", clear

keep if dgp0==3 & obs ==1000

label var oracle_bias "Infeasible Oracle"
 
twoway (hist oracle_bias   , width(0.01) start(-0.2) color(midgreen%30)  ) ///
		(hist ddml_2_bias  , width(0.01) start(-0.2) color(red%40)) ///
		(hist ddml_14_bias  , width(0.01) start(-0.2) color(navy%60))  , ///
		scheme(tab1) ///
		legend(order(1 "Oracle" 2 "CV-Lasso" 3 "Neural net")  size(medsmall)) ///
		xlabel(-.2(0.1).3,labsize(medsmall)) xsc(r(-.2 .3)  ) ///
		ylabel(,labsize(medsmall))  ///
		ytitle("",size(medsmall)) ///
		xtitle("Bias",size(medsmall))  
graph export ..\Slides\figures\Stata_learnchoice2.png , replace
graph close
