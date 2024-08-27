********************************************************************************
** RD with discrete running variable
** Author: Gonzalo Vazquez-Bare
********************************************************************************
** RDROBUST:  net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
** RDDENSITY: net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
** RDLOCRAND: net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
** RDMULTI:   net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
** RDPOWER:   net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace
********************************************************************************

use "../Datasets/education.dta", clear
sum

gen campus = loc_campus1 + 2*loc_campus2 + 3*loc_campus3

drop if nextGPA_nonorm == .

codebook X
tab X
tab X if abs(X)<=0.1
duplicates report X

twoway (histogram X if X<0, disc freq color(blue)) ///
	   (histogram X if X>=0, disc freq color(red)), graphregion(color(white)) legend(off) ///
	    xtitle(Score) ytitle(Number of Observations)

twoway (histogram X if X<0 & X>=-1, disc freq color(blue)) ///
	   (histogram X if X>=0 & X<=1, disc freq color(red)), graphregion(color(white)) legend(off) ///
	    xtitle(Score) ytitle(Number of Observations)

scatter nextGPA_nonorm X if abs(X)<=0.1
		
		
********************************************************************************
** Continuity-based falsification analysis
********************************************************************************

** Manipulation test

twoway (histogram X if X<0, width(0.1) freq color(blue) xline(0)) ///
	   (histogram X if X>=0, width(0.1) freq color(red)), graphregion(color(white)) legend(off) ///
	   xtitle(Score) ytitle(Number of Observations)	   
	   
rddensity X, plot

** Placebo effects

rdplot hsgrade_pct X
rdrobust hsgrade_pct X

rdplot age_at_entry X
rdrobust age_at_entry X

rdplot totcredits_year1 X
rdrobust totcredits_year1 X

********************************************************************************
** Continuity-based TE estimation
********************************************************************************

rdplot nextGPA_nonorm X
rdrobust nextGPA_nonorm X

** rdrobust on the collapsed data

preserve

collapse (mean) nextGPA_nonorm totcredits_year1 hsgrade_pct age_at_entry, by(X)

rdrobust totcredits_year1 X
rdrobust hsgrade_pct X
rdrobust age_at_entry X

rdplot nextGPA_nonorm X
rdrobust nextGPA_nonorm X

restore


********************************************************************************
** Local-randomization falsification analysis
********************************************************************************

** Manipulation test

tab X if abs(X)<=0.05

twoway (histogram X if X<0 & X>=-.5, disc freq color(blue)) ///
	   (histogram X if X>=0 & X<=.5, disc freq color(red)), graphregion(color(white)) legend(off) ///
	    xtitle(Score) ytitle(Number of Observations)

bitesti 228 77 0.5

** Placebo outcomes

rdrandinf hsgrade_pct X , seed(50) wl(-.00005) wr(.01)
rdrandinf age_at_entry X, seed(50) wl(-.00005) wr(.01)
rdrandinf totcredits_year1 X, seed(50) wl(-.00005) wr(.01)

********************************************************************************
** Local-randomization window selection and TE estimation
********************************************************************************

global covariates "hsgrade_pct totcredits_year1 age_at_entry male bpl_north_america"
rdwinselect X $covariates, wmasspoints

global wl = r(wmin_left) - 0.001
global wr = r(wmin_right) + 0.001

** rdrandinf

rdrandinf nextGPA_nonorm X, wl($wl) wr($wr) 

