********************************************************************************
** Local randomization
** Author: Gonzalo Vazquez-Bare
********************************************************************************
** RDROBUST:  net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
** RDDENSITY: net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
** RDLOCRAND: net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
** RDMULTI:   net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
** RDPOWER:   net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace
********************************************************************************

use "../Datasets/senate.dta", clear

gen double X = demmv 
gen double Y = demvoteshfor2
global covs "presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem dopen"


********************************************************************************
** Window selection using rdwinselect
********************************************************************************

** Initial window of 0.5, window increments of 0.125

rdwinselect X $covs, wmin(0.5) wstep(0.125) nwindows(50) plot approx
global w = 0.75

/* NOTE: option "approx" conducts inference using large sample approximations.
This may not be reliable when windows have few observations but is faster.
Remove "approx" option from syntax to use randomization inference instead
(this slows down calculations) */

* Increasing windows by adding at least 5 observations: symmetric windows

rdwinselect X $covs, wmin(0.5) wobs(5) approx
rdwinselect X $covs, wmin(0.5) wobs(5) approx dropmissing

* Increasing windows by adding at least 5 observations: asymmetric windows

rdwinselect X $covs, wmin(0.5) wobs(5) approx dropmissing wasymmetric

* Initial window with at least 10 observations

rdwinselect X $covs, obsmin(10) wobs(5) approx


********************************************************************************
** Randomization inference using rdrandinf
********************************************************************************

** Difference in means

rdrandinf Y X, wl(-$w) wr($w)

** Kolmogorov-Smirnov

rdrandinf Y X, wl(-$w) wr($w) stat(ksmirnov)

** All statistics

rdrandinf Y X, wl(-$w) wr($w) stat(all)

** Linear parametric adjustment

rdrandinf Y X, wl(-$w) wr($w) p(1)

qui sum Y if abs(X)<=$w & X<0
local m0 = r(mean)
qui sum Y if abs(X)<=$w & X>=0
local m1 = r(mean)

twoway (scatter Y X if abs(X)<0.75, mcolor(navy%30)) ///
       (lfit Y X if abs(X)<0.75 & X<0, lcolor(black))(lfit Y X if abs(X)<0.75 & X>=0, lcolor(black)) ///
	   (pci `m0' -$w `m0' 0, lcolor(black) lpattern(dash))(pci `m1' 0 `m1' $w, lcolor(black) lpattern(dash)), ///
	   xline(0, lpattern(dash) lcolor(black) lwidth(thin)) legend(off)

** Change null hypothesis

rdrandinf Y X, wl(-$w) wr($w) nulltau(9)
	   
** Permutation-based CI

rdrandinf Y X, wl(-$w) wr($w) ci(0.05)

