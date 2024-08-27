********************************************************************************
** RD estimation and inference
** Author: Gonzalo Vazquez-Bare
********************************************************************************
** RDROBUST:  net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
** RDDENSITY: net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
** RDLOCRAND: net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
** RDMULTI:   net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
** RDPOWER:   net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace
********************************************************************************

use "../Datasets/headstart.dta", clear

global Y mort_age59_related_postHS
gen double X = povrate60 - 59.1984
gen double D = X>=0 if X!=.

global covs60 "census1960_pop census1960_pctsch1417 census1960_pctsch534 census1960_pctsch25plus census1960_pop1417 census1960_pop534 census1960_pop25plus census1960_pcturban census1960_pctblack" 
global placebo "mort_age59_injury_postHS mort_age59_related_preHS"

describe $Y povrate60 $covs60 $placebo
sum $Y povrate60 $covs60 $placebo


********************************************************************************
** Visual representation
********************************************************************************

scatter $Y X

rdplot $Y X
rdplot $Y X, p(1)
rdplot $Y X, p(2)


********************************************************************************
** Parametric estimation
********************************************************************************

** Linear

reg $Y i.D##c.X, vce(robust)

** Quadratic 

reg $Y i.D##c.X##c.X, vce(robust)

** Quadratic within ad-hoc bandwidth

reg $Y i.D##c.X##c.X if abs(X)<=9, vce(robust)


********************************************************************************
** Nonparametric estimation with robust bias-corrected inference
********************************************************************************

** rdrobust

rdrobust $Y X
rdrobust $Y X, p(0)
rdrobust $Y X, kernel(uniform)
rdrobust $Y X, all

** Incorporating covariates

rdrobust $Y X, covs($covs60)

** rdplot for TE

rdbwselect $Y X
local h = e(h_mserd)
rdplot $Y X, h(`h') p(1) kernel(triangular)
rdplot $Y X if abs(X)<=`h', h(`h') p(1) kernel(triangular)

** rdrobust by hand

rdbwselect $Y X
global h = e(h_mserd)

gen K = (1-abs(X/$h))*(abs(X)<=$h) // Triangular kernel

reg $Y X if D==1 [w=K], vce(robust)
local b1 = _b[_cons]
reg $Y X if D==0 [w=K], vce(robust)
local b0 = _b[_cons]

di "Estimated TE = " `b1' - `b0'

** Regression using kernel weights

reg $Y i.D##c.X [w=K], vce(robust)

* Compare with rdrobust

rdrobust $Y X

** Alternative bandwidth choices

rdbwselect $Y X, all

rdrobust $Y X, bwselect(msetwo)

********************************************************************************
** Density test
********************************************************************************

histogram X, xline(0)
rddensity X, plot


********************************************************************************
** Placebo tests
********************************************************************************

foreach var of varlist $placebo{
	rdrobust `var' X
}

