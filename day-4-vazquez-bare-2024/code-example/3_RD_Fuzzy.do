********************************************************************************
** Fuzzy RD
** Author: Gonzalo Vazquez-Bare
********************************************************************************
** RDROBUST:  net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
** RDDENSITY: net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
** RDLOCRAND: net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
** RDMULTI:   net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
** RDPOWER:   net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace
********************************************************************************

use "../Datasets/uruguaycct.dta", clear

describe
tab T D

drop if D==.

********************************************************************************
** Effect on birth weight
********************************************************************************

** First stage

rdplot D X
rdrobust D X

** Intention-to-treat

rdplot Y1 X
rdrobust Y1 X

** Fuzzy RD: average effect on compliers

* rdrobust command

rdrobust Y1 X, fuzzy(D)
global h = e(h_l)
global b = e(b_l)

* By hand

qui rdrobust Y1 X, h($h) b($b)
global itt = e(tau_cl)

qui rdrobust D X, h($h) b($b)
global fs = e(tau_cl)

display "Tau_rd = itt/fs = " $itt/$fs

** Comparison with 2SLS for p=0

rdrobust Y1 X, fuzzy(D) masspoints(off) p(0) kernel(uniform)
ivregress 2sls Y1 (D=T) if abs(X)<=e(h_l)


********************************************************************************
** Effect on employment
********************************************************************************

** Intention-to-treat

rdplot Y2 X
rdrobust Y2 X

** Fuzzy RD: average effect on compliers

rdrobust Y2 X, fuzzy(D)
