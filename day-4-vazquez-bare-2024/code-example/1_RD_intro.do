********************************************************************************
** Introduction to RD Designs
** Author: Gonzalo Vazquez-Bare
********************************************************************************
** RDROBUST:  net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
** RDDENSITY: net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
** RDLOCRAND: net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
** RDMULTI:   net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
** RDPOWER:   net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace
********************************************************************************

********************************************************************************
** Example 1 : Head Start
********************************************************************************

use "../Datasets/headstart", clear
gl y mort_age59_related_postHS
gl x povrate60
gl c = 59.1984

** Basic Scatter Plot

scatter $y $x

********************************************************************************
** RD Plots
********************************************************************************

** RD plot

rdplot $y $x, c($c)

** Add title 

rdplot $y $x , c($c) graph_options(title(RD Plot - Head Start) ///
                            ytitle(Child Mortality 5 to 9 years old) ///
                            xtitle(Poverty Rate))
			    
** Evenly-spaced

rdplot $y $x, c($c) binselect(es)
rdplot $y $x, c($c) binselect(esmv)

** Quantile-spaced

rdplot $y $x, c($c) binselect(qs)
rdplot $y $x, c($c) binselect(qsmv)

** Global 2nd order polynomial

rdplot $y $x, c($c) p(2)

** Select manually number of bins

rdplot $y $x, c($c) nbins(10)  

** Add confidence intervals

rdplot $y $x, c($c) nbins(10) ci(95) 

** Add confidence intervals w/shade

rdplot $y $x, c($c) nbins(10) ci(95) shade

** Generate variables

rdplot $y $x, c($c) genvars

** Stored output

ereturn list
