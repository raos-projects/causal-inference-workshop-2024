******************************************************************************** 
** Power calculations in RD designs
** Author: Gonzalo Vazquez-Bare
********************************************************************************

use "../Datasets/senate.dta", clear
sum demmv demvoteshfor2 population dopen dmidterm

* Outcome

global Y "demvoteshfor2"

* Running variable

global R "demmv"

********************************************************************************
** RDPOWER
********************************************************************************

** rdpower against tau = 5

rdpow $Y $R, tau(5)

** rdpower with covariates

rdpow $Y $R, tau(5) covs(population dopen dmidterm)

** rdpower with user-specified plot options

rdpow $Y $R, tau(5) plot graph_range(-9 9) graph_step(2) ///
		                     graph_options(title(Power function) ///
							 xline(0, lcolor(black) lpattern(dash)) ///
							 yline(.05, lpattern(shortdash) lcolor(black)) ///
							 xtitle(tau) ytitle(power) ///
							 graphregion(fcolor(white))) 

** rdpower with rdrobust options

rdpow $Y $R, tau(5) h(16 18) b(18 20)

rdpow $Y $R, tau(5) kernel(uniform) vce(cluster state)

rdpow $Y $R, tau(5) bwselect(certwo) vce(hc3) scaleregul(0) rho(1)

** rdpower with conventional inference

rdpow $Y $R, tau(5) all

** rdpower with user-specified bias and variance

qui rdrobust $Y $R

local samph = e(h_l)
local sampsi_l = e(N_h_l)
local sampsi_r = e(N_h_r)

local bias_l = e(bias_l)/(e(h_l)^2)
local bias_r = e(bias_r)/(e(h_r)^2)

mat VL_RB = e(V_rb_l)
mat VR_RB = e(V_rb_r)

local Vl_rb = e(N)*e(h_l)*VL_RB[1,1]
local Vr_rb = e(N)*e(h_r)*VR_RB[1,1]

rdpow $Y $R, tau(5) bias(`bias_l' `bias_r') ///
                             var(`Vl_rb' `Vr_rb') ///
							 samph(`samph') sampsi(`sampsi_l' `sampsi_r')

** rdpower manually increasing variance by 20%

qui rdrobust $Y $R

mat VL_RB = e(V_rb_l)
mat VR_RB = e(V_rb_r)

local Vl_rb = e(N)*e(h_l)*VL_RB[1,1]*1.2
local Vr_rb = e(N)*e(h_r)*VR_RB[1,1]*1.2

rdpow $Y $R, tau(5) var(`Vl_rb' `Vr_rb')

** rdpower without data

qui rdpow $Y $R, tau(5)
rdpow, tau(5) nsamples(r(N_l) r(N_h_l) r(N_r) r(N_h_r)) ///
		 bias(r(bias_l) r(bias_r)) ///
		 var(r(Vl_rb) r(Vr_rb)) sampsi(r(sampsi_l) r(sampsi_r)) ///
		 samph(r(samph_l) r(samph_r))

** comparing ex-post power across specifications

rdpow $Y $R, tau(5) p(1) h(20) plot

rdpow $Y $R, tau(5) p(2) h(20) plot

rdpow $Y $R, tau(5) p(1) plot

rdpow $Y $R, tau(5) p(2) plot

** rdpower with clustering

rdpow $Y $R, tau(5) vce(cluster state)

********************************************************************************
** RDSAMPSI
********************************************************************************

** rsampsi with tau = 5

rdsampsi $Y $R, tau(5)

** rsampsi with tau = 5 setting bandwdith and nratio with plot

rdsampsi $Y $R, tau(5) beta(.9) samph(18 19) nratio(.5) plot

** rsampsi with conventional inference

rdsampsi $Y $R, tau(5) all

** rsampsi vs rdpower

rdsampsi $Y $R, tau(5)
rdpow $Y $R, tau(5) sampsi(r(sampsi_h_l) r(sampsi_h_r))

** rsampsi without data

qui rdsampsi $Y $R, tau(5)
local init = r(init_cond)
rdsampsi, tau(5) nsamples(r(N_l) r(N_h_l) r(N_r) r(N_h_r)) ///
				 bias(r(bias_l) r(bias_r)) ///
				 var(r(var_l) r(var_r)) ///
				 samph(r(samph_l) r(samph_r)) ///
				 init_cond(`init')

** comparing sample sizes across designs

rdsampsi $Y $R, tau(5) p(0) h(20) plot

rdsampsi $Y $R, tau(5) p(1) h(20) plot

rdsampsi $Y $R, tau(5) p(0) plot

rdsampsi $Y $R, tau(5) p(1) plot

** rdsampsi with clustering

rdsampsi $Y $R, tau(5) vce(cluster state)

********************************************************************************
** RDMDE
********************************************************************************

** rdmde with default options

rdmde $Y $R

** rdmde for a power of 0.75

rdmde $Y $R, beta(0.75)

** rdmde manually setting the bandwidths

rdmde $Y $R, samph(12 13)

** rdmde manually setting the sample size inside the bandwidths

rdmde $Y $R, sampsi(350 320)

** rdmde without data

qui rdrobust $Y $R
local bias_l = e(bias_l)/(e(h_l)^2)
local bias_r = e(bias_r)/(e(h_r)^2)
mat VL_RB = e(V_rb_l)
local Vl_rb = e(N)*e(h_l)*VL_RB[1,1]
local Vr_rb = `Vl_rb'*1.1
rdmde, nsamples(600 350 700 320) ///
	   bias(`bias_l' `bias_r') var(`Vl_rb' `Vr_rb') ///
	   samph(17) init_cond(4)
							 
** compare rdmde and rdpower
							 
rdmde $Y $R
local mde = r(mde)
rdpow $Y $R, tau(`mde')

** compare rdmde and rdsampsi

rdmde $Y $R 
local mde = r(mde)
local nratio = r(N_h_r) / (r(N_h_r)+r(N_h_l))
rdsampsi $Y $R, tau(`mde') nratio(`nratio')

** rdmde with clustering

rdmde $Y $R, vce(cluster state)