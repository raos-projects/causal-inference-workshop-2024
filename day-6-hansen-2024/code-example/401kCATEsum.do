*** Code for 401(k) example illustrating inference in heterogeneous effect model
*** CATE summaries

capture log close
log using 401kCATEsum.txt , text replace 

clear all
 
* Load data. Assumes I am in my code subfolder *
use "..\Data\401kATE.dta", clear 
* All the results from ATE estimation from 401kATE.do

*** Let's look at GATE by income quintile bin
egen incbin = cut(inc) , group(5)
tab incbin , gen(incdum)

*** Just going to use stacking results
*** Cross-fit fold number stored in m0_fid_#
*** Cross-fit propensity score stored in e401_ss_#
*** Cross-fit regression function stored in net_tfa_ss0_#, net_tfa_ss1_#

*** Form score function
forvalues r = 1/10 {
	gen psi_`r' = Y_net_tfa_ss1_`r' - Y_net_tfa_ss0_`r' ///
		+ e401*(net_tfa - Y_net_tfa_ss1_`r')/D_e401_ss_`r' ///
		- (1-e401)*(net_tfa - Y_net_tfa_ss0_`r')/(1-D_e401_ss_`r')
}

*** Form cross-fit estimates of group membership
forvalues r = 1/10 {
	forvalues b = 1/5 {
		qui gen pinc_`b'_`r' = .
		forvalues k = 1/5 {
			qui sum incdum`b' if m0_fid_`r' != `k'
			qui replace pinc_`b'_`r' = r(mean) if m0_fid_`r' == `k'
		}
	}
}

**** GATEs
matrix gates = J(10,5,.)
matrix gates_se = J(10,5,.)
matrix rownames gates = rep1 rep2 rep3 rep4 rep5 rep6 rep7 rep8 rep9 rep10
matrix rownames gates_se = rep1 rep2 rep3 rep4 rep5 rep6 rep7 rep8 rep9 rep10
matrix colnames gates = inc1 inc2 inc3 inc4 inc5 
matrix colnames gates_se = inc1 inc2 inc3 inc4 inc5 
forvalues r = 1/10 {
	forvalues b = 1/5 {
		qui gen denom = incdum`b'/pinc_`b'_`r'
		qui sum denom
		local md = r(mean)
		qui gen numer = (incdum`b'/pinc_`b'_`r')*psi_`r'
		qui sum numer
		local mn = r(mean)
		matrix gates[`r' , `b'] = `mn'/`md'
		qui gen phi2 = ((numer - (incdum`b'/pinc_`b'_`r')*(`mn'/`md'))/`md')^2
		qui sum phi2
		local V = r(mean)
		local N = r(N)
		matrix gates_se[`r' , `b'] = sqrt(`V'/`N')
		qui drop denom numer phi2
	}
}
matrix list gates
matrix list gates_se

coefplot (matrix(gates[1]) , se(gates_se[1])) /// 
		 (matrix(gates[2]) , se(gates_se[2])) ///
		 (matrix(gates[3]) , se(gates_se[3])) ///
		 (matrix(gates[4]) , se(gates_se[4])) ///
		 (matrix(gates[5]) , se(gates_se[5])) ///
		 (matrix(gates[6]) , se(gates_se[6])) ///
		 (matrix(gates[7]) , se(gates_se[7])) ///
		 (matrix(gates[8]) , se(gates_se[8])) ///
		 (matrix(gates[9]) , se(gates_se[9])) ///
		 (matrix(gates[10]) , se(gates_se[10])) , ///
		 plotlabels(rep1 rep2 rep3 rep4 rep5 rep6 rep7 rep8 rep9 rep10) ///
		 mcolor(ebblue) ciopts(color(red ebblue)) levels(99.5 95)
graph export ..\Slides\figures\Stata_401kGATEinc.png , replace
graph close

*** These are close, but not identical to, BLP using incbin

*** BLP of CATE
* Note variables are not in original units
forvalues r = 1/10 {
	reg psi_`r' age inc educ fsize marr twoearn db pira hown , robust
	est store results_`r'
}
coefplot results_1 results_2 results_3 results_4 results_5 results_6 results_7 results_8 results_9 results_10 , drop(_cons)
graph export ..\Slides\figures\Stata_401kBLP1.png , replace
graph close

forvalues r = 1/10 {
	reg psi_`r' incdum* , nocons robust
	est store results_`r'
}
coefplot results_1 results_2 results_3 results_4 results_5 results_6 results_7 results_8 results_9 results_10 , mcolor(ebblue) ciopts(color(red ebblue)) levels(99.5 95)
graph export ..\Slides\figures\Stata_401kBLPinc.png , replace
graph close


log close
