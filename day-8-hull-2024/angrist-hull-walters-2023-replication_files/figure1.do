/*************************************************

	HANDBOOK OF THE ECONOMICS OF EDUCATION: 
	METHODS FOR MEASURING SCHOOL EFFECTIVENESS
	
	
	FIGURE 1: COMPLIER POTENTIAL OUTCOMES BY RACE 
	
	This do-file contains code to plot complier characteristics 
	by race, as given in figure 1 of the handbook chapter.  
	
	NOTES: 
	1. This do-file does not conduct the hypothesis test for equality of 
	distributions that produces the p-values reported in figure 1. 
	To produce the p-values, please see the file figure1_test.do.
	
	2. Please make sure the dataset does not already have variables named D or Z. 
	D and Z are defined in the course of this do-file; the analysis will be 
	incorrect if the dataset already has a variable named D which is not the dummy 
	for treatment, or a variable named Z which is not the dummy for an offer received.
	
	Before running this file, please make sure your dataset has the following variables: 
	1. A variable with standardized test scores (the outcome)
	2. A dummy variable that indicates treatment 
	3. A dummy variable for the instrument (e.g. if an offer was made after the lottery)
	4. A numeric variable that captures grades 
	5. Dummy variables that capture risk sets 
	
	
	To run this file, please follow the steps below: 
	1. Provide the necessary local paths in the "globals for directories" section below
	2. Provide the requested variable names in the "globals for variables" section below
	
	
************************************************/


	clear all 
	
* install necessary packages 
	cap ssc install ivreg2 

* globals for directories (PROVIDE LOCAL PATHS)
	global data = "" // path to data file 
	global intermediate = "" // path where you wish to save intermediate data files produced by this do-file
	global output = ""  // path where you wish to save the resulting graphs
	global data_file = "" // name of data file 

* globals for variables (PROVIDE VARIABLE NAMES)
	global race1 = "" // a dummy var for the first racial group 
	global racial_group_title1 = "" // title for the first racial group, used in graph label
	global race2 = "" // a dummy var for the second racial group 
	global racial_group_title2 = "" // title for the second racial group, used in the graph label 
	global treatment = "" // treatment dummy 
	global lottery_offer = "" // instrument dummy 
	global outcome = "" // outcome variable 
	global grade_var = "" // numeric variable that captures grades
	global grade =  // grade for which you wish to plot complier potential outcomes (a number)
	global covariates = "" // (e.g. "special_lunch_status female")
	global risk_sets = "" // variable list of dummies that capture risk sets 
	

	 clear
	 clear matrix
	 set seed 49584

 foreach c in $race1 $race2 { 

********* SETUP STEPS BEGIN HERE ***********************************************

	use "${data}\\${data_file}", clear

	keep if `c' == 1
	
	* Create variables for (1) outcome of interest, Y, (2) treatment dummy, D, (3) instrument, Z

		* (1) Y, outcome of interest
			cap gen Y = $outcome

		* (2) D, treatment dummy 
			cap gen D = $treatment

		* (3) Z, instrument
			cap gen Z = $lottery_offer  

	* restrict sample to grade of interest 
		keep if $grade_var == $grade

	* Create a matrix to store relevant results
		matrix F=J(100,7,.)
		local r=1 // set the initial row to output results in the matrix

********SETUP ENDS *************************************************************

********ESTIMATION STEPS BEGIN HERE ********************************************


	* Step e1: Estimate first stage to get number of compliers
		qui ivreg2 D Z $covariates $risk_sets 
		local fs=_b[Z]
		sum Z
		local pz=r(mean)


	*Step e2: Get silverman rule of thumb bandwidth for each potential outcome

					*Y0
					quietly {
						preserve
						keep if Y!=. & D!=. & Z!=.
						gen Y0=Y*(1-D)
						gen D0=1-D
						ivreg2 Y0 (D0=Z) $covariates $risk_sets
						local EY=_b[D0]
						local EY0_`c'=_b[D0]
						replace Y0=(Y^2)*(1-D)
						ivreg2 Y0 (D0=Z) $covariates $risk_sets
						local EY2 = _b[D0]
						display in red "EY: `EY', EY2: `EY2'"

						local sigma=sqrt(`EY2' - (`EY')^2)
						local h0= 1.5 * 1.06*`sigma'*(((1-`pz')*`fs'*e(N))^(-1/5))
						local N_Y0_`c'=e(N)
						display in red "h0: `h0', sigma: `sigma', pz: `pz', fs: `fs'"
						restore
					}

					*Y1
					quietly {
						preserve
						keep if Y!=. & D!=. & Z!=.
						gen Y1=Y*D
						su Y1
						ivreg2 Y1 (D=Z) $covariates $risk_sets
						local EY=_b[D]
						local EY1_`c'=_b[D]
						replace Y1=(Y^2)*D
						ivreg2 Y1 (D=Z) $covariates $risk_sets
						local EY2 = _b[D]
						display in red "EY: `EY', EY2: `EY2'"
						local sigma=sqrt(`EY2' - (`EY')^2)
						local h1= 1.5 * 1.06*`sigma'*((`pz'*`fs'*e(N))^(-1/5))
						local N_Y1_`c'=e(N)
						display in red "h1: `h1', sigma: `sigma', pz: `pz', fs: `fs'"
						restore
					}

					

	*Step e3: Select values for estimating densities
				local nvals=100
				sum Y, detail
				local ymin=r(min)
				local ymax=r(max)
				local yinc=(`ymax'-`ymin')/`nvals'
				local y=`ymin'
				local count=1
				disp `ymin'


				*Estimate densities at each point
				local progress=1
				while `count'<=`nvals' {

					quietly {
						preserve
						keep if Y!=. & D!=. & Z!=.
						matrix F[`r',1]=`y'

						*Y0 density
						gen Y0=(1/`h0')*normalden((Y-`y')/`h0')*(1-D)
						gen D0=1-D
						qui ivreg2 Y0 (D0 = Z) $covariates $risk_sets
						matrix F[`r',3]=_b[D0]

						*Y1 density
						display "h1: `h1', y: `y'"
						gen Y1=(1/`h1')*normalden((Y-`y')/`h1')*D
						qui ivreg2 Y1 (D = Z) $covariates $risk_sets
						matrix F[`r',4]=_b[D]
						restore
					}

					local ++count
					local y=`y'+`yinc'
					local prog=`count'/`nvals'

					disp "Y=`y', progress=`prog'"

				local ++r
				}


	*Step e4: Save estimates
		clear
		svmat F
		ren F1 y
		ren F3 f0
		ren F4 f1

		save "$intermediate\complier_data_`c'_$grade", replace

	}


********* ESTIMATION ENDS ******************************************************

********* GRAPHING STEPS BEGIN HERE ********************************************


		use "$intermediate\complier_data_${race2}_$grade", clear
		drop if y ==.
		gen race = "$racial_group_title2"

		append using "$intermediate\complier_data_${race1}_$grade"
		replace race = "$racial_group_title1" if race == ""
		drop if y == .

		gen f0_$racial_group_title1 = f0 if race == "$racial_group_title1"
		gen f0_$racial_group_title2 = f0 if race == "$racial_group_title2"
		gen f1_$racial_group_title1 = f1 if race == "$racial_group_title1"
		gen f1_$racial_group_title2 = f1 if race == "$racial_group_title2"

		graph twoway (line f0_$racial_group_title2 y, lcolor(dkgreen)) ///
					(line f0_$racial_group_title1 y , lpattern(longdash) lcolor(black)), xtitle("Y(0), Math score (standard deviation)", size(vlarge))  xscale(lwidth(medium)) yscale(lwidth(medium)) ///
					 xline(`EY0_$race2', lcolor(dkgreen)) xline(`EY0_$race1', lpattern(longdash) lcolor(black)) ytitle("Density", size(large))  ylabel(, nogrid) name(y0_$grade) ///
					 graphregion(color(white)) title("$grade", size(large)) legend(off) 

		graph twoway (line f1_$racial_group_title2 y, lcolor(dkgreen)) ///
				(line f1_$racial_group_title1 y , lpattern(longdash) lcolor(black)), xtitle("Y(1), Math score (standard deviation)", size(vlarge))  xscale(lwidth(medium)) yscale(lwidth(medium)) ///
				 ytitle("Density", size(large)) xline(`EY1_$race2', lcolor(dkgreen)) xline(`EY1_$race1', lcolor(black) lpattern(longdash)) ylabel(, nogrid) name(y1_$grade) ///
				 graphregion(color(white)) title("$grade" , size(large)) legend(off) 

		graph combine y0_$grade y1_$grade, ycommon xcommon graphregion(color(white)) xsize(8) ysize(4)

		graph export "$output\complier_distb_$grade_`c(current_date)'.png", replace width(1500) height(500)



