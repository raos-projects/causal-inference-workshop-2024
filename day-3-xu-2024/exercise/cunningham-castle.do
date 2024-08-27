* load the data into memory

ssc install coefplot

use https://github.com/scunning1975/mixtape/raw/master/castle.dta, clear
set scheme cleanplots

drop post
drop treat

* Simple 2x2:

* treatment and control group (never treated)
* treatment group is the only groups of units that get treated and they get treated at the same time in 2006 and then they stay treated until 2010. but the control group is "never treated" in the course of our panel dataset.

keep if effyear==2006 | effyear==.

* In order to specify a diff in diff where you have only one treatment group (it can have multiple states in it but to be a group in a diff in diff it means you were treated all at the same time). 

* first part of the diff-in-diff: define your treatment group with a dummy variable.
gen treated=0
replace treated = 1 if effyear==2006

* second part of the diff-in-diff: define when the treatment occurred in time. 

gen post = 0
replace post =1 if year>=2006

* What is a diff-in-diff?

summarize l_homicide if treat==1 & post==0 // average homicide for treated in the pre-treatment period
gen y10=`r(mean)'

summarize l_homicide if treat==1 & post==1 // average homicide for treated in the post treatment period
gen y11=`r(mean)'

summarize l_homicide if treat==0 & post==0 // average homicide for control in the pre treatment period
gen y00 = `r(mean)'

summarize l_homicide if treat==0 & post==1 // average homicide for control in the post treatment period. 
gen y01 = `r(mean)'

gen dd = (y11 - y10) - (y01 - y00)
su dd 

** this is the exact same calculation as a "interaction regression"

reg l_homicide post##treat, cluster(sid)
reg l_homicide post##treat [aweight=popwt], cluster(sid)


reg l_homicide treated##ib2005.year, cluster(state)

coefplot, keep(1.treated#*) omitted baselevels cirecast(rcap) ///
    rename(1.treated#([0-9]+).year = \1, regex) at(_coef) ///
    yline(0, lp(solid)) xline(2006, lpattern(dash)) yline(-1000, lp(dash)) ///
    xlab(2000(1)2010) ///
    title("Difference-in-differences regression") ///
    subtitle("Disconnected Event study graphic")
	
	