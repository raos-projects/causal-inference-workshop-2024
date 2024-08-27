*** Code for STI testing example illustrating conditional inference 

capture log close
log using STIci.txt , text replace 

clear all

set maxvar 30000

* Load data. Assumes I am in my code subfolder *
import delimited using "..\Data\processed_esti.csv", clear

* Recombine some dummies
gen gender = gender_female+2*gender_male+3*gender_transgender
gen ethnicgrp = ethnicgrp_asian+2*ethnicgrp_black+3*ethnicgrp_mixed_multiple+ ///
				4*ethnicgrp_other+5*ethnicgrp_white				
		  
*** Set seed for reproducibility
set seed 73023

*** Split data into a training (~50%) and test (~50%) set
gen trrnd = runiform()
egen trdata = cut(trrnd) , group(2)

*** ATE in full sample and training sample
reg y w
reg y w if trdata == 1

* Horvitz-Thompson transformation - cheating by taking sample treated fraction 
* as design treatment probability
qui sum w if trdata == 1
local p = r(mean)
gen H = (w - `p')/(`p'*(1-`p'))
gen HY = H*y

*** Generate some candidate CATE estimates using the training data
gen YNo = (1-w)*y/(1-`p')
gen YAll = w*y/(`p')

gen Rrnd = runiform() if trdata == 0
qui sum Rrnd , d
local rmed = r(p50)
gen treatR = Rrnd > `rmed' if trdata == 0

* 1) baseline regression
reg HY i.gender i.ethnicgrp partners1 postlaunch msm age i.imd_decile if trdata == 1
predict cate1 
gen treat1 = cate1 > 0
gen Yt1 = treat1*YAll + (1-treat1)*YNo
qui sum cate1 if trdata == 0, d
local rmed = r(p50)
gen treat1R = cate1 > `rmed' & cate1 > 0 if trdata == 0

* 2) lasso with full interaction
gen age2 = age^2
gen age3 = age^3
pystacked HY ///
	(gender##ethnicgrp##partners1##postlaunch##msm##imd_decile)##c.(age age2 age3) || ///
	m(lassocv) || if trdata == 1 , type(reg)
predict cate2
gen treat2 = cate2 > 0
gen Yt2 = treat2*YAll + (1-treat2)*(1-w)*YNo
qui sum cate2 if trdata == 0, d
local rmed = r(p50)
gen treat2R = cate2 > `rmed' & cate2 > 0 if trdata == 0

* 3) ridge with full interaction
pystacked HY ///
	(gender##ethnicgrp##partners1##postlaunch##msm##imd_decile)##c.(age age2 age3) || ///
	m(ridgecv) || if trdata == 1 , type(reg)
predict cate3
gen treat3 = cate3 > 0
gen Yt3 = treat3*YAll + (1-treat3)*YNo
qui sum cate3 if trdata == 0, d
local rmed = r(p50)
gen treat3R = cate3 > `rmed' & cate3 > 0 if trdata == 0

* 4) random forest
pystacked HY ///
	gender ethnicgrp partners1 postlaunch msm age imd_decile || ///
	m(rf) opt(n_estimators(500) min_samples_leaf(20) random_state(731)) || ///
	if trdata == 1 , type(reg)
predict cate4
gen treat4 = cate4 > 0
gen Yt4 = treat4*YAll + (1-treat4)*YNo
qui sum cate4 if trdata == 0, d
local rmed = r(p50)
gen treat4R = cate4 > `rmed' & cate4 > 0 if trdata == 0

* 5) random forest
pystacked HY ///
	gender ethnicgrp partners1 postlaunch msm age imd_decile || ///
	m(rf) opt(n_estimators(500) random_state(731)) || ///
	if trdata == 1 , type(reg)
predict cate5
gen treat5 = cate5 > 0
gen Yt5 = treat5*YAll + (1-treat5)*YNo
qui sum cate5 if trdata == 0, d
local rmed = r(p50)
gen treat5R = cate5 > `rmed' & cate5 > 0 if trdata == 0

* In sample expected outcome
sum YNo YAll Yt1 Yt2 Yt3 Yt4 Yt5 if trdata == 1

* Hold out sample expected outcome
qui sum w if trdata == 0
local p = r(mean)

gen YNo_out = (1-w)*y/(1-`p')
gen YAll_out = w*y/(`p')
gen Yt1_out = treat1*YAll_out + (1-treat1)*YNo_out
gen Yt2_out = treat2*YAll_out + (1-treat2)*YNo_out
gen Yt3_out = treat3*YAll_out + (1-treat3)*YNo_out
gen Yt4_out = treat4*YAll_out + (1-treat4)*YNo_out
gen Yt5_out = treat5*YAll_out + (1-treat5)*YNo_out

sum YNo_out YAll_out Yt1_out Yt2_out Yt3_out Yt4_out Yt5_out if trdata == 0

gen mu1 = w*y
qui sum mu1
local m1 = r(mean)
gen mu0 = (1-w)*y
qui sum mu0
local m0 = r(mean)

qui sum w if trdata == 0
local p = r(mean)
gen c_w = w - `p'
qui sum mu1 if trdata == 0
local m1 = r(mean)
qui sum mu0 if trdata == 0
local m0 = r(mean)
gen sc0 = mu0/(1-`p') + (`m0'/((1-`p')^2))*c_w
gen sc1 = mu1/`p' - (`m1'/(`p'^2))*c_w

gen mu1_t1 = treat1*w*y
qui sum mu1_t1 
local m1 = r(mean)
gen mu0_t1 = (1 - treat1)*(1-w)*y
qui sum mu0_t1
local m0 = r(mean)
gen sct1 = mu1_t1/`p' + mu0_t1/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t2 = treat2*w*y
qui sum mu1_t2 
local m1 = r(mean)
gen mu0_t2 = (1 - treat2)*(1-w)*y
qui sum mu0_t2
local m0 = r(mean)
gen sct2 = mu1_t2/`p' + mu0_t2/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t3 = treat3*w*y
qui sum mu1_t3
local m1 = r(mean)
gen mu0_t3 = (1 - treat3)*(1-w)*y
qui sum mu0_t3
local m0 = r(mean)
gen sct3 = mu1_t3/`p' + mu0_t3/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t4 = treat4*w*y
qui sum mu1_t4
local m1 = r(mean)
gen mu0_t4 = (1 - treat4)*(1-w)*y
qui sum mu0_t4
local m0 = r(mean)
gen sct4 = mu1_t4/`p' + mu0_t4/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t5 = treat5*w*y
qui sum mu1_t5
local m1 = r(mean)
gen mu0_t5 = (1 - treat5)*(1-w)*y
qui sum mu0_t5
local m0 = r(mean)
gen sct5 = mu1_t5/`p' + mu0_t5/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

reg sc0 if trdata == 0 
reg sc1 if trdata == 0
reg sct1 if trdata == 0
reg sct2 if trdata == 0
reg sct3 if trdata == 0
reg sct4 if trdata == 0
reg sct5 if trdata == 0

gen d1 = sc1-sc0
gen dt1 = sct1-sc0
gen dt2 = sct2-sc0
gen dt3 = sct3-sc0
gen dt4 = sct4-sc0
gen dt5 = sct5-sc0

reg d1 if trdata == 0
reg dt1 if trdata == 0
reg dt2 if trdata == 0
reg dt3 if trdata == 0
reg dt4 if trdata == 0
reg dt5 if trdata == 0

gen YCR = treatR*YAll_out + (1-treatR)*YNo_out
gen Yt1R_out = treat1R*YAll_out + (1-treat1R)*YNo_out
gen Yt2R_out = treat2R*YAll_out + (1-treat2R)*YNo_out
gen Yt3R_out = treat3R*YAll_out + (1-treat3R)*YNo_out
gen Yt4R_out = treat4R*YAll_out + (1-treat4R)*YNo_out
gen Yt5R_out = treat5R*YAll_out + (1-treat5R)*YNo_out

sum YNo_out YCR Yt1R_out Yt2R_out Yt3R_out Yt4R_out Yt5R_out if trdata == 0

gen mu1_R = treatR*w*y
qui sum mu1_R 
local m1 = r(mean)
gen mu0_R = (1 - treatR)*(1-w)*y
qui sum mu0_R
local m0 = r(mean)
gen scR = mu1_R/`p' + mu0_R/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t1R = treat1R*w*y
qui sum mu1_t1R 
local m1 = r(mean)
gen mu0_t1R = (1 - treat1R)*(1-w)*y
qui sum mu0_t1R
local m0 = r(mean)
gen sct1R = mu1_t1R/`p' + mu0_t1R/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t2R = treat2R*w*y
qui sum mu1_t2R 
local m1 = r(mean)
gen mu0_t2R = (1 - treat2R)*(1-w)*y
qui sum mu0_t2R
local m0 = r(mean)
gen sct2R = mu1_t2R/`p' + mu0_t2R/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t3R = treat3R*w*y
qui sum mu1_t3R
local m1 = r(mean)
gen mu0_t3R = (1 - treat3R)*(1-w)*y
qui sum mu0_t3R
local m0 = r(mean)
gen sct3R = mu1_t3R/`p' + mu0_t3R/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t4R = treat4R*w*y
qui sum mu1_t4R
local m1 = r(mean)
gen mu0_t4R = (1 - treat4R)*(1-w)*y
qui sum mu0_t4R
local m0 = r(mean)
gen sct4R = mu1_t4R/`p' + mu0_t4R/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

gen mu1_t5R = treat5R*w*y
qui sum mu1_t5R
local m1 = r(mean)
gen mu0_t5R = (1 - treat5R)*(1-w)*y
qui sum mu0_t5R
local m0 = r(mean)
gen sct5R = mu1_t5R/`p' + mu0_t5R/(1-`p') + (`m0'/((1-`p')^2) - `m1'/(`p'^2))*c_w

reg scR if trdata == 0
reg sct1R if trdata == 0
reg sct2R if trdata == 0
reg sct3R if trdata == 0
reg sct4R if trdata == 0
reg sct5R if trdata == 0

gen dt1R = sct1R-scR
gen dt2R = sct2R-scR
gen dt3R = sct3R-scR
gen dt4R = sct4R-scR
gen dt5R = sct5R-scR

reg dt1R if trdata == 0
reg dt2R if trdata == 0
reg dt3R if trdata == 0
reg dt4R if trdata == 0
reg dt5R if trdata == 0


log close

