# causal-inference-workshop-2024

Course Material from Main and Advanced Causal Inference Workshops at Northwestern Pritzker School of Law (2024). The course description and announcement is reproduced with minor modification below. This repository is not in any way endorsed by or affiliated with the workshop.

## Main and Advanced Causal Inference Workshops
*Main: Monday-Friday, July 29-August 2, 2024*

*Advanced: Sunday-Wednesday, August 4-7, 2024*

We are excited to be holding our 13th annual workshop on Research Design for Causal Inference at Northwestern Pritzker School of Law in Chicago, IL. We invite you to attend. 

Our Advanced Workshop on Research Design for Causal Inference will be held this year on Sunday afternoon, August 4 through Wednesday, August 7.

What’s special about these workshops are the speakers. The session will be taught by world-class causal inference researchers, who are experts in the topics they will discuss. In person registration is limited to 125 participants for each workshop. 

There will also be a Zoom option, but please come in-person if you can. The online experience is not the same.

Location
Northwestern Pritzker School of Law
375 East Chicago Avenue, Chicago, IL 60611 

### Main Workshop Overview
We will cover the design of true randomized experiments and contrast them to natural or quasi experiments and to pure observational studies, where part of the sample is treated, the remainder is a control group, but the researcher controls neither which units are treated vs. control, nor administration of the treatment. We will assess the causal inferences one can draw from specific "causal" research designs, threats to valid causal inference, and research designs that can mitigate those threats.

Most empirical methods courses survey a variety of methods. We will begin instead with the goal of causal inference, and how to design a research plan to come closer to that goal, using messy, real-world datasets with limited sample sizes. The methods are often adapted to a particular study. 

#### Target Audience
Quantitative empirical researchers (including faculty, graduate students, post-docs, and other researchers) in social science, including law, political science, economics, many business-school areas (finance, accounting, management, marketing, etc.), medicine, sociology, education, psychology, etc. –anywhere that causal inference is important.

We will assume knowledge, at the level of an upper-level undergraduate econometrics or similar course, of multivariate regression, including OLS, logit, and probit; basic probability and statistics including confidence intervals, t-statistics, and standard errors; and some understanding of instrumental variables. This course should be suitable both for researchers with recent PhD-level training in econometrics and for empirical scholars with reasonable but more limited training.

### Advanced Workshop Overview

The advanced workshop provides in-depth discussion of selected topics that are beyond what we can cover in the main workshop. The principal topics for 2024 are application of machine learning methods to causal inference; difference-in-differences methods for staggered treatments (applied to different units at different times); and advanced instrumental variable methods.

#### Target Audience
Empirical researchers who are familiar with the basics of causal inference (from our main workshop or otherwise), and want to extend their knowledge.  We will assume familiarity, but not expertise, with potential outcomes, difference-in-differences, and panel data methods.

### Teaching Faculty
In order of appearance:

#### Donald Rubin (Harvard University)
Donald Rubin is John L. Loeb Professor of Statistics Emeritus, at Harvard. His work on the "Rubin Causal Model" is central to modern understanding of causal inference with observational data. Principal research interests: statistical methods for causal inference; Bayesian statistics; analysis of incomplete data.

#### Brigham Frandsen (Brigham Young University, Department of Economics)
Brigham Frandsen is Associate Professor of Economics at Brigham Young University. Research interests include developing methods for causal inference on how treatment effects vary across the treated population, and applying those methods to labor, education, and health economics questions.

#### Yiqing Xu (Stanford University)
Yiqing Xu is Assistant Professor of Political Science at Stanford University. His main methods research  involves causal inference with panel data.

#### Gonzalo Vazquez-Bare (University of California, Santa Barbara)
Gonzalo Vazquez-Bare is Assistant Professor of Economics at University of California, Santa Barbara.  His recent research includes estimation of causal spillover effects, and multiple-cutoff regression discontinuity designs. 

#### Eric French (Cambridge University)
Eric French is Montague Burton Professor of Industrial Relations and Labour Economics at the University of Cambridge, Faculty of Economics. His primary research interests include labor, public finance, health, and applied econometrics.

#### Christian Hansen (University of Chicago)
Christian Hansen is Wallace W. Booth Professor of Econometrics and Statistics at the University of Chicago, Booth School of Business. His research includes use of machine learning methods in estimation of causal and policy effects, estimation of panel data models, inference using clustered standard errors, quantile regression, and weak instruments.

#### Andrew Goodman-Bacon (Federal Reserve Board, Minneapolis)
Andrew Goodman-Bacon is Senior Economist at the Opportunity and Inclusive Growth Institute at the Federal Reserve Bank of Minneapolis. His research interests include economic history, health economics, public economics, and applied econometrics.  

#### Peter Hull (Brown University)
Peter Hull is Professor of Economics at Brown University. His research interests include applied econometrics, crime, discrimination, education, and healthcare.  

### Conference Organizers

#### Bernard Black  (Northwestern University)
Bernie Black is Nicholas J. Chabraja Professor at Northwestern University, with positions in the Pritzker School of Law, the Institute for Policy Research, and the Kellogg School of Management, Finance Department. Principal research interests: health law and policy; empirical legal studies, law and finance.

#### Scott Cunningham  (Baylor University)
Scott Cunningham is Professor of Economics at Baylor University. Principal research interests: mental healthcare; suicide; corrections; sex work; abortion policy; drug policy.  

### Workshop Schedule
Plan on full days, roughly 9:00-4:30. Breakfast will be available at 8:30.

There will be afternoon wine and cheese receptions for all attendees following the main sessions on Monday, July 29.

An informal wine-and cheese reception for all attendees will follow on Monday, August 5. 

#### Monday, July 29
**Day 1: Introduction to Modern Methods for Causal Inference**

*Donald Rubin*

Overview of causal inference and the Rubin "potential outcomes" causal model. The "gold standard" of a randomized experiment. Treatment and control groups, and the core role of the assignment (to treatment) mechanism. Causal inference as a missing data problem, and imputation of missing potential outcomes.  Experimental design and applications to observational studies. One-sided and two-sided noncompliance. 


#### Tuesday, July 30 
**Day 2: Matching and Reweighting Designs for "Pure" Observational Studies**

*Brigham Frandsen* 

The core, untestable requirement of selection [only] on observables. Ensuring covariate balance and common support. Matching, reweighting, and regression estimators of average treatment effects.  Propensity score methods.

#### Wednesday, July 31 
**Day 3: Panel Data and Difference-in-Differences**

*Yiqing Xu*

Panel data methods: pooled OLS, random effects, and fixed effects. Simple two-period DiD and panel data extensions. The core “parallel trends” assumption. Testing this assumption. Event study (leads and lags) and distributed lag models. Accommodating covariates. Robust and clustered standard errors. Robust and clustered standard errors. Many faces of DiD. Triple differences. 


#### Thursday, August 1 
**Day 4: Regression Discontinuity**

*Gonzalo Vazquez-Bare*

Regression discontinuity (RD) designs: sharp and fuzzy designs; continuity-based methods and bandwidth selection; local randomization methods and window selection; extensions and generalizations of canonical RD setup: discrete running variable, multi-cutoff, multi-score, and geographic designs. RD software website


#### Friday, August 2: Morning 
**Day 5a: Instrumental Variable Methods**

*Eric French*

Reasons for using instrumental variables (IV); causal inference with IV: the role of the exclusion restriction and first stage assumption; the monotonicity assumption and local average treatment effect (LATE) interpretation; applications to randomized experiments with imperfect compliance, including intent-to-treat designs. Marginal treatment effects (MTEs); Connections between IV and fuzzy RD designs.


#### Friday, August 2: Afternoon
**Day 5b: Feedback on your own research**

Attendees will present their own research design questions from current work in breakout sessions and receive feedback on research design.  Session leaders:  Bernie Black, Scott Cunningham, Eric French, Joshua Lerner).  Additional parallel sessions if needed to meet demand.

#### Sunday afternoon, August 4 (optional)
**Day 6a: Primer on machine learning approaches to prediction**

*Christian Hansen*

Introduction to "machine-learning" approaches to prediction algorithms, aimed at attendees with limited knowledge of machine learning methods. Shrinking a large set of potential predictors. Predicting without overpredicting: training and test sets; cross-validation. Lasso, regression trees, random forests, and deep nets. High-dimensional model selection (function classes, regularization, tuning). Combining models (ensemble models, bagging, boosting), model evaluation, and implementation.

#### Monday, August 5
**Day 6b: Applications of machine learning to causal inference**

*Christian Hansen*

When and how can machine learning methods be applied to causal inference questions. Limitations (prediction vs estimation) and opportunities (data pre-processing, prediction as quantity of interest, high-dimensional nuisance parameters), with examples from an emerging empirical literature.

#### Tuesday, August 6
**Day 7: Advanced Difference-in Differences**

*Andrew Goodman-Bacon*

New developments in causal inference in difference-in-differences designs. Limitations of two-way fixed effects regressions. Comparison of alternative estimation strategies that have been proposed to address these weaknesses and to accommodate complex treatment variables. Ways to weaken the parallel trends assumption and to diagnose and/or deal with violations of parallel trends.

#### Wednesday, August 7
**Day 8: Advanced Instrumental Variables**

*Peter Hull*

Design vs. model-based identification, weak and many instrument bias, estimating complier characteristics, judge IV, shift-share IV and other "formula" instruments. 


### Stata and R coding
On selected days, we will run parallel Stata and R sessions, following the main lectures, to illustrate code for the research designs discussed in the lectures. Some speakers will also build Stata or R code into their lecture slides. Presenters: Scott Cunningham (Stata) and Joshua Lerner (R).

We will also provide a repository of datasets and code to illustrate the methods presented in the workshop. 

### Registration and Workshop Cost
The workshop fee includes all materials, breakfast, lunch, snacks, and the receptions. 

Main Workshop: Tuition is \$900 (\$600 for post-docs and graduate students; \$500 if you are Northwestern-affiliated). 

Advanced Workshop: Tuition is \$650 (\$450 for post-docs and graduate students; \$300 if you are Northwestern affiliated).  

Discount for attending both workshops: There is a \$250 discount for non-Northwestern persons attending both workshops, for combined cost of \$1,300 (\$800 for post-docs and graduate students; \$150 additional discount for Northwestern affiliates).

Zoom option: We are charging the same amount for in-person and virtual attendance. Partly, we want to encourage in-person attendance. We also want to allow attendees to switch from one format to the other.

We know the workshop is not cheap. We use the funds to pay our speakers and expenses. Professor Black does not pay himself. 