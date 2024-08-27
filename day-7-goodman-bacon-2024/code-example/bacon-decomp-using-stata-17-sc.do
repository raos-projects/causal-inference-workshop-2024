use "https://github.com/scunning1975/mixtape/raw/master/castle.dta", clear

  gen treat = (year >= effyear)

  * Result from ddtiming gives me 25 2x2 because there are 5 timing groups and a never treated.  
	net install ddtiming, from(https://raw.githubusercontent.com/tgoldring/ddtiming/master)
	areg l_homicide i.year treat, a(sid) cluster(sid) robust 
	ddtiming l_homicide treat, i(sid) t(year)

	* But now when we use bacondecomp, I don't get 25 2x2s and comparing between them is confusing.
	bacondecomp l_homicide treat, ddetail
	
	bacondecomp l_homicide treat, ddetail gropt(title("Detail") name(detail, replace) legend(pos(6)))
	
	* The "Within" is the "never treated"
	* The "Timing groups" is the "alread treated"
	* The "Never treated vs timing" is "not yet treated"
	
	xtdidregress (l_homicide) (treat), group(sid) time(year)
	estat bdecomp, graph
