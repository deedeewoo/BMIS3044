// change working directory


set seed 3044
scalar u = runiform()
display u

// binomial Poisson
generate bi = rbinomial(10,0.3)
generate xp = rpoisson(5)
summarize bi xp

// plot

quietly twoway (hist xc2, width(1)) (kdensity xc2, lwidth(thick))
quietly graph save chi2.gph, replace
quietly twoway (hist xp, width(1)) (kdensity xp, lwidth(thick) w(1))
quietly graph save poisson.gph, replace
graph combine chi2.gph poisson.gph


/* 
Generate random variable from specific distribution

	Method 1 Probability integral transformation

 Example:
 1. continuous distribution: exponential distribution is f(x) = 1-exp(-x) 
 2. discrete distribution: Bernoulli
 
*/

clear
quietly set obs 2000
set seed 3044
generate xue = -ln(1-runiform()) // for continuous - exponential distribution
generate xbernoulli = runiform() > 0.6 // for discrete bernoulli where the success probability is 0.4 
summarize xue xbernoulli

cumul xue, generate(xue_c)
line xue_c xue, sort

	// for geometric distribution (refer to p813 of DeGroot book)

/* 
Generate random variable from specific distribution

	Method 2 Direct transformation

 Example:
 F distribution: from two chi-square distribution

*/

clear
quietly set obs 2000
set seed 3044
generate xc2 = rchi2(10) 
	// generate chi-square with DF of 10, we can also manually do this by adding up 10 standard normal
generate xfn = rchi2(10)/10
generate xfd = rchi2(5)/5
generate xf = xfn/xfd
// mean of F distribution: d2/(d2-2)

summarize xc2 xf

/*
Generate random variable from specific distribution

Example - multivariate normal (not independent)

*/

	** bivariate normal example
	** mean 10, 20
	** var 4, 9
	** correlation 0.5

	** method 1 - 'drawnorm' command
	
clear
quietly set obs 1000
set seed 3044
matrix mu = (10,20)
scalar sig12 = 0.5*sqrt(4*9)
matrix sigma = (4, sig12 \ sig12, 9)
drawnorm y1 y2, means(mu) cov(sigma)
summarize y1 y2

	** method 2 - cholesky decomposition

	** method 3 - Markov chain Monte Carlo

/*

Integral estimation

*/	

clear
quietly set obs 100
set seed 3044
generate double y = invnormal(runiform())
// sampling from standard normal
generate double gy = exp(-exp(y))
quietly summarize gy, meanonly
scalar Egy = r(mean)
display Egy


/*
Bootstrap
*/


/*
Apply simulation to regression
*/
	
	/*
	1. sampling distribution (sample mean) -- illustration of CLT
	*/
	clear
	quietly set obs 30
	generate x = runiform()

	*** user define program - like function in other language

	program onesample, rclass
		drop _all
		quietly set obs 30
		generate x = runiform()
		summarize x
		return scalar meanforonesample = r(mean)
	end

	onesample
	return list

	// two methods to do the simulation
	// the first one is using 'simulation' command
			
		simulate xbar = r(meanforonesample), reps(10000) nodots seed(3044): onesample
		summarize xbar

	// the 2nd one is using postfile (more flexible)

		set seed 3044
		postfile sim xmean using simulation_results, replace
		forvalues i = 1/10000{
			drop _all
			quietly set obs 30
			tempvar x
			generate `x' = runiform()
			quietly summarize `x'
			post sim (r(mean))
		}
		postclose sim

		use simulation_results, clear
		summarize

	/*
	2. check OLS convergence with skewed errors; check if the estimator is consistent
	*/

// assume our model is y = 1 + 2*x + mu, where mu follows chi2(1)-1

	global numobs 150
	global numsims "1000"

	program chi2data, rclass
		drop _all
		set obs $numobs
		generate double x = rchi2(1)
		generate y = 1 + 2*x + rchi2(1)-1
		regress y x
		return scalar b2 = _b[x]
		return scalar se2 = _se[x]
		return scalar t2 = (_b[x]-2) // manually calculate t statistic of whether beta-x is 2
		return scalar r2 = abs(return(t2))>invttail($numobs-2,.025) // whether reject null
		return scalar p2 = 2*ttail($numobs-2, abs(return(t2)))
	end
	
	set seed 3044
	quietly chi2data
	return list
	
	/*
	3. Test power
	*/	

	program chi2datab, rclass
		drop _all
		set obs $numobs
		generate double x = rchi2(1)
		generate y = 1 + 2*x + rchi2(1)-1
		regress y x
		return scalar b2 = _b[x]
		return scalar se2 = _se[x]
		test x=2.1 // set the alternative hypothesis
		return scalar r2 = (r(p)<0.05)
	end
	
	simulate b2f=r(b2) se2f=r(se2) reject2f=r(r2), reps($numsims) saving(chi2databres, replace) nolegend nodots: chi2datab
	mean b2f se2f reject2f
	
