findmodel = function(times){
  
  
  #Fitting Poisson process.
  interarrivals = diff(c(0, times))
  fitpp = fitdist(interarrivals, dist= "exp")
  ll1 = fitpp$loglik
  bic1 = log(length(times)) - 2 * ll1
  
  #Fitting Hawkes process.
  fith = fit_hawkes(times, parameters = c(mu = 1, alpha = 2, beta = 3))
  ll2 = fith$objective
  bic2 = 3 * log(length(times)) + 2 *ll2
  
  if(bic1 <= bic2){
    return("Poisson")
  }
  
  else if(bic1 > bic2){
    return("Hawkes")
  }
}

results = rep(NA, 1000)

for (i in 1:1000) {
  times = cumsum(rexp(3000,1))
  times = times[times < 1500]
  results[i] = findmodel(times)
}

sum(results == "Hawkes")