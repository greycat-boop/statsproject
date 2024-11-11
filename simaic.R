findmodel = function(times){
  
  
  #Fitting Poisson process.
  interarrivals = diff(c(0, times))
  fitpp = fitdist(interarrivals, dist= "exp")
  ll1 = fitpp$loglik
  aic1 = (2)*(1) + 2 * -ll1
  
  #Fitting Hawkes process.
  fith = fit_hawkes(times, parameters = c(mu = 1, alpha = 2, beta = 3))
  ll2 = fith$objective
  aic2 = (2)*(3) + 2 * ll2
  
  if(aic1 <= aic2){
    return("Poisson")
  }
  
  else if(aic1 > aic2){
    return("Hawkes")
  }
}

results = rep(NA, 1000)

for (i in 1:1000) {
  times = cumsum(rexp(2000,2))
  times = times[times < 500]
  results[i] = findmodel(times)
}

sum(results == "Hawkes")


results = rep(NA, 1000)

for (i in 1:1000) {
  times = cumsum(rexp(3000,2))
  times = times[times < 1000]
  results[i] = findmodel(times)
}

sum(results == "Hawkes")


results = rep(NA, 1000)

for (i in 1:1000) {
  times = cumsum(rexp(4000,2))
  times = times[times < 1500]
  results[i] = findmodel(times)
}

sum(results == "Hawkes")