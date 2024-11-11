findmodel = function(times){
  
  
  #Fitting Poisson process.
  interarrivals = diff(c(0, times))
  fitpp = fitdist(interarrivals, dist= "exp")
  ll1 = fitpp$loglik
  
  #Fitting Hawkes process.
  fith = fit_hawkes(times, parameters = c(mu = 1, alpha = 2, beta = 3))
  ll2 = -fith$objective
  
  
  if(ll1 >= ll2){
    return("Poisson")
  }
  
  else if(ll1 < ll2){
    return("Hawkes")
  }
}

results = rep(NA, 1000)

for (i in 1:1000) {
    times = cumsum(rexp(1000,1))
    times = times[times < 500]
  results[i] = findmodel(times)
}

sum(results == "Hawkes")

results = rep(NA, 1000)

for (i in 1:1000) {
  times = cumsum(rexp(1500,1))
  times = times[times < 1000]
  results[i] = findmodel(times)
}

sum(results == "Hawkes")

results = rep(NA, 1000)

  for (i in 1:1000) {
    times = cumsum(rexp(2000,1))
    times = times[times < 1500]
    results[i] = findmodel(times)
  }
  
  sum(results == "Hawkes")