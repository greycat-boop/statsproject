a= read.csv("earquake.csv")

a.times = unique(sort(a$hours))

findmodel = function(times){
  
  
  #Fitting Poisson process.
  interarrivals = diff(c(0, times))
  fitpp = fitdist(interarrivals, dist= "exp")
  ll1 = fitpp$loglik
  
  #Fitting Hawkes process.
  fith = fit_hawkes(times, parameters = c(mu = 1, alpha = 2, beta = 3))
  ll2 = -fith$objective
  m = measureemp(times, fitpp$estimate)
  
  np1 = 21 * m - 2 * ll1
  np2 = 21* 3 * m - 2 * ll2
  
  if(np1 < np2){
    return("Poisson")
  }
  
  else if(np2 < np1){
    return("Hawkes")
  }
}

findmodel(a.times)