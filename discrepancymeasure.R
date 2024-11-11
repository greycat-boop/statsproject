interarrivaltimes = rexp(100, rate = 1)

a = min(interarrivaltimes)
b = max(interarrivaltimes)

epsilon = 1e-04

lambdazero = 3
expside = function(i){
  exp(-1 * lambdazero * epsilon * i) * (exp(lambdazero * epsilon) - 1)
}

j = length(interarrivaltimes)



i= seq(1, b * (1/epsilon), by = 1)


empside = function(i){
  sum(ifelse(interarrivaltimes > (epsilon * (i -1)) & interarrivaltimes <= (epsilon * i), 1, 0)) / j
}


measuretotal = 0
for(point in i){
  
  add = min(expside(point), empside(point))
  measuretotal = measuretotal + add
}

library(stelfi)


arrivaltimes = sim_hawkes(mu = 3, alpha = 0.2, beta = 15, n = 500)
interarrivaltimes = diff(c(0, arrivaltimes))