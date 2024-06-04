
#' compute net profit'
#'
#' Function to compute profits
#' @param T annual temperature
#' @param P annual precip
#' @param irr
#' @param discount
#' @param price  of fruit
#' @param cost of irrigation
#' @return profit


compute_profit = function(T,P, irr, discount, price, cost, crop.pars) {
  
  total=0.0;
  nyears = length(T)
  
  yields= compute_yield(T=T,P=P, irr=irr, crop.pars)
  income = price * yields
  costs = rep(irr * cost, times=nyears)
  net = income-costs
  
  for (i in 1: nyears) {
    total = total+compute_NPV(net[i], i, discount)
  }
  
  return(total)
}
