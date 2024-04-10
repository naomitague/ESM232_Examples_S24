#' computes profit from power generation
#' @param  price ($/kj)
#' @param  energy (kj/yr)
#' @param  year (when was energy obtained)
#' @param discount rate (default 0.12)
#' @return data frame with estimate of profit
compute_profit_frompower = function(energy, year, price, discount=0.12) {

  # make sure values are reasonable
  if (length(energy) < 1)
    return(NA)
  
  # energy cannot be negative
  if (min(energy ) < 0)
    return(NA)
  
  # generate a unique identifier or scenario number
  scen = seq(from=1, to=length(energy))
  yearprofit = data.frame(scen=scen, energy=energy, year=year)
  yearprofit$net =  yearprofit$energy*price

  # note how discount is passed through to this function
  # remember to normalize the year to start year e.g the first year
  yearprofit= yearprofit %>% 
      mutate(netpre = compute_NPV(value=net, time=year-year[1], discount=discount ))
  
  return(yearprofit)
}
