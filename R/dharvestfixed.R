
#'  Logistic population growth derivative with harvesting
#' @param time time since start
#' @param P population
#' @param parms - as list with three values, r, K, harv
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @param h harvest rate
#' @param mincarbon minimum carbon to allow harvest
#' @return derivative of population with time

dharvestfixed= function(Time, biomass, parms) {

  # reduce harvest rate as you get below minimum carbon

	if (biomass < parms$mincarbon)
	  db = parms$r*biomass*(1-biomass/parms$K)
	else
	  db = parms$r * biomass * (1- biomass/parms$K) - parms$harv

	return(list(db))
}
