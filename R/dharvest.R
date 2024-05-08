
#'  Logistic population growth derivative with harvesting
#' @param time time since start
#' @param biomass biomass
#' @param parms - as list with three values, r, K, harv
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @param h harvest rate
#' @return db derivative of biomass with time

dharvest= function(Time, biomass, parms) {

	db = parms$r * biomass * (1- biomass/parms$K) - parms$harv*biomass
	return(list(db))
}
