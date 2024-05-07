
#'  Logistic population growth derivative
#' @param time time since start
#' @param P population
#' @param parms - as list with two values, r, K
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @return derivative of population with time

dpopgrowth = function(Time, P, parms) {

	dP = parms$r * P * (1- P/parms$K)
	return(list(dP))
}
