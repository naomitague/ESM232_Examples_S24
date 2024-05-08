#'  More complex population growth
#' @param T  period of growth
#' @param P initial population
#' @param parms$r - base growth rate
#' @parms parms$K - carrying capacity
#' @return change in population
#'
dexppopK = function(time, P, parms) {

  # compute rate of change of population
  dexpop = parms$r*P * (1-P/parms$K)


  return(list(dexpop))
}
