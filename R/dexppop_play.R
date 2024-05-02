#'  More complex population growth
#' @param T  period of growth
#' @param P initial population
#' @param parms$r - base growth rate
#' @parms parms$K - carrying capacity
#' @return change in population
#'
dexppop_play = function(time, P, parms) {

  # compute rate of change of population
  dexpop = parms$r*P

  # set rate of change to 0 if P is greater than carrying capacity
  dexpop = ifelse(P > parms$carry_capacity, 0, dexpop)
  return(list(dexpop))
}
