#'  Simple population growth
#' @param T  period of growth
#' @param P initial population
#' @param r intrinsic growth rate
#' @param K maximum pop (carrying capacity)
#' @return population at time T
#'
exppop = function(T,P0,r, K) {
    # analytical calculation of population
    P = P0 * exp(r*T)
    # check to see if greater than carrying capacity
    if (P > K) {
      P = K
    }
    return(P)
}
