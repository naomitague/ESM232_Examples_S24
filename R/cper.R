#' cper
#'
#' Compute a performance measure (0-1) between observation and model
#' based on both NSE and relative error
#' @param  m  model estimates
#' @param  o  observations
#' @param  weight.nse weighting to give NSE metric
#' @param  weight.relerr weighting to give relative error metric
#' @return  combined 0-1 performance measure


cper = function(m,o,weight.nse=0.5, weight.relerr=0.5) {

  nse = nse(m,o)
  mnse = max(nse,0)

  rel.err = relerr(m,o)
  merr = 1.0-min(1.0, abs(rel.err)/max(abs(rel.err)))

  combined = weight.nse*mnse + weight.relerr*merr

  return(combined)

}

