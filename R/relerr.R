#' relerr
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @return relerr


relerr = function(m,o) {

  err = m-o
  meanobs = mean(o)
  meanerr = mean(err)

  res = meanerr/meanobs
  return(res)
}
