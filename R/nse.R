#' nse
#'
#' Compute NSE between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @return nse


nse = function(m,o) {

  err = m-o
  meanobs = mean(o)
  mse = sum(err*err)
  ovar = sum((o-meanobs)*(o-meanobs))
  nse = 1.0-mse/ovar

  return(nse)
}

