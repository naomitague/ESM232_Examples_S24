#' lowflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param low_flow_months which to use default (August 8)
#' @return annual_min_err, annual_min_corr, low_month_cor, low_month_err

compute_lowflowmetrics = function(m,o, month, day, year,wy, low_flow_months=8) {

  flow = cbind.data.frame(m,o, month, day, year,wy)
  # first lets get minimum yearly values

  tmp = flow %>% group_by(wy) %>% summarize(mino=min(o), minm=min(m))

  annual_min_err = mean(tmp$minm-tmp$mino)

  annual_min_cor = cor(tmp$minm, tmp$mino)

  # now lets get monthly values
  tmp = flow %>% group_by(month, year) %>% summarize(model=sum(m), obs=sum(o))
  # now extract august
  low = subset(tmp, month %in% low_flow_months)
  low_month_err = mean(low$model-low$obs)
  low_month_cor=cor(low$model, low$obs)

  return(list(annual_min_err=annual_min_err, annual_min_cor=annual_min_cor, low_month_err=low_month_err,
              low_month_cor=low_month_cor))
}
