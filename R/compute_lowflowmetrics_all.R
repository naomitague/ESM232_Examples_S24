#' lowflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param low_flow_months which to use default (August 8)
#' @param wts (vector of 4 for annual_min_err, annual_min_corr, low_month_cor, low_month_err)
#' @param max_err_annual_min
#' @param max_err_low_month
#' @return annual_min_err, annual_min_corr, low_month_cor, low_month_err


compute_lowflowmetrics_all = function(m,o, month, day, year,wy, low_flow_months=8,
                                      max_err_annual_min=NULL, max_err_low_month=NULL, wts=c(0.25,0.25,0.25,0.25)) {


  flow = cbind.data.frame(m,o, month, day, year,wy)
  # first lets get minimum yearly values

  tmp = flow %>% group_by(wy) %>% dplyr::summarize(mino=min(o), minm=min(m))

  annual_min_err = mean(tmp$minm-tmp$mino)

  annual_min_cor = cor(tmp$minm, tmp$mino)

  if (is.null(max_err_annual_min))
  { max_err_annual_min = 0.5*mean(tmp$mino)}

  # now lets get monthly values
  tmp = flow %>% group_by(month, year) %>% dplyr::summarize(model=sum(m), obs=sum(o))
  # now extract august
  low = subset(tmp, month %in% low_flow_months)
  low_month_err = mean(low$model-low$obs)
  low_month_cor=cor(low$model, low$obs)

  # if user doesn't specify maximum errors use 50% of mean observed values
  if (is.null(max_err_low_month))
  { max_err_low_month = 0.5*mean(low$obs)}

  annual_min_err_trans = max(0,(1-abs(annual_min_err/max_err_annual_min) ))
  low_month_err_trans = max(0,(1-abs(low_month_err/max_err_low_month) ))

    # apply weight (normalize in case they don't sum to 1)
  wts = wts/sum(wts)

  combined = wts[1]*annual_min_err_trans + wts[2]*annual_min_cor+
              wts[3]*low_month_cor + wts[4]*low_month_err_trans

  return(list(annual_min_err=annual_min_err, annual_min_cor=annual_min_cor, low_month_err=low_month_err,
              low_month_cor=low_month_cor, combined=combined))
}
