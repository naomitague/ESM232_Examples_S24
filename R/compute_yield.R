
#' compute annual yield'
#' 
#' Function to compute yeild of different fruits as a function of annual temperature and precipitation
#' @param T annual temperature (C)
#' @param T optimal temperature (C)
#' @param P annual precipitation (mm)
#' @param max.water  maximum water requirement (mm)
#' @param ts slope on temperature
#' @param tp slope on precipitation
#' @param base.yield baseline yield  (kg)
#' @param irr  irrigation in (mm) 
#' @return yield in kg


compute_yield = function(T, P, irr, crop.pars) {

with(as.list(crop.pars), {

nyears=length(T)
irr.peryear = rep(irr, times=nyears)
water.input = P+irr.peryear;
yield = ifelse(water.input < max.water,
	tp*water.input - ts*abs(T-Topt) + base.yield,
	tp*max.water - ts*abs(T-Topt) + base.yield )
yield=pmax(yield,0)
return(yield)
})

}

