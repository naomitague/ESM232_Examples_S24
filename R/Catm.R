#' Compute Atmospheric Conductance
#'
#' THis function atmospheric conductance as a function of windspeed, and vegetation cahracteristics
#' @param       v windspeed (m/s)
#' @param      height vegetation height (m)
#' @param       zm measurement height of wind (m) (default 2m)
#' @param      k_o scalar for roughness (default 0.1)
#' @param      k_d scalar for zero plane displacement (default 0.7)
#' @author Naomi
#'
#' @return  Conductance (mm/s)

Catm = function(v, height, zm_add=2, k_o=0.1, k_d=0.7) {


    zd = k_d*height
    zo = k_o*height

    zm = height+zm_add

    zd = ifelse(zd < 0, 0, zd)
    Ca = ifelse(zo > 0,
     v / (6.25*log((zm-zd)/zo)**2),0)

# convert to mm
    Ca = Ca*1000
   return(Ca)
}
