#' Power Generation
#'
#' This function computes instantaneous power generation
#’ from a reservoir given its height and flow rate into turbines
#' @param rho Density of water (kg/m3) Default is 1000
#' @param g Acceleration due to gravity (m/sec2) Default is 9.8
#' @param Kefficiency Turbine Efficiency (0-1) Default is 0.8
#' @param height height of water in reservoir (m)
#' @param flow flow rate (m3/sec)
#' @author Naomi
#' @examples power_gen(20, 1)
#' @return Power generation (W/s)


power_gen = function(height, flow, rho=1000, g=9.8, Keff=0.8) {

 

  # calculate power
    result = rho * height * (flow) * g * Keff


  return(result)
}

