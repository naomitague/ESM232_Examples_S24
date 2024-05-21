#' Lorenz
#'
#' classic function initially developed to estimate meteorlogy by Lorenz
#' used to illustrate extreme sensitivity to initial conditions
#' @param t  time (days)
#' @param pt
#'  \emph{x} dim1
#'  \emph{y} dim2
#'  \emph{z} dim3
#' @param parms
#'  \emph{a}
#'  \emph{b}
#'  \emph{c}
#' @examples
#'
#'
#' pars = list(a=10,b=28,c=8/3)
#' lorenz(pt=list(x=0,y=0,z=1),parms=pars)
#'
#' res = ode(func=lorenz, c(x=0.1,y=0,z=0), times=seq(0,50,by=0.01), parms=pars)
#' @return  a 3-dimensional equation
#' \describe{
#' \item{dx}{rate of change in x dimension}
#' \item{dy}{rate of change in y dimension}
#' \item{dz}{rate of change in z dimension}
#'}

lorenz = function(t, pt, parms) {
  with(as.list(c(pt,parms)), {
  dx = a*(y-x)
  dy = x*(b-z)-y
  dz = x*y-c*z

  return(list(c(dx,dy,dz)))
  })
}
