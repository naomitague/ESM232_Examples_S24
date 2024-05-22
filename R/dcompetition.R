#' Competition
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values popa and popb (populations of a and b competitors)
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{ra} is growth rate of  population a
#'  \emph{rb} is growth rate of  population b
#'  \emph{alphaab} is effect of population b on a's resources
#'  \emph{alphaba} is effect of population a on b's resources
#'  \emph{pmortb}  mortality rate of b population
#'  \emph{pmorta}  mortality rate of b population
#'  \emph{Ka}  carrying capacity of population a
#'  \emph{Kb}. carrying capacity of population b
#' @examples
#'  currpop=c(a=10,b=10)
#  days = seq(from=1,to=100)
#'  pars = c(ra=0.5, rb=0.3, alphaab=1.0, alphaba=1.0,  pmorta=0.2, pmortb=0.1, Ka=100, Kb=200, )
#' res = ode(func=dcompetition, y=currpop, times=days, parms=pars)
#'
#' @return  dcompetition returns a list containing the following components
#' \describe{
#' \item{da}{rate of change of prey populutation}
#' \item{db}{rate of change of preditor populutation}
#'}

dcompetition = function(t, pop, pars) {
with(as.list(c(pars,pop)), {
da = ra*a*(1-(a+alphaab*b)/Ka)
db = rb*b*(1-(b+alphaba*a)/Kb)
return(list(c(da,db)))})
}




