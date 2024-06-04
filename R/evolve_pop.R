#' Population Evolution using Leslie Matrix
#' Evolve a population
#' @param fertility fertility rates
#' @param survivability survivability rates
#' @param initialpop initial population
#' @param nstep number of time steps
#' @return population structure for each time step (OR error message if population cannot be defined)


evolve_pop = function(fertility, survivability, initialpop, nstep) {


nclasses = length(fertility)

# make sure inputs are in the right format
if ((nclasses!=length(survivability) ))
{ return(sprintf("fertility %d doesn’t match survivability %d", 
                 nclasses, length(survivability))) }

if ((nclasses!=length(initialpop) ))
{ return(sprintf("population initialization %d  doesn’t match fertility %d ", length(initialpop),
         length(fertility))) }

#initialize the Leslie matrix
leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
leslie_matrix[,] = 0.0
leslie_matrix[1,] = fertility

for (i in 1:(nclasses-1)) {
leslie_matrix[i+1,i] = survivability[i]
}
leslie_matrix[nclasses,nclasses] = survivability[nclasses]

# create an matrix to store population structure
pop_structure = matrix(nrow=nclasses, ncol=nstep)
total_pop = rep(0, times=nstep)
pop_structure[,1] = initialpop


for (i in 2:nstep) {

total_pop[i]=sum(pop_structure[,i-1])
pop_structure[,i] = leslie_matrix %*% pop_structure[,i-1]

}

return(list(popbyage=pop_structure, poptot=total_pop))
}
