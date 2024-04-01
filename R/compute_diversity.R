#' Simpson's Species Diversity Index
#'
#' Compute a species diversity index
#' @param species list of species (names, or code)
#' @return value of Species Diversity Index
#' @examples
#' compute_simpson_index(c("butterfly","butterfly","mosquito","butterfly","ladybug","ladybug")))
#' @references
#' http://www.tiem.utk.edu/~gross/bioed/bealsmodules/simpsonDI.html

compute_diversity = function(species) {

  # make sure the species list is a factor
species = as.factor(species)

  # use summary to get frequency counts for each unique species
ssp = summary(species)

  # find the total number of individuals
tt  = sum(ssp)

  # compute the diversity index
diversity = sum((ssp/tt)**2)

  # use the the frequency count (ssp) to get the most common species
mostfreq = names(which.max(ssp))

return(list(simpson=diversity, mostcommon=mostfreq))
}


