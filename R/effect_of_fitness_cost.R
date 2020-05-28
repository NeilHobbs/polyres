#' This function implements the fitness cost associated with having insecticide resistance.
#' 
#' @param resistance.cost Costs associated with insecticide resistance. Values recommended between 0.01 to 0.2
#' @param nsim number of simulations of the runif functions
#' @param minimum.insecticide.resistance.hertitability minimum heritability of IR (default is 0.05) 
#' @param maximum.insecticide.resistance.hertitability maximum heritability of IR (default is 0.30)
#' @param minimum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 0)
#' @param maximum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 1)
#' @param minimum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.4)
#' @param maximum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.9)
#' 
#' @return fitness.cost change in population resistance intensity.


effect_of_fitness_cost = function(resistance.cost,
                                  exposure.scaling.factor = 10,
                                  nsim = 1000, 
                                  minimum.insecticide.resistance.hertitability = 0.05, 
                                  maximum.insecticide.resistance.hertitability = 0.30,
                                  minimum.male.insecticide.exposure = 0,
                                  maximum.male.insecticide.exposure = 1, 
                                  minimum.female.insecticide.exposure = 0.4, 
                                  maximum.female.insecticide.exposure = 0.9){
                  #Make resistance cost negative. This means minus symbols in future equations must be pluses to prevent double negative 
  fitness.cost = (-resistance.cost) * response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                        nsim = nsim, 
                                                                        minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                                                        maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                                                        minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                        maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                        minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                        maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  return(fitness.cost)
  
}
