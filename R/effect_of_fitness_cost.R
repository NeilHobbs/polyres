#' @title Implements the fitness cost associated with having insecticide resistance.
#' 
#' @param resistance.cost Costs associated with insecticide resistance. Values recommended between 0.01 to 0.2
#' @param nsim number of simulations of the runif functions
#' @param minimum.insecticide.resistance.heritability minimum heritability of IR (default is 0.05) 
#' @param maximum.insecticide.resistance.heritability maximum heritability of IR (default is 0.30)
#' @param minimum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 0)
#' @param maximum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 1)
#' @param minimum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.4)
#' @param maximum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.9)
#' 
#' @return fitness.cost  in population resistance intensity. Will be a negative number.


effect_of_fitness_cost = function(resistance.cost = 0,
                                  exposure.scaling.factor = 10,
                                  nsim = 1000, 
                                  minimum.insecticide.resistance.heritability = 0.05, 
                                  maximum.insecticide.resistance.heritability = 0.30,
                                  minimum.male.insecticide.exposure = 0,
                                  maximum.male.insecticide.exposure = 1, 
                                  minimum.female.insecticide.exposure = 0.4, 
                                  maximum.female.insecticide.exposure = 0.9){
                  #Make resistance cost negative. This means minus symbols in future equations must be pluses to prevent double negative 
  fitness.cost = (-resistance.cost) * response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                        nsim = nsim, 
                                                                        minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                        maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                        minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                        maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                        minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                        maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  #Will be a negative number (unless zero).
  return(fitness.cost)
  
}

#This function is currently Equation 5 in the MS
