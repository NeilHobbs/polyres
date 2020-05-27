#' This function implements the fitness cost associated with having insecticide resistance.
#' 
#' @param resistance.cost Costs associated with insecticide resistance. Values can be 0.01 to 0.2
#' @param resistance.selection.response Value input from response_to_insecticide_selection.
#' 
#' @return fitness.cost change in population resistance intensity


#[NB set a check in the code so that z_bar cannot fall below 0 when the insecticide is not being deployed] 

effect_of_fitness_cost = function(resistance.cost,
                                  exposure.scaling.factor = 10,
                                  nsim = 1000, 
                                  minimum.insecticide.resistance.hertitability = 0.05, 
                                  maximum.insecticide.resistance.hertitability = 0.30,
                                  minimum.male.insecticide.exposure = 0,
                                  maximum.male.insecticide.exposure = 1, 
                                  minimum.female.insecticide.exposure = 0.4, 
                                  maximum.female.insecticide.exposure = 0.9){
  
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
