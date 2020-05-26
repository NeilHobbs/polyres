#' This is equation 8A
#' 
#' @param initial.resistance.intensity,
#' @param resistance.cost,
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.hertitability = 0.05, 
#' @param maximum.insecticide.resistance.hertitability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9


insecticide_not_deployed_cost = function(initial.resistance.intensity,
                                         resistance.cost,
                                         exposure.scaling.factor = 10,
                                         nsim = 1000, 
                                         minimum.insecticide.resistance.hertitability = 0.05, 
                                         maximum.insecticide.resistance.hertitability = 0.30,
                                         minimum.male.insecticide.exposure = 0,
                                         maximum.male.insecticide.exposure = 1, 
                                         minimum.female.insecticide.exposure = 0.4, 
                                         maximum.female.insecticide.exposure = 0.9){
  
  track.resistance.intensity = initial.resistance.intensity + effect_of_fitness_cost(resistance.cost,
                                                                                     exposure.scaling.factor = exposure.scaling.factor,
                                                                                     nsim = nsim, 
                                                                                     minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                                                                     maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                                                                     minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                                     maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                                     minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                                     maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
}