#' @title Implement the selection costs in the refugia
#' 
#' @param initial.refugia.resistance,
#' @param resistance.cost,
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.heritability = 0.05, 
#' @param maximum.insecticide.resistance.heritability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9
#' 
#' @return track.refugia.resistance is the resistance intensity in the refugia (a vector of length nsim).

refugia_selection_costs = function(initial.refugia.resistance,
                                   resistance.cost,
                                   exposure.scaling.factor = 10,
                                   nsim = 1000, 
                                   minimum.insecticide.resistance.heritability = 0.05, 
                                   maximum.insecticide.resistance.heritability = 0.30,
                                   minimum.male.insecticide.exposure = 0,
                                   maximum.male.insecticide.exposure = 1, 
                                   minimum.female.insecticide.exposure = 0.4, 
                                   maximum.female.insecticide.exposure = 0.9){
                                  #plus used as effect_of_fitness_cost does negative
  track.refugia.resistance = initial.refugia.resistance + effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                                          exposure.scaling.factor = exposure.scaling.factor,
                                                                          nsim = nsim, 
                                                                          minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                          maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                          minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                          maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                          minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                          maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  ##To prevent resistance intensity being less than 0
  track.refugia.resistance = ifelse(track.refugia.resistance < 0, 0, track.refugia.resistance) 
  
  return(track.refugia.resistance)
}

#This is equation 9A in the  MS