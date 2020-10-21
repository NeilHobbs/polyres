#' @title Track the resistance intensity when the insecticide is not deployed
#' 
#' @description Tracks the insecticide resistance intensity when the corresponding insecticide is 
#' not deployed in the treatment site. But before the effective of mosquito migration has taken place. 
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
#' 
#' @return track.resistance.intensity 


insecticide_not_deployed_selection_cost = function(
                                         initial.resistance.intensity,
                                         resistance.cost,
                                         exposure.scaling.factor = 10,
                                         nsim = 1000, 
                                         minimum.insecticide.resistance.hertitability = 0.05, 
                                         maximum.insecticide.resistance.hertitability = 0.30,
                                         minimum.male.insecticide.exposure = 0,
                                         maximum.male.insecticide.exposure = 1, 
                                         minimum.female.insecticide.exposure = 0.4, 
                                         maximum.female.insecticide.exposure = 0.9){
  
  #Error messages to ensure model can only be run with correct parameter values (eg. the proportion are between 0 and 1):
  if(0 > minimum.insecticide.resistance.hertitability |minimum.insecticide.resistance.hertitability > 1){stop("minimum.insecticide.resistance.hertitability must be between 0 and 1")}
  if(0 > maximum.insecticide.resistance.hertitability |maximum.insecticide.resistance.hertitability > 1){stop("maximum.insecticide.resistance.hertitability must be between 0 and 1")}
  if(minimum.insecticide.resistance.hertitability > maximum.insecticide.resistance.hertitability){stop("minimum.insecticide.resistance.hertitability is greater than maximum.insecticide.resistance.hertitability")}
  
  if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
  if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}
  
  if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
  if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}
  
  
  
  track.resistance.intensity = initial.resistance.intensity + #made negative in effect_of_fitness_cost function
                                effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                       exposure.scaling.factor = exposure.scaling.factor,
                                                       nsim = nsim, 
                                                       minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                                       maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                                       minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                       maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                       minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                       maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  #To prevent resistance.intensity being less than zero:
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, 0, track.resistance.intensity)
  
  return(track.resistance.intensity)
}

#This is equation 8A in the MS