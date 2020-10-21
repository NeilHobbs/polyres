#' @title Implement  the selection costs when the insecticide is present in the treated site.
#' 
#' @description
#' Implements the selection costs when insecticide is present in the treated site Does not include dispersal effect. 
#' Gives the resistance intensity in the next generation afer selection costs and before migration in the treated site. 
#' 
#' @param exposure.scaling.factor
#' @param nsim
#' @param minimum.insecticide.resistance.hertitability
#' @param maximum.insecticide.resistance.hertitability
#' @param minimum.male.insecticide.exposure
#' @param maximum.male.insecticide.exposure
#' @param minimum.female.insecticide.exposure
#' @param maximum.female.insecticide.exposure
#' @param fitness.cost
#' 
#' @return resistance.intensity.values

#This function is used when the insecticide is deployed
insecticide_deployed_selection_cost = function(
                                  exposure.scaling.factor = 10,
                                  nsim = 1000, 
                                  minimum.insecticide.resistance.hertitability = 0.05, 
                                  maximum.insecticide.resistance.hertitability = 0.30,
                                  minimum.male.insecticide.exposure = 0,
                                  maximum.male.insecticide.exposure = 1, 
                                  minimum.female.insecticide.exposure = 0.4, 
                                  maximum.female.insecticide.exposure = 0.9,
                                  resistance.cost = 0.1,
                                  initial.resistance.intensity = 0){ #The resistance intensity in the previous generation. Set zero for generation 1
    
  
  if(0 > minimum.insecticide.resistance.hertitability |minimum.insecticide.resistance.hertitability > 1){stop("minimum.insecticide.resistance.hertitability must be between 0 and 1")}
  if(0 > maximum.insecticide.resistance.hertitability |maximum.insecticide.resistance.hertitability > 1){stop("maximum.insecticide.resistance.hertitability must be between 0 and 1")}
  if(minimum.insecticide.resistance.hertitability > maximum.insecticide.resistance.hertitability){stop("minimum.insecticide.resistance.hertitability is greater than maximum.insecticide.resistance.hertitability")}
  
  if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
  if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}
  
  if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
  if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}
  
    resistance.intensity.values = initial.resistance.intensity + ##The resistance intensity in the previous generation
      
                                 (response_to_insecticide_selection( 
                                   exposure.scaling.factor = exposure.scaling.factor,
                                   nsim = nsim, 
                                   minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                   maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                   minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                   maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                   minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                   maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)) 
#plus sign used as made negative effect put in effect_of_fitness_cost function        
   + (effect_of_fitness_cost(
                            resistance.cost = resistance.cost,
                            exposure.scaling.factor = exposure.scaling.factor,
                            nsim = nsim, 
                            minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                            maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                            minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                            maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                            minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                            maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))
    
    #Resistance intensity cannot be below 0. Set values below zero as zero
    resistance.intensity.values = ifelse(resistance.intensity.values < 0, 0, resistance.intensity.values)
    
    return(resistance.intensity.values)
  
}

#This is equation 7A in the MS