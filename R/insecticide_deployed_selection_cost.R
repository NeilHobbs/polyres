#' Implements the selection costs when insecticide is present in the treated area. Does not include dispersal. 
#' Equation 7A 
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
                                  initial.resistance.intensity = 0){
    
    
    resistance.intensity.values = initial.resistance.intensity + 
                                 (response_to_insecticide_selection(
                                   exposure.scaling.factor = exposure.scaling.factor,
                                   nsim = nsim, 
                                   minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                   maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                   minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                   maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                   minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                   maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)) #plus sign used as made negative in function
        
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
    
    #Resistance intensity cannot be below 0.
    resistance.intensity.values = ifelse(resistance.intensity.values < 0, 0, resistance.intensity.values)
    
    return(resistance.intensity.values)
    
}
