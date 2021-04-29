#' @title Implement  the selection costs when the insecticide is deployed in a mixture in the intervention site.
#' 
#' @description
#' Implements the selection and fitness costs when the insecticide is deployed in a mixture in the intervention site.
#' Gives the resistance intensity in the next generation afer selection costs and before migration in the intervention site. 
#' 
#' @param exposure.scaling.factor
#' @param nsim
#' @param minimum.insecticide.resistance.heritability
#' @param maximum.insecticide.resistance.heritability
#' @param minimum.male.insecticide.exposure
#' @param maximum.male.insecticide.exposure
#' @param minimum.female.insecticide.exposure
#' @param maximum.female.insecticide.exposure
#' @param resistance.cost
#' @param initial.resistance.intensity = 0,
#' @param intensity.to.other.mixture.part,
#' @param half.population.bioassay.survival.resistance
#' 
#' @return resistance.intensity.values

#This function is used when the insecticide is deployed
insecticide_deployed_selection_cost_mixture = function(exposure.scaling.factor = 10,
                                                       nsim = 1000, 
                                                       minimum.insecticide.resistance.heritability = 0.05, 
                                                       maximum.insecticide.resistance.heritability = 0.30,
                                                       minimum.male.insecticide.exposure = 0,
                                                       maximum.male.insecticide.exposure = 1, 
                                                       minimum.female.insecticide.exposure = 0.4, 
                                                       maximum.female.insecticide.exposure = 0.9,
                                                       resistance.cost = 0.1,
                                                       initial.resistance.intensity = 0,
                                                       intensity.to.other.mixture.part,
                                                       half.population.bioassay.survival.resistance,
                                                       conversion.factor = 0.48,
                                                       intercept = 0.15){#The resistance intensity in the previous generation. Set zero for generation 1
  
  #Warning and error messages
  if(0 > minimum.insecticide.resistance.heritability |minimum.insecticide.resistance.heritability > 1){stop("minimum.insecticide.resistance.heritability must be between 0 and 1")}
  if(0 > maximum.insecticide.resistance.heritability |maximum.insecticide.resistance.heritability > 1){stop("maximum.insecticide.resistance.heritability must be between 0 and 1")}
  if(minimum.insecticide.resistance.heritability > maximum.insecticide.resistance.heritability){stop("minimum.insecticide.resistance.heritability is greater than maximum.insecticide.resistance.heritability")}
  
  if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
  if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}
  
  if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
  if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}
  
  response.to.insecticide.selection = response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                        nsim = nsim, 
                                                                        minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                        maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                        minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                        maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                        minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                        maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) 
  
  
  #plus sign used as made negative effect put in effect_of_fitness_cost function        
  effect.of.fitness.cost = effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                  exposure.scaling.factor = exposure.scaling.factor,
                                                  nsim = nsim, 
                                                  minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                  maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                  minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                  maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                  minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                  maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  
  survival.to.other.insecticide = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                  mean.population.resistance = intensity.to.other.mixture.part,
                                                                  michaelis.menten.slope = 1, 
                                                                  half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                  sd.population.resistance = 0, 
                                                                  nsim = nsim)
  
  
  field.survival = convert_bioassay_survival_to_field(bioassay.survival = survival.to.other.insecticide,
                                                      conversion.factor = conversion.factor,
                                                      intercept = intercept)
  
  #plus sign used as made negative effect put in effect_of_fitness_cost function   
  resistance.intensity.values = initial.resistance.intensity + ((response.to.insecticide.selection + effect.of.fitness.cost) * field.survival)
  
  #Resistance intensity cannot be below 0. Set values below zero as zero
  resistance.intensity.values = ifelse(resistance.intensity.values < 0, yes = 0, no = resistance.intensity.values)
  
  return(resistance.intensity.values)
  
}





