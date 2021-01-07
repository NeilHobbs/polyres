#' @title The effect of dispersal and resistance costs when an insecticide is deployed in a mixture.
#'
#' @description This function returns the population mean resistance for the treated location, when an
#' insecticide is deployed in a mixture, after migration and selection and fitness costs have happened for that generation.
#' 
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.heritability = 0.05, 
#' @param maximum.insecticide.resistance.heritability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9,
#' @param resistance.cost,
#' @param initial.resistance.intensity,
#' @param min.intervention.coverage = 0.1, 
#' @param max.intervention.coverage = 0.9, 
#' @param initial.refugia.resistance,
#' @param min.dispersal.rate = 0.1
#' @param max.dispersal.rate = 0.9
#' @param intensity.to.other.mixture.part = The resistance intensity of the second part of the insecticide mixture
#' @param half.population.bioassay.survival.resistance = The resistance intensity that gives 50% bioassay survival
#' 
#' @return resistance.intensity.migration A vector of length nsim

insecticide_deployed_migration_mixture = function(exposure.scaling.factor = 10,
                                          nsim = 1000, 
                                          minimum.insecticide.resistance.heritability = 0.05,
                                          maximum.insecticide.resistance.heritability = 0.30,
                                          minimum.male.insecticide.exposure = 0,
                                          maximum.male.insecticide.exposure = 1, 
                                          minimum.female.insecticide.exposure = 0.4, 
                                          maximum.female.insecticide.exposure = 0.9,
                                          resistance.cost = 0,
                                          initial.resistance.intensity,
                                          min.intervention.coverage = 0.1, 
                                          max.intervention.coverage = 0.9, 
                                          initial.refugia.resistance,
                                          min.dispersal.rate = 0.1,
                                          max.dispersal.rate = 0.9,
                                          intensity.to.other.mixture.part,
                                          half.population.bioassay.survival.resistance,
                                          conversion.factor = 0.48,
                                          intercept = 0.15){
  
  selection.treatment = insecticide_deployed_selection_cost_mixture(exposure.scaling.factor = exposure.scaling.factor,
                                                                    nsim = nsim, 
                                                                    minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                    maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                    minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                    maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                    minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                    maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                    resistance.cost = resistance.cost,
                                                                    initial.resistance.intensity = initial.resistance.intensity,
                                                                    intensity.to.other.mixture.part = intensity.to.other.mixture.part,
                                                                    half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                    conversion.factor = conversion.factor,
                                                                    intercept = intercept)
     
  
  migration = migration_treatment_to_refugia(nsim = nsim, #for runif function coverage and dispersal uniformly distributed
                                             min.intervention.coverage = min.intervention.coverage, 
                                             max.intervention.coverage = max.intervention.coverage, 
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate)
  
  
  refugia.selection = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                              resistance.cost = resistance.cost,
                                              exposure.scaling.factor = exposure.scaling.factor,
                                              nsim = nsim, #for runif function: heritability, and insectidide.exposure uniformly distributed
                                              minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                              maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  
  resistance.intensity.migration = (selection.treatment * (1 -migration)) + (refugia.selection * migration)
  ##To prevent resistance intensity being less than 0. Sets any values less than zero at zero.
  resistance.intensity.migration = ifelse(resistance.intensity.migration < 0, 0, resistance.intensity.migration) 
  
  return(resistance.intensity.migration) # The resistance intensity after migration and selection costs
  
}


#This is currently equation 7B in the MS.
