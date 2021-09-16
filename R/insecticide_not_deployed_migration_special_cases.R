#' @title The effect of dispersal and resistance costs when the insecticide is deployed.
#'
#' @description 
#' This function returns the population mean resistance for the treated location, when
#' insecticide is deployed, after migration and selection costs.
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
#' @param cross.selection.matrix = a matrix holding the cross selection correlation values
#' @param current.insecticide.efficacy = The insecticide efficacy of insecticide i at time since deployment τ defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.
#' @param intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#' @param conversion.factor = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#' @param sim.array = the array that holds the simulation data
#' @param current.generation = Current generation in the simulation
#' @param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#' 
#' @return resistance.intensity.migration A vector of length nsim

insecticide_not_deployed_migration_special_cases = function(resistance.cost = 0,
                                                            exposure.scaling.factor = 10,
                                                            nsim = 1000, 
                                                            minimum.insecticide.resistance.heritability = 0.05, 
                                                            maximum.insecticide.resistance.heritability = 0.30,
                                                            minimum.male.insecticide.exposure = 0,
                                                            maximum.male.insecticide.exposure = 1, 
                                                            minimum.female.insecticide.exposure = 0.4, 
                                                            maximum.female.insecticide.exposure = 0.9,
                                                            min.intervention.coverage = 0.1, 
                                                            max.intervention.coverage = 0.9, 
                                                            min.dispersal.rate = 1,
                                                            max.dispersal.rate = 1,
                                                            currently.tracked.insecticide,
                                                            cross.selection.matrix,
                                                            initial.resistance.intensity,
                                                            initial.refugia.resistance,
                                                            currently.deployed.insecticide,
                                                            current.insecticide.efficacy,
                                                            intercept,
                                                            conversion.factor,
                                                            sim.array,
                                                            current.generation,
                                                            half.population.bioassay.survival.resistance,
                                                            insecticide.suppression){
  
  
  selection.cost.refugia = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                                   resistance.cost = resistance.cost,
                                                   exposure.scaling.factor = exposure.scaling.factor,
                                                   nsim = nsim, 
                                                   minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                   maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                   minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                   maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                   minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                   maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  cross.selection.treatment = insecticide_not_deployed_special_cases(resistance.cost = resistance.cost,
                                                                     exposure.scaling.factor = exposure.scaling.factor,
                                                                     nsim = nsim,
                                                                     minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability,
                                                                     maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                     minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                     maximum.male.insecticide.exposure = maximum.male.insecticide.exposure,
                                                                     minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
                                                                     maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                     initial.resistance.intensity = initial.resistance.intensity,
                                                                     currently.deployed.insecticide = currently.deployed.insecticide,
                                                                     cross.selection.matrix = cross.selection.matrix,
                                                                     currently.tracked.insecticide = currently.tracked.insecticide,
                                                                     current.insecticide.efficacy = current.insecticide.efficacy)
  
   proportion.remaining = ifelse(insecticide.suppression == TRUE,
                                  yes = calculate_insecticide_population_suppression(minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
                                                                                     maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                                     nsim = nsim,
                                                                                     intercept = intercept,
                                                                                     conversion.factor = conversion.factor,
                                                                                     current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                     currently.deployed.insecticide = currently.deployed.insecticide,
                                                                                     sim.array = sim.array,
                                                                                     current.generation = current.generation,
                                                                                     half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                  no = 1)
  

  staying.in.treatment = (1 - migration_treatment_to_refugia(nsim = nsim,
                                                            min.intervention.coverage = min.intervention.coverage,
                                                            max.intervention.coverage = max.intervention.coverage,
                                                            min.dispersal.rate = min.dispersal.rate,
                                                            max.dispersal.rate = max.dispersal.rate))*proportion.remaining
  
  
  migrating.from.refugia = migration_refugia_to_treatment(nsim = nsim,
                                                          min.intervention.coverage = min.intervention.coverage,
                                                          max.intervention.coverage = max.intervention.coverage,
                                                          min.dispersal.rate = min.dispersal.rate,
                                                          max.dispersal.rate = max.dispersal.rate)
  
 
  
  numerator = (cross.selection.treatment * staying.in.treatment) + (selection.cost.refugia * migrating.from.refugia)
  #denominator = 0 when: migraton = 0 (e.g. no dispersal)AND population.suppression = 0. Can only happen when no dispersal.
  
  denominator = staying.in.treatment + migrating.from.refugia

  resistance.intensity.migration = ifelse(denominator == 0,
                                          yes = numerator,
                                          no = numerator/denominator
                                          )


  #To prevent resistance intensity being less than 0. Sets any values less than zero at zero.
  resistance.intensity.migration = ifelse(resistance.intensity.migration < 0, 
                                          yes = 0, no = resistance.intensity.migration)

  
  resistance.intensity.migration = ifelse(is.na(resistance.intensity.migration),
                                          yes = 0,
                                          no = resistance.intensity.migration)
  
  #return(resistance.intensity.migration) # The resistance intensity after migration and selection costs
  return(resistance.intensity.migration)
  
}


