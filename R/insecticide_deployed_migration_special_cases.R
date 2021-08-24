#' @title The effect of dispersal and resistance costs when the insecticide is deployed: special cases - decay, suppression, cross selection
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
#' @param current.insecticide.efficacy = The insecticide efficacy of insecticide i at time since deployment τ defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.
#' @param intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#' @param conversion.factor = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#' @param sim.array = the array that holds the simulation data
#' @param current.generation = Current generation in the simulation
#' @param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#' 
#' @return resistance.intensity.migration A vector of length nsim

insecticide_deployed_migration_special_cases = function(resistance.cost = 0,
                                                        exposure.scaling.factor = 10,
                                                        nsim = 1000, 
                                                        minimum.insecticide.resistance.heritability = 0.05, 
                                                        maximum.insecticide.resistance.heritability = 0.30,
                                                        minimum.male.insecticide.exposure = 0,
                                                        maximum.male.insecticide.exposure = 1, 
                                                        minimum.female.insecticide.exposure = 0.4, 
                                                        maximum.female.insecticide.exposure = 0.9,
                                                        min.dispersal.rate = 0,
                                                        max.dispersal.rate = 1,
                                                        min.intervention.coverage = 0.1, 
                                                        max.intervention.coverage = 0.9,
                                                        number.of.insecticides,
                                                        cross.selection.matrix,
                                                        initial.resistance.intensity,
                                                        initial.refugia.resistance,
                                                        currently.deployed.insecticide,
                                                        current.insecticide.efficacy,
                                                        sim.array,
                                                        current.generation,
                                                        half.population.bioassay.survival.resistance,
                                                        intercept,
                                                        conversion.factor,
                                                        insecticide.suppression){
  
  
  refugia.after.selection = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                              resistance.cost = resistance.cost,
                                              exposure.scaling.factor = exposure.scaling.factor,
                                              nsim = nsim, 
                                              minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                              maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)  
  
  cross.selection.treatment = insecticide_deployed_special_cases(resistance.cost = resistance.cost,
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
                                                                            number.of.insecticides = number.of.insecticides,
                                                                            cross.selection.matrix = cross.selection.matrix,
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
  
  staying.in.treatment = migration_treatment_to_refugia(nsim = nsim,
                                                            min.intervention.coverage = min.intervention.coverage,
                                                            max.intervention.coverage = max.intervention.coverage,
                                                            min.dispersal.rate = min.dispersal.rate,
                                                            max.dispersal.rate = max.dispersal.rate)*proportion.remaining
  

  migrating.from.refugia = migration_refugia_to_treatment(nsim = nsim,
                                                          min.intervention.coverage = min.intervention.coverage,
                                                          max.intervention.coverage = max.intervention.coverage,
                                                          min.dispersal.rate = min.dispersal.rate,
                                                          max.dispersal.rate = max.dispersal.rate)
  
  
  numerator = (cross.selection.treatment*staying.in.treatment) + (migrating.from.refugia*refugia.after.selection)
  denominator =  staying.in.treatment + migrating.from.refugia
  
  intervention.after.migration =  ifelse(denominator == 0,
                                         yes = numerator,
                                         no = numerator/denominator
  )
  
  intervention.after.migration = ifelse(is.na(intervention.after.migration),
                                        yes = 0,
                                        no = intervention.after.migration)
  
  
  intervention.after.migration = ifelse(intervention.after.migration < 0, yes = 0, no = intervention.after.migration)
  
  #return(intervention.after.migration) # The resistance intensity after migration and selection costs
  return(intervention.after.migration)
}

# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 1,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# 
# 
# insecticide_deployed_migration_cross_resistance(resistance.cost = 0,
#                                                 exposure.scaling.factor = 10,
#                                                 nsim = 1,
#                                                 minimum.insecticide.resistance.heritability = 1,
#                                                 maximum.insecticide.resistance.heritability = 1,
#                                                 minimum.male.insecticide.exposure = 1,
#                                                 maximum.male.insecticide.exposure = 1,
#                                                 minimum.female.insecticide.exposure = 1,
#                                                 maximum.female.insecticide.exposure = 1,
#                                                 initial.resistance.intensity = 10,
#                                                 initial.refugia.resistance = 10,
#                                                 min.dispersal.rate = 0.5,
#                                                 max.dispersal.rate = 0.5,
#                                                 min.intervention.coverage = 0.5,
#                                                 max.intervention.coverage = 0.5,
#                                                 number.of.insecticides = 1,
#                                                 currently.tracked.insecticide = 1,
#                                                 cross.selection.matrix = temp.matrix,
#                                                 currently.deployed.insecticide = 1)
# #This is currently equation 7B in the MS.#
# 
# insecticide_deployed_migration(resistance.cost = 0,
#                                exposure.scaling.factor = 10,
#                                nsim = 1,
#                                minimum.insecticide.resistance.heritability = 1,
#                                maximum.insecticide.resistance.heritability = 1,
#                                minimum.male.insecticide.exposure = 1,
#                                maximum.male.insecticide.exposure = 1,
#                                minimum.female.insecticide.exposure = 1,
#                                maximum.female.insecticide.exposure = 1,
#                                initial.resistance.intensity = 10,
#                                initial.refugia.resistance = 10,
#                                min.dispersal.rate = 0.5,
#                                max.dispersal.rate = 0.5,
#                                min.intervention.coverage = 0.5,
#                                max.intervention.coverage = 0.5)
