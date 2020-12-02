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
#' 
#' @return resistance.intensity.migration A vector of length nsim

insecticide_not_deployed_migration_cross_resistance = function(resistance.cost = 0,
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
                                                           currently.deployed.insecticide){
  
 
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
  
  cross.selection.treatment = insecticide_not_deployed_indirect_cross_selection(resistance.cost = resistance.cost,
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
                                                                                 currently.tracked.insecticide = currently.tracked.insecticide)
      
  
  
  migration.values = migration_treatment_to_refugia(nsim = nsim,
                                                    min.intervention.coverage = min.intervention.coverage,
                                                    max.intervention.coverage = max.intervention.coverage,
                                                    min.dispersal.rate = min.dispersal.rate,
                                                    max.dispersal.rate = max.dispersal.rate)
  
  
  
  resistance.intensity.migration = (cross.selection.treatment * (1 -migration.values)) + (selection.cost.refugia * migration.values)
  
  ##To prevent resistance intensity being less than 0. Sets any values less than zero at zero.
  resistance.intensity.migration = ifelse(resistance.intensity.migration < 0, 0, resistance.intensity.migration)
  
  #return(resistance.intensity.migration) # The resistance intensity after migration and selection costs
  return(resistance.intensity.migration)
  
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
#
# insecticide_not_deployed_migration_cross_resistance(resistance.cost = 0,
#                                                     exposure.scaling.factor = 10,
#                                                     nsim = 1, 
#                                                     minimum.insecticide.resistance.heritability = 1, 
#                                                     maximum.insecticide.resistance.heritability = 1,
#                                                     minimum.male.insecticide.exposure = 1,
#                                                     maximum.male.insecticide.exposure = 1, 
#                                                     minimum.female.insecticide.exposure = 1, 
#                                                     maximum.female.insecticide.exposure = 1,
#                                                     min.intervention.coverage = 1, 
#                                                     max.intervention.coverage = 1, 
#                                                     min.dispersal.rate = 1,
#                                                     max.dispersal.rate = 1,
#                                                     currently.tracked.insecticide = 1,
#                                                     cross.selection.matrix = temp.matrix,
#                                                     initial.resistance.intensity = 8,
#                                                     initial.refugia.resistance = 1000,
#                                                     currently.deployed.insecticide = 2)
# 
# #This is currently equation 7B in the MS.
# 
# 
# insecticide_not_deployed_migration(initial.resistance.intensity = 8,
#                                    resistance.cost = 0,
#                                    initial.refugia.resistance = 1000,
#                                    exposure.scaling.factor = 10,
#                                    nsim = 1, 
#                                    minimum.insecticide.resistance.heritability = 1, 
#                                    maximum.insecticide.resistance.heritability = 1,
#                                    minimum.male.insecticide.exposure = 1,
#                                    maximum.male.insecticide.exposure = 1, 
#                                    minimum.female.insecticide.exposure = 1, 
#                                    maximum.female.insecticide.exposure = 1, 
#                                    min.intervention.coverage = 1, 
#                                    max.intervention.coverage = 1, 
#                                    min.dispersal.rate = 1,
#                                    max.dispersal.rate = 1)

