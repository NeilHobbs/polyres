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

insecticide_deployed_migration_cross_resistance = function(resistance.cost = 0,
                                                           exposure.scaling.factor = 10,
                                                           nsim = 1000, 
                                                           minimum.insecticide.resistance.heritability = 0.05, 
                                                           maximum.insecticide.resistance.heritability = 0.30,
                                                           minimum.male.insecticide.exposure = 0,
                                                           maximum.male.insecticide.exposure = 1, 
                                                           minimum.female.insecticide.exposure = 0.4, 
                                                           maximum.female.insecticide.exposure = 0.9,
                                                           number.of.insecticides,
                                                           currently.tracked.insecticide,
                                                           cross.selection.matrix,
                                                           initial.resistance.intensity,
                                                           initial.refugia.resistance){
  
resistance.intensity.migration = (insectcide_deployed_with_cross_selection(resistance.cost = 0,
                                                                          exposure.scaling.factor = 10,
                                                                          nsim = 1000, 
                                                                          minimum.insecticide.resistance.heritability = 0.05, 
                                                                          maximum.insecticide.resistance.heritability = 0.30,
                                                                          minimum.male.insecticide.exposure = 0,
                                                                          maximum.male.insecticide.exposure = 1, 
                                                                          minimum.female.insecticide.exposure = 0.4, 
                                                                          maximum.female.insecticide.exposure = 0.9,
                                                                          initial.resistance.intensity,
                                                                          currently.deployed.insecticide,
                                                                          number.of.insecticides,
                                                                          cross.selection.matrix)*(1 - 
                                                                            (migration_treatment_to_refugia(nsim = 1000, 
                                                                                                            min.intervention.coverage = 0.1, 
                                                                                                            max.intervention.coverage = 0.9, 
                                                                                                            min.dispersal.rate = 0.1,
                                                                                                            max.dispersal.rate = 0.9))))
+
    (refugia_with_cross_selection(resistance.cost = 0,
                                 exposure.scaling.factor = 10,
                                 nsim = 1000, 
                                 minimum.insecticide.resistance.heritability = 0.05, 
                                 maximum.insecticide.resistance.heritability = 0.30,
                                 minimum.male.insecticide.exposure = 0,
                                 maximum.male.insecticide.exposure = 1, 
                                 minimum.female.insecticide.exposure = 0.4, 
                                 maximum.female.insecticide.exposure = 0.9,
                                 number.of.insecticides,
                                 currently.tracked.insecticide,
                                 cross.selection.matrix) *
    migration_treatment_to_refugia(
      nsim = nsim, #for runif function, coverage and dispersal normally distributed
      min.intervention.coverage = min.intervention.coverage, 
      max.intervention.coverage = max.intervention.coverage, 
      min.dispersal.rate = min.dispersal.rate,
      max.dispersal.rate = max.dispersal.rate))
  
  ##To prevent resistance intensity being less than 0. Sets any values less than zero at zero.
  resistance.intensity.migration = ifelse(resistance.intensity.migration < 0, 0, resistance.intensity.migration) 
  
  return(resistance.intensity.migration) # The resistance intensity after migration and selection costs
  
}


#This is currently equation 7B in the MS.