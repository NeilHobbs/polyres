#' @title The effect of migration on the resistance intensity in the refugia
#' 
#' @param initial.refugia.resistance The current IR intensity in the refugia.
#' @param resistance.cost The cost of having IR
#' @param exposure.scaling.factor factor converting exposure into selection differential (default is 10)
#' @param nsim number of simulations of the runif functions
#' @param minimum.insecticide.resistance.heritability minimum heritability of IR (default is 0.05) 
#' @param maximum.insecticide.resistance.heritability maximum heritability of IR (default is 0.30)
#' @param minimum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 0)
#' @param maximum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 1)
#' @param minimum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.4)
#' @param maximum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.9)
#' @param min.intervention.coverage Proportion of the insect population in the treated intervention area (default is 0.1) 
#' @param max.intervention.coverage Proportion of the insect population in the treated intervention area (default is 0.9)  
#' @param min.dispersal.rate Proportion of population exchanged between the treatment and refugia sites per generation (default 0.1)
#' @param max.dispersal.rate Proportion of the population exchanged between the treatment and refugia sites per generation 
#' 
#' @return track.refugia.resistance A vector length nsim of resistance intensity values in the refugia


refugia_migration_effect_not_deployed_cross_resistance = function(exposure.scaling.factor = 10,
                                                                  nsim = 1000, 
                                                                  minimum.insecticide.resistance.heritability = 0.05, 
                                                                  maximum.insecticide.resistance.heritability = 0.30,
                                                                  minimum.male.insecticide.exposure = 0,
                                                                  maximum.male.insecticide.exposure = 1, 
                                                                  minimum.female.insecticide.exposure = 0.4, 
                                                                  maximum.female.insecticide.exposure = 0.9,
                                                                  min.intervention.coverage = 0.1, 
                                                                  max.intervention.coverage = 0.9, 
                                                                  min.dispersal.rate = 0.1,
                                                                  max.dispersal.rate = 0.9,
                                                                  resistance.cost = 0.1,
                                                                  initial.resistance.intensity = 0,
                                                                  cross.selection.matrix,
                                                                  currently.deployed.insecticide,
                                                                  currently.tracked.insecticide,
                                                                  initial.refugia.resistance){
  
  refugia.selection = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                              resistance.cost = resistance.cost,
                                              exposure.scaling.factor = exposure.scaling.factor,
                                              nsim = nsim, 
                                              minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                              maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  treatment.site.selection = insecticide_not_deployed_indirect_cross_selection(currently.deployed.insecticide = currently.deployed.insecticide,
                                                                               currently.tracked.insecticide = currently.tracked.insecticide,
                                                                               cross.selection.matrix = cross.selection.matrix,
                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                               nsim = nsim, 
                                                                               minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                               maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                               minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                               maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                               minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                               maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                               resistance.cost = resistance.cost,
                                                                               initial.resistance.intensity = initial.resistance.intensity)
    
  
  migration = migration_refugia_to_treatment(min.dispersal.rate = min.dispersal.rate,
                                                    max.dispersal.rate = max.dispersal.rate,
                                                    min.intervention.coverage = min.intervention.coverage, 
                                                    max.intervention.coverage = max.intervention.coverage,
                                                    nsim = nsim)
  
  
  
  
  track.refugia.resistance = (refugia.selection * (1 - migration)) + (treatment.site.selection * migration)
  
  
  #Prevent resistance intensity going below 0
  track.refugia.resistance = ifelse(track.refugia.resistance < 0, yes=  0, no = track.refugia.resistance)
  
  return(track.refugia.resistance)
  
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# 
# refugia_migration_effect_not_deployed_cross_resistance(exposure.scaling.factor = 10,
#                                                        nsim = 1, 
#                                                        minimum.insecticide.resistance.heritability = 1, 
#                                                        maximum.insecticide.resistance.heritability = 1,
#                                                        minimum.male.insecticide.exposure = 1,
#                                                        maximum.male.insecticide.exposure = 1, 
#                                                        minimum.female.insecticide.exposure = 1, 
#                                                        maximum.female.insecticide.exposure = 1,
#                                                        min.intervention.coverage = 1, 
#                                                        max.intervention.coverage = 1, 
#                                                        min.dispersal.rate = 1,
#                                                        max.dispersal.rate = 1,
#                                                        resistance.cost = 0,
#                                                        initial.resistance.intensity = 0,
#                                                        cross.selection.matrix = temp.matrix,
#                                                        currently.deployed.insecticide = 2,
#                                                        currently.tracked.insecticide = 1,
#                                                        initial.refugia.resistance = 0)
#  
# 
# 
# refugia_migration_effect_insecticide_not_deployed(initial.refugia.resistance = 5,
#                                                   initial.resistance.intensity = 10,
#                                                   resistance.cost = 0,
#                                                   exposure.scaling.factor = 10,
#                                                   nsim = 1, 
#                                                   minimum.insecticide.resistance.heritability = 1, 
#                                                   maximum.insecticide.resistance.heritability = 1,
#                                                   minimum.male.insecticide.exposure = 1,
#                                                   maximum.male.insecticide.exposure = 1, 
#                                                   minimum.female.insecticide.exposure = 1, 
#                                                   maximum.female.insecticide.exposure = 1,
#                                                   min.intervention.coverage = 1, 
#                                                   max.intervention.coverage = 1, 
#                                                   min.dispersal.rate = 0.5,
#                                                   max.dispersal.rate = 0.5)
# #This is equation 9B in the MS
# migration_refugia_to_treatment(min.dispersal.rate = 0.5,
#                                                   max.dispersal.rate = 0.5,
#                                                   min.intervention.coverage = 1, 
#                                                   max.intervention.coverage = 1,
#                                                   nsim = 1)
