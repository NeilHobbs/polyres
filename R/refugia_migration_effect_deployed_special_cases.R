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
#' @param current.insecticide.efficacy = The insecticide efficacy of insecticide i at time since deployment τ defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.
#' @param intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#' @param conversion.factor = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#' @param sim.array = the array that holds the simulation data
#' @param current.generation = Current generation in the simulation
#' @param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#' 
#' 
#' @return track.refugia.resistance A vector length nsim of resistance intensity values in the refugia


refugia_migration_effect_deployed_special_cases = function(exposure.scaling.factor = 10,
                                                           nsim = 1000, 
                                                           minimum.insecticide.resistance.heritability = 0.05, 
                                                           maximum.insecticide.resistance.heritability = 0.30,
                                                           minimum.male.insecticide.exposure = 0,
                                                           maximum.male.insecticide.exposure = 1, 
                                                           minimum.female.insecticide.exposure = 0.4, 
                                                           maximum.female.insecticide.exposure = 0.9,
                                                           resistance.cost = 0.1,
                                                           initial.resistance.intensity = 0,
                                                           cross.selection.matrix,
                                                           currently.deployed.insecticide,
                                                           currently.tracked.insecticide,
                                                           initial.refugia.resistance,
                                                           number.of.insecticides,
                                                           min.intervention.coverage = 0.1, 
                                                           max.intervention.coverage = 0.9, 
                                                           min.dispersal.rate = 0.1,
                                                           max.dispersal.rate = 0.9,
                                                           intercept,
                                                           conversion.factor,
                                                           current.insecticide.efficacy,
                                                           sim.array,
                                                           current.generation,
                                                           half.population.bioassay.survival.resistance,
                                                           insecticide.suppression){
  
  
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
  
  treatment.site.selection = insecticide_deployed_special_cases(currently.deployed.insecticide = currently.deployed.insecticide,
                                                                number.of.insecticides = number.of.insecticides,
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
                                                                initial.resistance.intensity = initial.resistance.intensity,
                                                                current.insecticide.efficacy)
  
  impact.insecticide.selection = ifelse(insecticide.suppression == TRUE,
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
  
  migration = migration_refugia_to_treatment(nsim = nsim,
                                             min.intervention.coverage = min.intervention.coverage,
                                             max.intervention.coverage = max.intervention.coverage,
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate)
  
  
  

  track.refugia.resistance = (refugia.selection * (1 - migration)) + (treatment.site.selection * migration * impact.insecticide.selection)
  
  
  
  
  
  # track.refugia.resistance = (refugia.selection * staying.in.refugia) + (treatment.site.selection * migrating.from.treatment)
  #denominator = staying.in.refugia + migrating.from.treatment

  #If migration = 0, and population supppression = 0 then denominator = 0.


  # track.refugia.resistance = ifelse(denominator == 0,
  #                                   yes = numerator,
  #                                   no = numerator/denominator
  #                                   )


   #Prevent resistance intensity going below 0
  track.refugia.resistance = ifelse(track.refugia.resistance < 0, 0, track.refugia.resistance)
  
  track.refugia.resistance = ifelse(is.na(track.refugia.resistance),
                                    yes = 0,
                                    no = track.refugia.resistance)
  
  return(track.refugia.resistance)
}

# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# 
# 
# refugia_migration_effect_deployed_cross_resistance(
#   exposure.scaling.factor = 10,
#   nsim = 1000,
#   minimum.insecticide.resistance.heritability = 1,
#   maximum.insecticide.resistance.heritability = 1,
#   minimum.male.insecticide.exposure = 1,
#   maximum.male.insecticide.exposure = 1,
#   minimum.female.insecticide.exposure = 1,
#   maximum.female.insecticide.exposure = 1,
#   resistance.cost = 0,
#   initial.resistance.intensity = 10,
#   cross.selection.matrix = temp.matrix,
#   currently.tracked.insecticide = 1,
#   currently.deployed.insecticide = 1,
#   initial.refugia.resistance = 10,
#   number.of.insecticides = 3,
#   min.intervention.coverage = 1,
#   max.intervention.coverage = 1,
#   min.dispersal.rate = 1,
#   max.dispersal.rate = 1)
# #This is equation 9B in the MS
# 
# 
# refugia_migration_effect_insecticide_deployed(
#   exposure.scaling.factor = 10,
#   nsim = 1000,
#   minimum.insecticide.resistance.heritability = 1,
#   maximum.insecticide.resistance.heritability = 1,
#   minimum.male.insecticide.exposure = 1,
#   maximum.male.insecticide.exposure = 1,
#   minimum.female.insecticide.exposure = 1,
#   maximum.female.insecticide.exposure = 1,
#   resistance.cost = 0,
#   initial.resistance.intensity = 10,
#   initial.refugia.resistance = 10,
#   min.intervention.coverage = 1,
#   max.intervention.coverage = 1,
#   min.dispersal.rate = 1,
#   max.dispersal.rate = 1)
