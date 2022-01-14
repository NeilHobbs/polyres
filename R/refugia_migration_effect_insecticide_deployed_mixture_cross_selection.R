#Equation 9b (parsed 7a(iii))

refugia_migration_effect_insecticide_deployed_mixture_cross_selection = function(exposure.scaling.factor = 10,
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
                                                                                 currently.deployed.insecticide,
                                                                                 cross.selection.matrix,
                                                                                 min.intervention.coverage = 0.1, 
                                                                                 max.intervention.coverage = 0.9, 
                                                                                 min.dispersal.rate = 0.1,
                                                                                 max.dispersal.rate = 0.9,
                                                                                 initial.refugia.resistance,
                                                                                 number.of.insecticides,
                                                                                 conversion.factor = 0.48,
                                                                                 intercept = 0.15){
  
  refugia.intensity = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                              resistance.cost = resistance.cost[currently.deployed.insecticide],
                                              exposure.scaling.factor = exposure.scaling.factor,
                                              nsim = nsim, 
                                              minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability[currently.deployed.insecticide], 
                                              maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability[currently.deployed.insecticide],
                                              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  intervention.intensity = insecticide_deployed_selection_cost_mixture_cross_selection(exposure.scaling.factor = exposure.scaling.factor,
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
                                                                                       currently.deployed.insecticide = currently.deployed.insecticide,
                                                                                       cross.selection.matrix = cross.selection.matrix,
                                                                                       number.of.insecticides = number.of.insecticides,
                                                                                       conversion.factor = conversion.factor,
                                                                                       intercept = intercept)
  
  migration = migration_refugia_to_treatment(nsim = nsim, 
                                             min.intervention.coverage = min.intervention.coverage, 
                                             max.intervention.coverage = max.intervention.coverage, 
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate)
  
  
  update.refugia.intensity = ((refugia.intensity * (1-migration)) + (intervention.intensity * migration))
  
  return(update.refugia.intensity)
}
