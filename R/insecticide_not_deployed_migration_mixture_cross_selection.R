
#This is equation 8B (parsed equation 8aiii)


insecticide_not_deployed_migration_mixture_cross_selection = function(nsim,
                                                                      min.intervention.coverage,
                                                                      max.intervention.coverage,
                                                                      min.dispersal.rate,
                                                                      max.dispersal.rate,
                                                                      initial.refugia.resistance,
                                                                      resistance.cost,
                                                                      exposure.scaling.factor = 10,
                                                                      minimum.insecticide.resistance.heritability = 0.05, 
                                                                      maximum.insecticide.resistance.heritability = 0.30,
                                                                      minimum.male.insecticide.exposure = 0,
                                                                      maximum.male.insecticide.exposure = 1, 
                                                                      minimum.female.insecticide.exposure = 0.4, 
                                                                      maximum.female.insecticide.exposure = 0.9,
                                                                      initial.resistance.intensity = 0,
                                                                      cross.selection.matrix,
                                                                      deployed.mixture.1,
                                                                      deployed.mixture.2,
                                                                      currently.tracked.insecticide,
                                                                      conversion.factor = 0.48,
                                                                      intercept = 0.15){
  
  refugia.intensity = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                              resistance.cost,
                                              exposure.scaling.factor = exposure.scaling.factor,
                                              nsim = nsim, 
                                              minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                              maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  intervention.intensity = insecticide_not_deployed_selection_cost_mixture_cross_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                                           nsim = nsim, 
                                                                                           minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                                           maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                                           minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                                           maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                                           minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                                           maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                                           resistance.cost = resistance.cost,
                                                                                           initial.resistance.intensity = initial.resistance.intensity,
                                                                                           cross.selection.matrix = cross.selection.matrix,
                                                                                           deployed.mixture.1 = deployed.mixture.1,
                                                                                           deployed.mixture.2 = deployed.mixture.2,
                                                                                           currently.tracked.insecticide = currently.tracked.insecticide,
                                                                                           conversion.factor = conversion.factor,
                                                                                           intercept = intercept)
  
  migration = migration_treatment_to_refugia(nsim = nsim, 
                                             min.intervention.coverage = min.intervention.coverage, 
                                             max.intervention.coverage = max.intervention.coverage, 
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate)
  
  
  update.intervention.intensity  = (intervention.intensity * (1 - migration)) + (refugia.intensity * migration)
  
  return(update.intervention.intensity)
}

