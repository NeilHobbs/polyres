#Equation 9b (parsed 8a(iii))

refugia_migration_effect_insecticide_not_deployed_mixture_special_cases = function(exposure.scaling.factor = 10,
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
                                                                                   cross.selection.matrix,
                                                                                   min.intervention.coverage = 0.1, 
                                                                                   max.intervention.coverage = 0.9, 
                                                                                   min.dispersal.rate = 0.1,
                                                                                   max.dispersal.rate = 0.9,
                                                                                   initial.refugia.resistance,
                                                                                   deployed.mixture.1,
                                                                                   deployed.mixture.2,
                                                                                   currently.tracked.insecticide,
                                                                                   conversion.factor = 0.48,
                                                                                   intercept = 0.15,
                                                                                   efficacy.part.1,
                                                                                   efficacy.part.2,
                                                                                   insecticide.suppression){
  
  refugia.intensity = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                              resistance.cost = resistance.cost,
                                              exposure.scaling.factor = exposure.scaling.factor,
                                              nsim = nsim, 
                                              minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                              maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  intervention.intensity = insecticide_not_deployed_mixtures_special_cases(exposure.scaling.factor = exposure.scaling.factor,
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
                                                                           intercept = intercept,
                                                                           efficacy.part.1 = efficacy.part.1,
                                                                           efficacy.part.2 = efficacy.part.2)
  
  migration = migration_refugia_to_treatment(nsim = nsim, 
                                             min.intervention.coverage = min.intervention.coverage, 
                                             max.intervention.coverage = max.intervention.coverage, 
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate)
  
  
  
  population.suppression = ifelse(insecticide.suppression == TRUE,
                                  yes = calculate_insecticide_population_suppression_mixtures(minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
                                                                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                                              nsim = nsim,
                                                                                              intercept = intercept,
                                                                                              conversion.factor = conversion.factor,
                                                                                              current.insecticide.efficacy.part.1 = efficacy.part.1,
                                                                                              current.insecticide.efficacy.part.2 = efficacy.part.2,
                                                                                              currently.deployed.mixture.part.1 = deployed.mixture.1,
                                                                                              currently.deployed.mixture.part.2 = deployed.mixture.2,
                                                                                              sim.array = sim.array,
                                                                                              current.generation = current.generation,
                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                  no = 1)
  
  
  numerator = ((refugia.intensity * (1-migration)) + ((intervention.intensity * migration)*population.suppression))
  denominator = (1-migration) + (migration*population.suppression)
  
  update.refugia.intensity = ifelse(denominator = 0,
                                    yes = numerator,
                                    no = numerator/denominator)
  
  return(update.refugia.intensity)
}
