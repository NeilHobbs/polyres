#This is equation 8B (parsed equation 8aiii)


insecticide_not_deployed_migration_mixture_special_cases = function(nsim,
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
                                                                    currently.tracked.insecticide,
                                                                    conversion.factor = 0.48,
                                                                    intercept = 0.15,
                                                                    mixture.part.1,
                                                                    efficacy.mixture.part.1,
                                                                    mixture.part.2,
                                                                    efficacy.mixture.part.2,
                                                                    current.generation,
                                                                    sim.array,
                                                                    insecticide.suppression,
                                                                    deployed.mixture,
                                                                    half.population.bioassay.survival.resistance){
  
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
                                                                           deployed.mixture.1 = mixture.part.1,
                                                                           deployed.mixture.2 = mixture.part.2,
                                                                           currently.tracked.insecticide= currently.tracked.insecticide,
                                                                           conversion.factor = conversion.factor,
                                                                           intercept = intercept,
                                                                           efficacy.part.1 = efficacy.mixture.part.1,
                                                                           efficacy.part.2 = efficacy.mixture.part.2,
                                                                           deployed.mixture = deployed.mixture,
                                                                           current.generation = current.generation,
                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)
  
  proportion.surviving = ifelse(insecticide.suppression == TRUE,
                                  yes = calculate_insecticide_population_suppression_mixtures(minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
                                                                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                                              nsim = nsim,
                                                                                              intercept = intercept,
                                                                                              conversion.factor = conversion.factor,
                                                                                              current.insecticide.efficacy.part.1 = efficacy.mixture.part.1,
                                                                                              current.insecticide.efficacy.part.2 = efficacy.mixture.part.2,
                                                                                              currently.deployed.mixture.part.1 = mixture.part.1,
                                                                                              currently.deployed.mixture.part.2 = mixture.part.2,
                                                                                              sim.array = sim.array,
                                                                                              current.generation = current.generation,
                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                  no = 1) 
  
  staying.in.treatment = (1- migration_treatment_to_refugia(nsim = nsim, 
                                             min.intervention.coverage = min.intervention.coverage, 
                                             max.intervention.coverage = max.intervention.coverage, 
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate))*proportion.surviving
  
  joining.from.refugia = migration_refugia_to_treatment(nsim = nsim, 
                                             min.intervention.coverage = min.intervention.coverage, 
                                             max.intervention.coverage = max.intervention.coverage, 
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate)
  
  
  
  
  numerator = (intervention.intensity * staying.in.treatment) + (refugia.intensity * joining.from.refugia)
  denominator = (staying.in.treatment + joining.from.refugia)
  
  update.intervention.intensity = ifelse(denominator == 0,
                                         yes = numerator,
                                         no = numerator/denominator)
  
  update.intervention.intensity = ifelse(update.intervention.intensity < 0, 
                                         yes = 0,
                                         no = update.intervention.intensity)
  
  update.intervention.intensity = ifelse(is.na(update.intervention.intensity),
                                         yes = 0,
                                         no = update.intervention.intensity)

  return(update.intervention.intensity)
}