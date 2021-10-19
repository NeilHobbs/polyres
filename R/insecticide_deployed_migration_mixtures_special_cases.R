


insecticide_deployed_migration_mixtures_special_cases = function(exposure.scaling.factor = 10,
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
                                                                  initial.refugia.resistance,
                                                                  min.intervention.coverage = 0.1, 
                                                                  max.intervention.coverage = 0.9, 
                                                                  min.dispersal.rate = 0.1,
                                                                  max.dispersal.rate = 0.9,
                                                                  number.of.insecticides,
                                                                  conversion.factor = 0.48,
                                                                  intercept = 0.15,
                                                                  tracked.insecticide.efficacy,
                                                                  other.insecticide.in.mixture,
                                                                  efficacy.of.other.insecticide,
                                                                 insecticide.suppression,
                                                                 current.generation,
                                                                 sim.array){
  
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
  
  
  intervention.intensity = insecticide_deployed_mixtures_special_cases(exposure.scaling.factor = exposure.scaling.factor,
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
                                                                                  intercept = intercept,
                                                                                  tracked.insecticide.efficacy = tracked.insecticide.efficacy,
                                                                                  other.insecticide.in.mixture = other.insecticide.in.mixture,
                                                                                  efficacy.of.other.insecticide = efficacy.of.other.insecticide)
  
   proportion.surviving = ifelse(insecticide.suppression == TRUE,
                                 yes = calculate_insecticide_population_suppression_mixtures(minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
                                                                                             maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                                             nsim = nsim,
                                                                                             intercept = intercept,
                                                                                             conversion.factor = conversion.factor,
                                                                                             current.insecticide.efficacy.part.1 = tracked.insecticide.efficacy,
                                                                                             current.insecticide.efficacy.part.2 = efficacy.of.other.insecticide,
                                                                                             currently.deployed.mixture.part.1 = currently.deployed.insecticide,
                                                                                             currently.deployed.mixture.part.2 = other.insecticide.in.mixture,
                                                                                             sim.array = sim.array,
                                                                                             current.generation = current.generation,
                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                 no = 1)
   
  staying.in.treatment = (1 - migration_treatment_to_refugia(nsim = nsim, 
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
 
 update.intervention.intensity = ifelse(update.intervention.intensity < 0, yes = 0, no = update.intervention.intensity)

 update.intervention.intensity = ifelse(is.na(update.intervention.intensity),
                                        yes = 0,
                                        no = update.intervention.intensity)
 
  return(update.intervention.intensity)
}
