#'@title 
#'
#'
#'@param exposure.scaling.factor = 10,
#'@para nsim = 1000, 
#'@param minimum.insecticide.resistance.heritability = 0.05, 
#'@para maximum.insecticide.resistance.heritability = 0.30,
#'@param minimum.male.insecticide.exposure = 0,
#'@param maximum.male.insecticide.exposure = 1, 
#'@param minimum.female.insecticide.exposure = 0.4, 
#'@param maximum.female.insecticide.exposure = 0.9,
#'@param resistance.cost = 0.1,
#'@param initial.resistance.intensity = 0,
#'@param intensity.to.other.mixture.part,
#'@param half.population.bioassay.survival.resistance,
#'@param currently.deployed.insecticide,
#'@param cross.selection.matrix,
#'@param number.of.insecticides,
#'@param conversion.factor = 0.48,
#'@param intercept = 0.15

#See insecticide_deployed_selection_cost_cross_selection and
  #insecticide_deployed_selection_cost_mixture

insecticide_deployed_selection_cost_mixture_cross_selection = function(exposure.scaling.factor = 10,
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
                                                                       number.of.insecticides,
                                                                       conversion.factor = 0.48,
                                                                       intercept = 0.15,
                                                                       other.mixture.part){
  
  
  previous.intensity = initial.resistance.intensity
  
  direct.selection = response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                   nsim = nsim, 
                                                                   minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                   maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                   minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                   maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                   minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                   maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
    effect_of_fitness_cost(resistance.cost = resistance.cost,
                           exposure.scaling.factor = exposure.scaling.factor,
                           nsim = nsim, 
                           minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                           maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                           minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                           maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                           minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                           maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  
  indirect.selection = (response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                         nsim = nsim, 
                                                         minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                         maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                         minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                         maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                         minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                         maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
    effect_of_fitness_cost(resistance.cost = resistance.cost,
                           exposure.scaling.factor = exposure.scaling.factor,
                           nsim = nsim, 
                           minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                           maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                           minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                           maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                           minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                           maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))*cross.selection.matrix[other.mixture.part, currently.deployed.insecticide]
  
  
  
  survival.to.other.insecticide = convert_bioassay_survival_to_field(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                          mean.population.resistance = intensity.to.other.mixture.part,
                                                                                                          michaelis.menten.slope = 1, 
                                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                          sd.population.resistance = 0, 
                                                                                                          nsim = nsim),
                                                      conversion.factor = conversion.factor,
                                                      intercept = intercept)
  
  survival.tracked.insecticide = convert_bioassay_survival_to_field(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                         mean.population.resistance = initial.resistance.intensity,
                                                                                                                         michaelis.menten.slope = 1, 
                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                         sd.population.resistance = 0, 
                                                                                                                         nsim = nsim),
                                                                     conversion.factor = conversion.factor,
                                                                     intercept = intercept)
  
  track.resistance.intensity = sum(previous.intensity, (direct.selection*survival.to.other.insecticide), (indirect.selection*survival.tracked.insecticide))
    
    
  return(track.resistance.intensity)
}

