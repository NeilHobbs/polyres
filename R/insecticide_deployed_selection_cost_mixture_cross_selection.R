#Equation 7a(iii)

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
                                                                       intercept = 0.15){
  
  
  previous.intensity = initial.resistance.intensity
  
  direct.insecticide.selection = response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                   nsim = nsim, 
                                                                   minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                   maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                   minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                   maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                   minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                   maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) 
  
  
  direct.fitness.costs = effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                exposure.scaling.factor = exposure.scaling.factor,
                                                nsim = nsim, 
                                                minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  
  
  indirect.selection.and.fitness = insecticide_deployed_cross_selection(currently.deployed.insecticide = currently.deployed.insecticide,
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
                                                                        resistance.cost = resistance.cost)
  
  
  survival.to.other.insecticide = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                  mean.population.resistance = intensity.to.other.mixture.part,
                                                                  michaelis.menten.slope = 1, 
                                                                  half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                  sd.population.resistance = 0, 
                                                                  nsim = nsim)
  #convert to field based survival:
  field.survival = convert_bioassay_survival_to_field(bioassay.survival = survival.to.other.insecticide,
                                                      conversion.factor = conversion.factor,
                                                      intercept = intercept)
  
  
  track.resistance.intensity = previous.intensity + ((direct.insecticide.selection +
                                                        direct.fitness.costs +
                                                        indirect.selection.and.fitness)*
                                                       field.survival)
  
  return(track.resistance.intensity)
}

