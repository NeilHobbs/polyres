#Equation 8a(iii)

#' @title 
#' 
#' @description 
#' 
#' @param

#This is equation 8a(i)
insecticide_not_deployed_mixtures_cross_selection = function(exposure.scaling.factor = 10,
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
                                                             deployed.mixture.1,
                                                             deployed.mixture.2,
                                                             currently.tracked.insecticide,
                                                             conversion.factor = 0.48,
                                                             intercept = 0.15){
  
  #The genetic correlation between the trait which gives resistance to the tracked insecticide the trait
  #which gives resistance to the the deployed insecticide.
  genetic.correlation.mix.1 = cross.selection.matrix[deployed.mixture.1, currently.tracked.insecticide]
  
  genetic.correlation.mix.2 = cross.selection.matrix[deployed.mixture.2, currently.tracked.insecticide]
  
  
  #Same for all insecticides at the moment
  fitness.costs = effect_of_fitness_cost(resistance.cost = resistance.cost,
                                         exposure.scaling.factor = exposure.scaling.factor,
                                         nsim = nsim, 
                                         minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                         maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                         minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                         maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                         minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                         maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  #Same for all insecticides at the moment
  response.to.selection = response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                            nsim = nsim, 
                                                            minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                            maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                            minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                            maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                            minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                            maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  intensity.mix.1 = resistance_intensity_to_other_part_of_mixture(deployed.mixture = deployed.mixture,
                                                                  generation = generation,
                                                                  insecticide = deployed.mixture.1,
                                                                  sim.array = sim.array)
  
  field.survival.mix.1 = convert_bioassay_survival_to_field(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                mean.population.resistance = intensity.mix.1,
                                                                                                                michaelis.menten.slope = 1, 
                                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                sd.population.resistance = 0, 
                                                                                                                nsim = nsim),
                                                            conversion.factor = conversion.factor,
                                                            intercept = intercept)
  
  intensity.mix.2 = resistance_intensity_to_other_part_of_mixture(deployed.mixture = deployed.mixture,
                                                                  generation = generation,
                                                                  insecticide = deployed.mixture.2,
                                                                  sim.array = sim.array)
  
  
  field.survival.mix.2 = convert_bioassay_survival_to_field(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                                mean.population.resistance = intensity.mix.2,
                                                                                                                michaelis.menten.slope = 1, 
                                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                sd.population.resistance = 0, 
                                                                                                                nsim = nsim),
                                                            conversion.factor = conversion.factor,
                                                            intercept = intercept)
  
  #The effect of indirect selection.
  indirect.selection.mix.1 = (response.to.selection + fitness.costs) * genetic.correlation.mix.1 * field.survival.mix.2
  indirect.selection.mix.2 = (response.to.selection + fitness.costs) * genetic.correlation.mix.2 * field.survival.mix.1
  
  
  track.resistance.intensity = initial.resistance.intensity + fitness.costs + indirect.selection.mix.1 + indirect.selection.mix.2
  
  #resistance intensity values cannot be below zero.
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, yes = 0, no = track.resistance.intensity)  
  
  return(track.resistance.intensity)
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# insecticide_not_deployed_indirect_cross_selection(exposure.scaling.factor = 10,
#                                                     nsim = 1,
#                                                     minimum.insecticide.resistance.heritability = 1,
#                                                     maximum.insecticide.resistance.heritability = 1,
#                                                     minimum.male.insecticide.exposure = 1,
#                                                     maximum.male.insecticide.exposure = 1,
#                                                     minimum.female.insecticide.exposure = 1,
#                                                     maximum.female.insecticide.exposure = 1,
#                                                     resistance.cost = 0.5,
#                                                     initial.resistance.intensity = 50,
#                                                     cross.selection.matrix = temp.matrix,
#                                                     currently.deployed.insecticide = 1,
#                                                     currently.tracked.insecticide = 2)
# 
# 
# 
# insecticide_not_deployed_selection_cost(
#    initial.resistance.intensity = 50,
#    resistance.cost = 0.5,
#    exposure.scaling.factor = 10,
#    nsim = 1, 
#    minimum.insecticide.resistance.heritability = 1, 
#    maximum.insecticide.resistance.heritability = 1,
#    minimum.male.insecticide.exposure = 1,
#    maximum.male.insecticide.exposure = 1, 
#    minimum.female.insecticide.exposure = 1, 
#    maximum.female.insecticide.exposure = 1)
