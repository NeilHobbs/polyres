
insecticide_not_deployed_direct_cross_selection = function(exposure.scaling.factor = 10,
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
                                                           currently.tracked.insecticide){
  
  
 
  track.resistance.intensity = initial.resistance.intensity + 
                                                 ((response_to_insecticide_selection(
                                                                                      exposure.scaling.factor = exposure.scaling.factor,
                                                                                      nsim = nsim, 
                                                                                      minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                                      maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                                      minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                                      maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                                      minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                                      maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
                                                 effect_of_fitness_cost(
                                                   resistance.cost = resistance.cost,
                                                   exposure.scaling.factor = exposure.scaling.factor,
                                                   nsim = nsim, 
                                                   minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                   maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                   minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                   maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                   minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                   maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)) *  
                                                            cross.selection.matrix[currently.deployed.insecticide, currently.tracked.insecticide]) +
                            
                 ((response_to_insecticide_selection(
                                                        exposure.scaling.factor = exposure.scaling.factor,
                                                        nsim = nsim, 
                                                        minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                        maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                        minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                        maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                        minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                        maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
                      effect_of_fitness_cost(
                                                        resistance.cost = resistance.cost,
                                                        exposure.scaling.factor = exposure.scaling.factor,
                                                        nsim = nsim, 
                                                        minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                        maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                        minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                        maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                        minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                        maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)) *  
      cross.selection.matrix[currently.tracked.insecticide, currently.deployed.insecticide])
  
  
  
  
  return(track.resistance.intensity)
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# insecticide_not_deployed_direct_cross_selection(exposure.scaling.factor = 10,
#                                                     nsim = 1,
#                                                     minimum.insecticide.resistance.heritability = 1,
#                                                     maximum.insecticide.resistance.heritability = 1,
#                                                     minimum.male.insecticide.exposure = 1,
#                                                     maximum.male.insecticide.exposure = 1,
#                                                     minimum.female.insecticide.exposure = 1,
#                                                     maximum.female.insecticide.exposure = 1,
#                                                     resistance.cost = 0,
#                                                     initial.resistance.intensity = 0,
#                                                     cross.selection.matrix = temp.matrix,
#                                                     currently.deployed.insecticide = 1,
#                                                     currently.tracked.insecticide = 2)
