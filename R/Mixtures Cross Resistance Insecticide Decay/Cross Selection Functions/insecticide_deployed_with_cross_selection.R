
insecticide_deployed_with_cross_selection = function(resistance.cost = 0,
                                                     exposure.scaling.factor = 10,
                                                     nsim = 1000, 
                                                     minimum.insecticide.resistance.heritability = 0.05, 
                                                     maximum.insecticide.resistance.heritability = 0.30,
                                                     minimum.male.insecticide.exposure = 0,
                                                     maximum.male.insecticide.exposure = 1, 
                                                     minimum.female.insecticide.exposure = 0.4, 
                                                     maximum.female.insecticide.exposure = 0.9,
                                                     initial.resistance.intensity,
                                                     currently.deployed.insecticide,
                                                     number.of.insecticides,
                                                     cross.selection.matrix){
  
  
  track.resistance.intensity = 
                 initial.resistance.intensity + 
                                response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
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
                                                       maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
    
                                insecticide_deployed_cross_selection(currently.deployed.insecticide = currently.deployed.insecticide,
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
  
  
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, yes = 0, no = track.resistance.intensity)
  
  return(track.resistance.intensity)
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# 
# insecticide_deployed_with_cross_selection(resistance.cost = 0,
#                                                      exposure.scaling.factor = 10,
#                                                      nsim = 1,
#                                                      minimum.insecticide.resistance.heritability = 1,
#                                                      maximum.insecticide.resistance.heritability = 1,
#                                                      minimum.male.insecticide.exposure = 1,
#                                                      maximum.male.insecticide.exposure = 1,
#                                                      minimum.female.insecticide.exposure = 1,
#                                                      maximum.female.insecticide.exposure = 1,
#                                                      initial.resistance.intensity = 0,
#                                                      currently.deployed.insecticide = 1,
#                                                      number.of.insecticides = 3,
#                                                      cross.selection.matrix = temp.matrix)
# 
# 
# response_to_insecticide_selection(exposure.scaling.factor = 10,
#                                   nsim = 1,
#                                   minimum.insecticide.resistance.heritability = 1,
#                                   maximum.insecticide.resistance.heritability = 1,
#                                   minimum.male.insecticide.exposure = 1,
#                                   maximum.male.insecticide.exposure = 1,
#                                   minimum.female.insecticide.exposure = 1,
#                                   maximum.female.insecticide.exposure = 1)+
# effect_of_fitness_cost(resistance.cost = 0,
#                        exposure.scaling.factor = 10,
#                        nsim = 1,
#                        minimum.insecticide.resistance.heritability = 1,
#                        maximum.insecticide.resistance.heritability = 1,
#                        minimum.male.insecticide.exposure = 1,
#                        maximum.male.insecticide.exposure = 1,
#                        minimum.female.insecticide.exposure = 1,
#                        maximum.female.insecticide.exposure = 1)
