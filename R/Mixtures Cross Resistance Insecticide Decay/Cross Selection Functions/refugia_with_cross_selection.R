
refugia_with_cross_selection = function(resistance.cost = 0,
                                        exposure.scaling.factor = 10,
                                        nsim = 1000, 
                                        minimum.insecticide.resistance.heritability = 0.05, 
                                        maximum.insecticide.resistance.heritability = 0.30,
                                        minimum.male.insecticide.exposure = 0,
                                        maximum.male.insecticide.exposure = 1, 
                                        minimum.female.insecticide.exposure = 0.4, 
                                        maximum.female.insecticide.exposure = 0.9,
                                        number.of.insecticides,
                                        currently.tracked.insecticide,
                                        cross.selection.matrix,
                                        initial.refugia.resistance){
  
  track.refugia.resistance = initial.refugia.resistance +
                                effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                       exposure.scaling.factor = exposure.scaling.factor,
                                                       nsim = nsim, 
                                                       minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                       maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                       minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                       maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                       minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                       maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
                                refugia_cross_selection_fitness_costs(
                                                        resistance.cost = resistance.cost,
                                                        exposure.scaling.factor = exposure.scaling.factor,
                                                        nsim = nsim, 
                                                        minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                        maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                        minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                        maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                        minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                        maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                        number.of.insecticides = number.of.insecticides,
                                                        currently.tracked.insecticide = currently.tracked.insecticide,
                                                        cross.selection.matrix = cross.selection.matrix
                                )
  
  
  #resistance intensity cannot be below 0
  track.refugia.resistance = ifelse(track.refugia.resistance < 0, yes = 0, no = track.refugia.resistance)
  
  return(track.refugia.resistance)
}

# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
#                                           min.cross.selection = -1,
#                                           max.cross.selection = 1)
# 
# 
# refugia_with_cross_selection(resistance.cost = 0.1,
#                                         exposure.scaling.factor = 10,
#                                         nsim = 1000,
#                                         minimum.insecticide.resistance.heritability = 0.05,
#                                         maximum.insecticide.resistance.heritability = 0.30,
#                                         minimum.male.insecticide.exposure = 0,
#                                         maximum.male.insecticide.exposure = 1,
#                                         minimum.female.insecticide.exposure = 0.4,
#                                         maximum.female.insecticide.exposure = 0.9,
#                                         number.of.insecticides = 3,
#                                         currently.tracked.insecticide =1,
#                                         cross.selection.matrix = temp.matrix,
#                                         initial.refugia.resistance = 10)
