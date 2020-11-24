
refugia_cross_selection_fitness_costs = function( resistance.cost = 0,
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
                                                  cross.selection.matrix){
  
  insecticide.vector = seq(1, number.of.insecticides, by = 1)
  insecticide.vector = insecticide.vector[-currently.tracked.insecticide] 
  
  cross.fitness.costs.values = c()
  
  for(j in min(insecticide.vector):max(insecticide.vector)){
    cross.fitness.costs.values[j] = mean(effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                                exposure.scaling.factor = exposure.scaling.factor,
                                                                nsim = nsim, 
                                                                minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))*
                                                                cross.selection.matrix[j, currently.tracked.insecticide]
  }
  
  cross.fitness.costs.value = sum(cross.fitness.costs.values, na.rm = TRUE)
  
  return(cross.fitness.costs.value)
}




# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
#                                           min.cross.selection = -1,
#                                           max.cross.selection = 1)
# 
# 
# refugia_cross_selection_fitness_costs(resistance.cost = 0.1,
#                                                   exposure.scaling.factor = 10,
#                                                   nsim = 1, 
#                                                   minimum.insecticide.resistance.heritability = 0.05, 
#                                                   maximum.insecticide.resistance.heritability = 0.30,
#                                                   minimum.male.insecticide.exposure = 0,
#                                                   maximum.male.insecticide.exposure = 1, 
#                                                   minimum.female.insecticide.exposure = 0.4, 
#                                                   maximum.female.insecticide.exposure = 0.9,
#                                                   number.of.insecticides = 3,
#                                                   currently.tracked.insecticide = 1,
#                                                   cross.selection.matrix = temp.matrix)
