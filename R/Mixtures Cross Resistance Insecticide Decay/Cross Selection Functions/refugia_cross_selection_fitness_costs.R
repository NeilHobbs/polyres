
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
    cross.fitness.costs.values[j] = mean(effect_of_fitness_cost(resistance.cost = 0,
                                                                exposure.scaling.factor = 10,
                                                                nsim = 1000, 
                                                                minimum.insecticide.resistance.heritability = 0.05, 
                                                                maximum.insecticide.resistance.heritability = 0.30,
                                                                minimum.male.insecticide.exposure = 0,
                                                                maximum.male.insecticide.exposure = 1, 
                                                                minimum.female.insecticide.exposure = 0.4, 
                                                                maximum.female.insecticide.exposure = 0.9))*
                                                                  cross.selection.matrix[j, currently.tracked.insecticide]
  }
  
  cross.fitness.costs.value = sum(cross.fitness.costs.values)
  
  return(cross.fitness.costs.value)
}