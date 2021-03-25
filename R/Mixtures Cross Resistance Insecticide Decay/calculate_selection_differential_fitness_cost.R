calculate_selection_differential_fitness_cost = function(relative.fitness,
                                            population.sd){
  
 
  #Truncation selection for selection intensity
  selection.differential = -population.sd * ((dnorm(qnorm(1-relative.fitness)))/relative.fitness)

  
  return(selection.differential)
}


calculate_selection_differential_fitness_cost(0.93, 5)
