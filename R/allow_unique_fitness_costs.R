allow_unique_fitness_costs = function(resistance.cost,
                                      number.of.insecticides){
  
  if(length(resistance.cost) == 1){
    fitness.cost.vector = rep(resistance.cost, times = number.of.insecticides)
    
    return(fitness.cost.vector)
  }
  
  if(length(resistance.cost) == number.of.insecticides){
    fitness.cost.vector = resistance.cost
    
    return(fitness.cost.vector)
  }
  
  if(length(resistance.cost) != 1 & 
     length(resistance.cost) != number.of.insecticides){
    stop("length of resistance.cost must be 1 or number.of.insecticides")
  }
}