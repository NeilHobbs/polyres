calculate_fitness_cost = function(base.fitness.cost,
                                  current.resistance.intensity,
                                  offset.rate){
  
  
  current.fitness.cost = base.fitness.cost * exp(-(current.resistance.intensity^2) * offset.rate)

  current.fitness.cost = ifelse(current.fitness.cost < 0, yes = 0, no = current.fitness.cost)
  
  
  return(current.fitness.cost)
  
}


calculate_fitness_cost(1,
                       80,
                       0.01)

z.values = seq(1, 200, by= 0.01)
fit.vals = c()

for(i in 1:length(z.values)){
  
  fit.vals[i] = calculate_fitness_cost(base.fitness.cost = 0.2,
                                       current.resistance.intensity = z.values[i],
                                       offset.rate = 0.002)
  
}


plot(z.values, fit.vals)

