selection_differential_fitness_cost = function(sd,
                         Z_S,
                         z,
                         fitness.cost){
  
 relative.fitness.cost = (Z_S - z) * fitness.cost
 i = dnorm(pnorm((1-(relative.fitness.cost)) / 1-(relative.fitness.cost)))
 S = -(i*sd)  
 
 return(S)  
}

selection_differential_fitness_cost(sd=10,
                                    Z_S = 11,
                                    z=10,
                                    fitness = 0.001)


