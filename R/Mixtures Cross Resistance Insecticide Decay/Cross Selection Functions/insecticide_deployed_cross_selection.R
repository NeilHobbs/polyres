
insecticide_deployed_cross_selection = function(currently.deployed.insecticide,
                                                number.of.insecticides,
                                                cross.selection.matrix,
                                                exposure.scaling.factor = 10,
                                                nsim = 1000, 
                                                minimum.insecticide.resistance.heritability = 0.05, 
                                                maximum.insecticide.resistance.heritability = 0.30,
                                                minimum.male.insecticide.exposure = 0,
                                                maximum.male.insecticide.exposure = 1, 
                                                minimum.female.insecticide.exposure = 0.4, 
                                                maximum.female.insecticide.exposure = 0.9,
                                                resistance.cost = 0.1){

    
    if(0 > minimum.insecticide.resistance.heritability |minimum.insecticide.resistance.heritability > 1){stop("minimum.insecticide.resistance.heritability must be between 0 and 1")}
    if(0 > maximum.insecticide.resistance.heritability |maximum.insecticide.resistance.heritability > 1){stop("maximum.insecticide.resistance.heritability must be between 0 and 1")}
    if(minimum.insecticide.resistance.heritability > maximum.insecticide.resistance.heritability){stop("minimum.insecticide.resistance.heritability is greater than maximum.insecticide.resistance.heritability")}
    
    if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
    if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
    if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}
    
    if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
    if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
    if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}
    
  
     #Must not include the deployed.insecticide
     insecticide.vector = seq(1, number.of.insecticides, by = 1)
     insecticide.vector = insecticide.vector[-currently.deployed.insecticide]
  
  cross.selection.values = c()
  
  for(j in min(insecticide.vector):max(insecticide.vector)){
  
  
    cross.selection.values[j] =   mean(response_to_insecticide_selection( 
                                                                      exposure.scaling.factor = exposure.scaling.factor,
                                                                      nsim = nsim, 
                                                                      minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                      maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                      minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                      maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                      minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                      maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) 
    #plus sign used as made negative effect put in effect_of_fitness_cost function        
    + effect_of_fitness_cost(
      resistance.cost = resistance.cost,
      exposure.scaling.factor = exposure.scaling.factor,
      nsim = nsim, 
      minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
      maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
      minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
      maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
      minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
      maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)) * (cross.selection.matrix[currently.deployed.insecticide, j])
}

cross.selection.value = sum(cross.selection.values)
  return(cross.selection.value)
}
