#Equation 7a(i)


insecticide_deployed_indirect_cross_selection  = function(currently.deployed.insecticide,
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
                                                               resistance.cost = 0.1,
                                                               initial.resistance.intensity){
  
  
  #This is the response of insecticide i on trait I
  direct.selection.response = response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                nsim = nsim, 
                                                                minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) 
  direct.selection.fitness.cost =  effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                          exposure.scaling.factor = exposure.scaling.factor,
                                                          nsim = nsim, 
                                                          minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                          maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                          minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                          maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                          minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                          maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  #This is the total of the effect of insecticide i on Traits J
  indirect.selection = insecticide_deployed_cross_selection(currently.deployed.insecticide = currently.deployed.insecticide,
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
  
  
  track.resistance.intensity = initial.resistance.intensity + (direct.selection.response + direct.selection.fitness.cost) + indirect.selection
  
  #Resistance intensity is not allowed to drop below zero.
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, yes = 0, no = track.resistance.intensity)
  
  return(track.resistance.intensity)
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# insecticide_deployed_indirect_cross_selection(currently.deployed.insecticide = 2,
#                                                                number.of.insecticides = 2,
#                                                                cross.selection.matrix = temp.matrix,
#                                                                exposure.scaling.factor = 10,
#                                                                nsim = 1,
#                                                                minimum.insecticide.resistance.heritability = 1,
#                                                                maximum.insecticide.resistance.heritability = 1,
#                                                                minimum.male.insecticide.exposure = 1,
#                                                                maximum.male.insecticide.exposure = 1,
#                                                                minimum.female.insecticide.exposure = 1,
#                                                                maximum.female.insecticide.exposure = 1,
#                                                                resistance.cost = 0,
#                                                                initial.resistance.intensity = 0)








