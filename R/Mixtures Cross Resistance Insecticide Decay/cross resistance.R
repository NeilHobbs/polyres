
#DO CROSS RESISTANCE::Indirect Response under Direct Selection.
    #This adds the Indirect Response under direct selection. 

cross_resistance = function(number.of.insecticides = 4,
                            insecticide_i = 1,
                            generation = 10,
                            exposure.scaling.factor = 10,
                            nsim = 1000,
                            minimum.insecticide.resistance.hertitability = 0.05,
                            maximum.insecticide.resistance.hertitability = 0.30,
                            minimum.male.insecticide.exposure = 0,
                            maximum.male.insecticide.exposure = 1,
                            minimum.female.insecticide.exposure = 0.1,
                            maximum.female.insecticide.exposure = 0.9,
                            resistance.cost = 0.2,
                            degree.of.cross.resistance = 0,
                            sim.array){
  
    #create a vector that does not have insecticide_i present. 
    insecticide.vector = seq(1, number.of.insecticides, by = 1)#creates a vector of all the insecticides
    insecticide.vector = insecticide.vector[!insecticide.vector %in% insecticide_i]#removes insecticide_i from the vector

    cross.resist.contrib = c()

    #indirect selection
  for(insecticide_j in 1:length(insecticide.vector)){
                                        #calculate the mean to prevent error of incorrect vector length
  cross.resist.contrib[insecticide_j] = mean(sim.array['treatment', insecticide.vector[insecticide_j], generation - 1] +
    response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                      nsim = nsim,
                                      minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability,
                                      maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                      minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                      maximum.male.insecticide.exposure = maximum.male.insecticide.exposure,
                                      minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
                                      maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
    effect_of_fitness_cost(resistance.cost = resistance.cost,
                           exposure.scaling.factor = exposure.scaling.factor,
                           nsim = nsim,
                           minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability,
                           maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                           minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                           maximum.male.insecticide.exposure = maximum.male.insecticide.exposure,
                           minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
                           maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))*degree.of.cross.resistance
  }
    total_cross_resistance = sum(cross.resist.contrib)
    total_cross_resistance = ifelse(is.na(total_cross_resistance), yes = 0, no=total_cross_resistance)
    }

sim.array = create_starting_array(n.insecticides = 8, maximum.generations = 10)

sim.array["treatment", 1, 2] = 10
sim.array["treatment", 2, 2] = 50
sim.array["treatment", 3, 2] = 0
sim.array["treatment", 4, 2] = 0
sim.array["treatment", 5, 2] = 0
sim.array["treatment", 6, 2] = 0
sim.array["treatment", 7, 2] = 0
sim.array["treatment", 8, 2] = 0

#Current major issue is that the rate of increase would be directly proportional to the number of insecticides tracked.
  #And would give the impression that having ~100 insecticides would give faster resistance than ~1. (even if 99 of them are never used).

cross_resistance(number.of.insecticides = 8,
                            insecticide_i = 1,
                            generation = 3,
                            exposure.scaling.factor = 10,
                            nsim = 1000,
                            minimum.insecticide.resistance.hertitability = 0.30,
                            maximum.insecticide.resistance.hertitability = 0.30,
                            minimum.male.insecticide.exposure = 1,
                            maximum.male.insecticide.exposure = 1,
                            minimum.female.insecticide.exposure = 0.9,
                            maximum.female.insecticide.exposure = 0.9,
                            resistance.cost = 0.2,
                            degree.of.cross.resistance = 0.1,
                            sim.array = sim.array)



sim.array['treatment', 1, 3] = sim.array["treatment", 1, 2] + response_to_insecticide_selection(exposure.scaling.factor = 10,
                                                                            nsim = 1, 
                                                                            minimum.insecticide.resistance.hertitability = 0.30, 
                                                                            maximum.insecticide.resistance.hertitability = 0.30,
                                                                            minimum.male.insecticide.exposure = 1,
                                                                            maximum.male.insecticide.exposure = 1, 
                                                                            minimum.female.insecticide.exposure = 0.9, 
                                                                            maximum.female.insecticide.exposure = 0.9) +
  
  cross_resistance(number.of.insecticides = 8,
                   insecticide_i = 1,
                   generation = 3,
                   exposure.scaling.factor = 10,
                   nsim = 1000,
                   minimum.insecticide.resistance.hertitability = 0.30,
                   maximum.insecticide.resistance.hertitability = 0.30,
                   minimum.male.insecticide.exposure = 1,
                   maximum.male.insecticide.exposure = 1,
                   minimum.female.insecticide.exposure = 0.9,
                   maximum.female.insecticide.exposure = 0.9,
                   resistance.cost = 0.2,
                   degree.of.cross.resistance = 0.1,
                   sim.array = sim.array)



print(sim.array['treatment', 1, 3])


####OR is there another way:::

cross_resistance_ver2 = function(){
  
}


