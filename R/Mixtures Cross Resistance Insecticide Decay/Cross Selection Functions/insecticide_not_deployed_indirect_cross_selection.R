

insecticide_not_deployed_indirect_cross_selection= function(exposure.scaling.factor = 10,
                                                            nsim = 1000, 
                                                            minimum.insecticide.resistance.heritability = 0.05, 
                                                            maximum.insecticide.resistance.heritability = 0.30,
                                                            minimum.male.insecticide.exposure = 0,
                                                            maximum.male.insecticide.exposure = 1, 
                                                            minimum.female.insecticide.exposure = 0.4, 
                                                            maximum.female.insecticide.exposure = 0.9,
                                                            resistance.cost = 0.1,
                                                            cross.selection.matrix,
                                                            currently.deployed.insecticide,
                                                            currently.tracked.insecticide){
  
  #Must not include the deployed.insecticide
     insecticide.vector = seq(1, number.of.insecticides, by = 1)
     insecticide.vector = insecticide.vector[!insecticide.vector %in% c(currently.deployed.insecticide,currently.tracked.insecticide)]
  
       indirect.cross.selection.values = c()

       for(v in min(insecticide.vector):max(insecticide.vector)){

         indirect.cross.selection.values[v] =   mean((response_to_insecticide_selection(
                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                             nsim = nsim, 
                                                                             minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                             maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                             minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                             maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                             minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                             maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) +
                                                      effect_of_fitness_cost(
                                                                             resistance.cost = resistance.cost,
                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                             nsim = nsim, 
                                                                             minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                             maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                             minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                             maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                             minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                             maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)) *  
            (cross.selection.matrix[currently.deployed.insecticide, v]) * (cross.selection.matrix[v, currently.tracked.insecticide]))


       }
#The sum of each individual bit.total.indirect.cross.selection = sum(indirect.cross.selection.values, na.rm = TRUE)

return(total.indirect.cross.selection)

}

  