
insecticide_not_deployed_with_cross_selection = function(exposure.scaling.factor = 10,
                                                         nsim = 1000, 
                                                         minimum.insecticide.resistance.heritability = 0.05, 
                                                         maximum.insecticide.resistance.heritability = 0.30,
                                                         minimum.male.insecticide.exposure = 0,
                                                         maximum.male.insecticide.exposure = 1, 
                                                         minimum.female.insecticide.exposure = 0.4, 
                                                         maximum.female.insecticide.exposure = 0.9,
                                                         resistance.cost = 0.1,
                                                         initial.resistance.intensity = 0,
                                                         cross.selection.matrix,
                                                         currently.deployed.insecticide,
                                                         currently.tracked.insecticide){
  
  track.resistance.intensity = insecticide_not_deployed_direct_cross_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                              nsim = nsim, 
                                                                              minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                              maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                              resistance.cost = resistance.cost,
                                                                              initial.resistance.intensity = initial.resistance.intensity,
                                                                              cross.selection.matrix = cross.selection.matrix,
                                                                              currently.deployed.insecticide = currently.deployed.insecticide,
                                                                              currently.tracked.insecticide = currently.tracked.insecticide) +
                              insecticide_not_deployed_indirect_cross_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                                nsim = nsim, 
                                                                                minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                                maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                                minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                                maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                                minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                                maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                                resistance.cost = resistance.cost,
                                                                                cross.selection.matrix = cross.selection.matrix,
                                                                                currently.deployed.insecticide = currently.deployed.insecticide,
                                                                                currently.tracked.insecticide = currently.tracked.insecticide)
  
  
    return(track.resistance.intensity)
}