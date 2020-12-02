A = get_simulation_dataframe(run_simulation_intervention_test(number.of.insecticides = 2,
                                                              exposure.scaling.factor = 10,
                                                              nsim = 1,
                                                              minimum.insecticide.resistance.heritability = 0.3,
                                                              maximum.insecticide.resistance.heritability = 0.3,
                                                              minimum.male.insecticide.exposure = 1,
                                                              maximum.male.insecticide.exposure = 1,
                                                              minimum.female.insecticide.exposure = 1,
                                                              maximum.female.insecticide.exposure = 1,
                                                              resistance.cost = 0.1,
                                                              starting.treatment.site.intensity = 0,
                                                              starting.refugia.intensity = 0,
                                                              min.intervention.coverage = 1,
                                                              max.intervention.coverage = 1,
                                                              min.dispersal.rate = 0.5,
                                                              max.dispersal.rate = 0.5,
                                                              maximum.generations = 50,
                                                              irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                              half.population.bioassay.survival.resistance = 900,
                                                              withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                              return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                              deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                              maximum.resistance.value = 25000), 
                             number.of.insecticides = 2,
                             maximum.generations = 500)
  

plot_simulation(A,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05)



B =get_simulation_dataframe(run_simulation_intervention_cross_selection_test(number.of.insecticides = 5,
                                                                             exposure.scaling.factor = 10,
                                                                             nsim = 1,
                                                                             minimum.insecticide.resistance.heritability = 0.3,
                                                                             maximum.insecticide.resistance.heritability = 0.3,
                                                                             minimum.male.insecticide.exposure = 1,
                                                                             maximum.male.insecticide.exposure = 1,
                                                                             minimum.female.insecticide.exposure = 1,
                                                                             maximum.female.insecticide.exposure = 1,
                                                                             resistance.cost = 0.1,
                                                                             starting.treatment.site.intensity = 0,
                                                                             starting.refugia.intensity = 0,
                                                                             min.intervention.coverage = 1,
                                                                             max.intervention.coverage = 1,
                                                                             min.dispersal.rate = 0.1,
                                                                             max.dispersal.rate = 0.1,
                                                                             maximum.generations = 500,
                                                                             irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                                             half.population.bioassay.survival.resistance = 900,
                                                                             withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                             return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                             deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                             maximum.resistance.value = 25000,
                                                                             min.cross.selection = 0,
                                                                             max.cross.selection = 0),
                            number.of.insecticides = 5,
                            maximum.generations = 500)  
  

plot_simulation(B,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05)


#Issue is with dispersal.rate
