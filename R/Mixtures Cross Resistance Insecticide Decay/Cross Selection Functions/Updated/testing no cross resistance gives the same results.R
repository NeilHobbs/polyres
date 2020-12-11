A = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 3,
                                                         exposure.scaling.factor = 10,
                                                         nsim = 1,
                                                         minimum.insecticide.resistance.heritability = 0.3,
                                                         maximum.insecticide.resistance.heritability = 0.3,
                                                         minimum.male.insecticide.exposure = 0.5,
                                                         maximum.male.insecticide.exposure = 0.5,
                                                         minimum.female.insecticide.exposure = 0.8,
                                                         maximum.female.insecticide.exposure = 0.8,
                                                         resistance.cost = 0.2,
                                                         starting.treatment.site.intensity = 0,
                                                         starting.refugia.intensity = 0,
                                                         min.intervention.coverage = 0.7,
                                                         max.intervention.coverage = 0.7,
                                                         min.dispersal.rate = 0.3,
                                                         max.dispersal.rate = 0.3,
                                                         maximum.generations = 500,
                                                         irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                                         half.population.bioassay.survival.resistance = 900,
                                                         withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                         return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                         deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                         maximum.resistance.value = 25000), 
                             number.of.insecticides = 3,
                             maximum.generations = 500)
  

plot_simulation(A,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05)



B =get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 3,
                                                                        exposure.scaling.factor = 10,
                                                                        nsim = 1,
                                                                        minimum.insecticide.resistance.heritability = 0.3,
                                                                        maximum.insecticide.resistance.heritability = 0.3,
                                                                        minimum.male.insecticide.exposure = 0.5,
                                                                        maximum.male.insecticide.exposure = 0.5,
                                                                        minimum.female.insecticide.exposure = 0.8,
                                                                        maximum.female.insecticide.exposure = 0.8,
                                                                        resistance.cost = 0.2,
                                                                        starting.treatment.site.intensity = 0,
                                                                        starting.refugia.intensity = 0,
                                                                        min.intervention.coverage = 0.7,
                                                                        max.intervention.coverage = 0.7,
                                                                        min.dispersal.rate = 0.3,
                                                                        max.dispersal.rate = 0.3,
                                                                        maximum.generations = 500,
                                                                        irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                                                        half.population.bioassay.survival.resistance = 900,
                                                                        withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                        return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                        deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                        maximum.resistance.value = 25000,
                                                                        min.cross.selection = 0,
                                                                        max.cross.selection = 0),
                            number.of.insecticides = 3,
                            maximum.generations = 500)  



plot_simulation(B,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05)


