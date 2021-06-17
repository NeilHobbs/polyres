A = run_simulation_intervention_special_cases(number.of.insecticides = 5,
                                          exposure.scaling.factor = 10,
                                          nsim = 1,
                                          minimum.insecticide.resistance.heritability = 0.2,
                                          maximum.insecticide.resistance.heritability = 0.2,
                                          minimum.male.insecticide.exposure = 1,
                                          maximum.male.insecticide.exposure = 1,
                                          minimum.female.insecticide.exposure = 0.5,
                                          maximum.female.insecticide.exposure = 0.5,
                                          resistance.cost = 0.1,
                                          starting.treatment.site.intensity = 0,
                                          starting.refugia.intensity = 0,
                                          min.intervention.coverage = 1,
                                          max.intervention.coverage = 1,
                                          min.dispersal.rate = 0,
                                          max.dispersal.rate = 0,
                                          maximum.generations = 200,
                                          irm.strategy = "sequence",
                                          half.population.bioassay.survival.resistance = 900,
                                          withdrawal.threshold.value = 0.1, 
                                          return.threshold.value = 0.05, 
                                          deployment.frequency = 10, 
                                          maximum.resistance.value = 25000,
                                          min.cross.selection = 0.3,
                                          max.cross.selection = 0.3,
                                          applied.insecticide.dose = 1.2,
                                          recommended.insecticide.dose = 1,
                                          threshold.generation = 1,
                                          base.efficacy.decay.rate = 0.01,
                                          rapid.decay.rate = 0.2,
                                          intercept = 0.15,
                                          conversion.factor = 0.48,
                                          insecticide.suppression = FALSE)



A[[3]]
A[[2]]

B = get_simulation_dataframe(simulation.array = A,
                             maximum.generations = 200,
                             number.of.insecticides = 5)


plot_simulation(simulation.dataframe = B,
                half.population.bioassay.survival.resistance = 900,
                withdrawal.threshold = 0.1,
                return.threshold = 0.05)

