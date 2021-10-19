non.special.cases = get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 2,
                                                           exposure.scaling.factor = 10,
                                                           nsim = 1,
                                                           minimum.insecticide.resistance.heritability = 0.3,
                                                           maximum.insecticide.resistance.heritability = 0.3,
                                                           minimum.male.insecticide.exposure = 0.3,
                                                           maximum.male.insecticide.exposure = 0.3,
                                                           minimum.female.insecticide.exposure = 0.3,
                                                           maximum.female.insecticide.exposure = 0.3,
                                                           resistance.cost = 0,
                                                           starting.treatment.site.intensity = 0,
                                                           starting.refugia.intensity = 0,
                                                           min.intervention.coverage = 0.7,
                                                           max.intervention.coverage = 0.7,
                                                           min.dispersal.rate = 0.3,
                                                           max.dispersal.rate = 0.3,
                                                           maximum.generations = 100,
                                                           irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                           half.population.bioassay.survival.resistance = 900,
                                                           withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                           return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                           deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                           maximum.resistance.value = 25000), number.of.insecticides = 2, maximum.generations = 100)

special.cases.switched.off = get_simulation_dataframe(run_simulation_intervention_special_cases(number.of.insecticides = 2,
                                                                       exposure.scaling.factor = 10,
                                                                       nsim = 1,
                                                                       minimum.insecticide.resistance.heritability = 0.3,
                                                                       maximum.insecticide.resistance.heritability = 0.3,
                                                                       minimum.male.insecticide.exposure = 0.3,
                                                                       maximum.male.insecticide.exposure = 0.3,
                                                                       minimum.female.insecticide.exposure = 0.3,
                                                                       maximum.female.insecticide.exposure = 0.3,
                                                                       resistance.cost = 0,
                                                                       starting.treatment.site.intensity = 0,
                                                                       starting.refugia.intensity = 0,
                                                                       min.intervention.coverage = 0.7,
                                                                       max.intervention.coverage = 0.7,
                                                                       min.dispersal.rate = 0.3,
                                                                       max.dispersal.rate = 0.3,
                                                                       maximum.generations = 100,
                                                                       irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                                       half.population.bioassay.survival.resistance = 900,
                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                       deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                       maximum.resistance.value = 25000,
                                                                       min.cross.selection = 0,
                                                                       max.cross.selection = 0,
                                                                       applied.insecticide.dose = 1,
                                                                       recommended.insecticide.dose = 1,
                                                                       threshold.generation = 10,
                                                                       base.efficacy.decay.rate = 0,
                                                                       rapid.decay.rate = 0,
                                                                       intercept = 0,
                                                                       conversion.factor = 1,
                                                                       insecticide.suppression = FALSE), number.of.insecticides = 2, maximum.generations = 100)






test_that("special cases equal no special cases when all switched off", {
  expect_equal(non.special.cases, special.cases.switched.off)
})
