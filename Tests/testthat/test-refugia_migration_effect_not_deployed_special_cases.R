sim.array = create_starting_array(n.insecticides = 2,
                                  maximum.generations = 10)
sim.array["treatment", 1, 4] = 0
sim.array["refugia", 1, 4] = 0
sim.array["treatment", 2, 4] = 0


test_that("no cross selection", {
  expect_equal(refugia_migration_effect_not_deployed_special_cases(exposure.scaling.factor = 10,
                                                                   nsim = 1, 
                                                                   minimum.insecticide.resistance.heritability = 1, 
                                                                   maximum.insecticide.resistance.heritability = 1,
                                                                   minimum.male.insecticide.exposure = 1,
                                                                   maximum.male.insecticide.exposure = 1, 
                                                                   minimum.female.insecticide.exposure = 1, 
                                                                   maximum.female.insecticide.exposure = 1,
                                                                   min.intervention.coverage = 1, 
                                                                   max.intervention.coverage = 1, 
                                                                   min.dispersal.rate = 0,
                                                                   max.dispersal.rate = 0,
                                                                   resistance.cost = 0,
                                                                   initial.resistance.intensity = 0,
                                                                   cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                        min.cross.selection = 0,
                                                                                                                        max.cross.selection = 0),
                                                                   currently.deployed.insecticide = 1,
                                                                   currently.tracked.insecticide = 2,
                                                                   initial.refugia.resistance= 0,
                                                                   current.insecticide.efficacy= 1,
                                                                   intercept = 0.15,
                                                                   conversion.factor = 0.48,
                                                                   sim.array = sim.array,
                                                                   current.generation = 5,
                                                                   half.population.bioassay.survival.resistance = 900,
                                                                   insecticide.suppression = FALSE), 0)
})


test_that("cross selection works - dispersal", {
  expect_equal(refugia_migration_effect_not_deployed_special_cases(exposure.scaling.factor = 10,
                                                                   nsim = 1, 
                                                                   minimum.insecticide.resistance.heritability = 1, 
                                                                   maximum.insecticide.resistance.heritability = 1,
                                                                   minimum.male.insecticide.exposure = 1,
                                                                   maximum.male.insecticide.exposure = 1, 
                                                                   minimum.female.insecticide.exposure = 1, 
                                                                   maximum.female.insecticide.exposure = 1,
                                                                   min.intervention.coverage = 1, 
                                                                   max.intervention.coverage = 1, 
                                                                   min.dispersal.rate = 1,
                                                                   max.dispersal.rate = 1,
                                                                   resistance.cost = 0,
                                                                   initial.resistance.intensity = 0,
                                                                   cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                        min.cross.selection = 1,
                                                                                                                        max.cross.selection = 1),
                                                                   currently.deployed.insecticide = 1,
                                                                   currently.tracked.insecticide = 2,
                                                                   initial.refugia.resistance= 0,
                                                                   current.insecticide.efficacy= 1,
                                                                   intercept = 1,
                                                                   conversion.factor = 1,
                                                                   sim.array = sim.array,
                                                                   current.generation = 5,
                                                                   half.population.bioassay.survival.resistance = 900,
                                                                   insecticide.suppression = FALSE), 10)
})
