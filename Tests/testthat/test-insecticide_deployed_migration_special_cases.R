sim.array = create_starting_array(n.insecticides = 2,
                                  maximum.generations = 10)

sim.array["treatment", 1, 4] = 0
sim.array["treatment", 2, 4] = 0

test_that("gives 10", {
  expect_equal(insecticide_deployed_migration_special_cases(resistance.cost = 0,
                                                            exposure.scaling.factor = 10,
                                                            nsim = 1, 
                                                            minimum.insecticide.resistance.heritability = 1, 
                                                            maximum.insecticide.resistance.heritability = 1,
                                                            minimum.male.insecticide.exposure = 1,
                                                            maximum.male.insecticide.exposure = 1, 
                                                            minimum.female.insecticide.exposure = 1, 
                                                            maximum.female.insecticide.exposure = 1,
                                                            min.dispersal.rate = 0,
                                                            max.dispersal.rate = 0,
                                                            min.intervention.coverage = 1, 
                                                            max.intervention.coverage = 1,
                                                            number.of.insecticides = 2,
                                                            cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                 min.cross.selection = 0,
                                                                                                                 max.cross.selection = 0),
                                                            initial.resistance.intensity = 0,
                                                            initial.refugia.resistance = 0,
                                                            currently.deployed.insecticide = 1,
                                                            current.insecticide.efficacy = 1,
                                                            sim.array = sim.array,
                                                            current.generation = 5,
                                                            half.population.bioassay.survival.resistance = 900,
                                                            intercept = 1,
                                                            conversion.factor = 1,
                                                            insecticide.suppression = FALSE), 10)
})



test_that("not heritable", {
  expect_equal(insecticide_deployed_migration_special_cases(resistance.cost = 0,
                                                            exposure.scaling.factor = 10,
                                                            nsim = 1, 
                                                            minimum.insecticide.resistance.heritability = 0, 
                                                            maximum.insecticide.resistance.heritability = 0,
                                                            minimum.male.insecticide.exposure = 1,
                                                            maximum.male.insecticide.exposure = 1, 
                                                            minimum.female.insecticide.exposure = 1, 
                                                            maximum.female.insecticide.exposure = 1,
                                                            min.dispersal.rate = 0,
                                                            max.dispersal.rate = 0,
                                                            min.intervention.coverage = 1, 
                                                            max.intervention.coverage = 1,
                                                            number.of.insecticides = 2,
                                                            cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                 min.cross.selection = 0,
                                                                                                                 max.cross.selection = 0),
                                                            initial.resistance.intensity = 0,
                                                            initial.refugia.resistance = 0,
                                                            currently.deployed.insecticide = 1,
                                                            current.insecticide.efficacy = 1,
                                                            sim.array = sim.array,
                                                            current.generation = 5,
                                                            half.population.bioassay.survival.resistance = 900,
                                                            intercept = 0.5,
                                                            conversion.factor = 0,
                                                            insecticide.suppression = FALSE), 0)
})


test_that("full cross resistance doubles rate", {
  expect_equal(insecticide_deployed_migration_special_cases(resistance.cost = 0,
                                                            exposure.scaling.factor = 10,
                                                            nsim = 1, 
                                                            minimum.insecticide.resistance.heritability = 1, 
                                                            maximum.insecticide.resistance.heritability = 1,
                                                            minimum.male.insecticide.exposure = 1,
                                                            maximum.male.insecticide.exposure = 1, 
                                                            minimum.female.insecticide.exposure = 1, 
                                                            maximum.female.insecticide.exposure = 1,
                                                            min.dispersal.rate = 0,
                                                            max.dispersal.rate = 0,
                                                            min.intervention.coverage = 1, 
                                                            max.intervention.coverage = 1,
                                                            number.of.insecticides = 2,
                                                            cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                 min.cross.selection = 1,
                                                                                                                 max.cross.selection = 1),
                                                            initial.resistance.intensity = 0,
                                                            initial.refugia.resistance = 0,
                                                            currently.deployed.insecticide = 1,
                                                            current.insecticide.efficacy = 1,
                                                            sim.array = sim.array,
                                                            current.generation = 5,
                                                            half.population.bioassay.survival.resistance = 900,
                                                            intercept = 1,
                                                            conversion.factor = 0,
                                                            insecticide.suppression = FALSE), 20)
})