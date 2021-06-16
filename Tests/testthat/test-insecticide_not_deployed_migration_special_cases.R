sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 10)

sim.array["treatment", 1, 4] = 0
sim.array["refugia", 1, 4] = 0
sim.array["treatment", 2, 4] = 0


test_that("no change", {
  expect_equal(insecticide_not_deployed_migration_special_cases(resistance.cost = 0,
                                                                exposure.scaling.factor = 10,
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
                                                                currently.tracked.insecticide = 1,
                                                                cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                     min.cross.selection = 0,
                                                                                                                     max.cross.selection = 0),
                                                                initial.resistance.intensity = sim.array["treatment", 1, 4],
                                                                initial.refugia.resistance = sim.array["refugia", 1, 4],
                                                                currently.deployed.insecticide = 2,
                                                                current.insecticide.efficacy = 1,
                                                                intercept = 1,
                                                                conversion.factor = 1,
                                                                sim.array = sim.array,
                                                                current.generation = 5,
                                                                half.population.bioassay.survival.resistance = 900,
                                                                insecticide.suppression = FALSE), 0)
})

test_that("cross selection works", {
  expect_equal(insecticide_not_deployed_migration_special_cases(resistance.cost = 0,
                                                                exposure.scaling.factor = 10,
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
                                                                currently.tracked.insecticide = 1,
                                                                cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                     min.cross.selection = 1,
                                                                                                                     max.cross.selection = 1),
                                                                initial.resistance.intensity = sim.array["treatment", 1, 4],
                                                                initial.refugia.resistance = sim.array["refugia", 1, 4],
                                                                currently.deployed.insecticide = 2,
                                                                current.insecticide.efficacy = 1,
                                                                intercept = 1,
                                                                conversion.factor = 1,
                                                                sim.array = sim.array,
                                                                current.generation = 5,
                                                                half.population.bioassay.survival.resistance = 900,
                                                                insecticide.suppression = FALSE), 10)
})