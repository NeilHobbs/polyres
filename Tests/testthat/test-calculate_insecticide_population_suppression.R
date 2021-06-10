sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 10)

sim.array["treatment", 2, 5] = 9
sim.array["treatment", 3, 5] = 900

test_that("cannot go above 1", {
  expect_equal(calculate_insecticide_population_suppression(minimum.female.insecticide.exposure = 1,
                                                            maximum.female.insecticide.exposure = 1,
                                                            nsim = 1,
                                                            intercept = 1000,
                                                            conversion.factor = 1000,
                                                            current.insecticide.efficacy = 1,
                                                            currently.deployed.insecticide = 2,
                                                            sim.array = sim.array,
                                                            current.generation = 6,
                                                            half.population.bioassay.survival.resistance = 900), 1)
})

test_that("cannot go below 1", {
  expect_equal(calculate_insecticide_population_suppression(minimum.female.insecticide.exposure = 1,
                                                            maximum.female.insecticide.exposure = 1,
                                                            nsim = 1,
                                                            intercept = -1000,
                                                            conversion.factor = 1,
                                                            current.insecticide.efficacy = 1,
                                                            currently.deployed.insecticide = 2,
                                                            sim.array = sim.array,
                                                            current.generation = 6,
                                                            half.population.bioassay.survival.resistance = 900), 0)
})

test_that("gives half", {
  expect_equal(calculate_insecticide_population_suppression(minimum.female.insecticide.exposure = 1,
                                                            maximum.female.insecticide.exposure = 1,
                                                            nsim = 1,
                                                            intercept = 0,
                                                            conversion.factor = 1,
                                                            current.insecticide.efficacy = 1,
                                                            currently.deployed.insecticide = 3,
                                                            sim.array = sim.array,
                                                            current.generation =6,
                                                            half.population.bioassay.survival.resistance = 900), 0.5)
})
