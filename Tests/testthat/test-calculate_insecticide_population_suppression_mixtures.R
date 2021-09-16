sim.array = create_starting_array(n.insecticides = 2,
                                  maximum.generations = 10)

sim.array["treatment", 1, 5] = 100
sim.array["treatment", 2, 5] = 100



test_that("population size is unchanged", {
  expect_equal(calculate_insecticide_population_suppression_mixtures(minimum.female.insecticide.exposure = 1,
                                                      maximum.female.insecticide.exposure = 1,
                                                      nsim = 1,
                                                      intercept = 1,
                                                      conversion.factor = 1,
                                                      current.insecticide.efficacy.part.1 = 1,
                                                      current.insecticide.efficacy.part.2 = 1,
                                                      currently.deployed.mixture.part.1 = 1,
                                                      currently.deployed.mixture.part.2 = 2,
                                                      sim.array = sim.array,
                                                      current.generation = 6,
                                                      half.population.bioassay.survival.resistance = 900), 1)
})

test_that("all die", {
  expect_equal(calculate_insecticide_population_suppression_mixtures(minimum.female.insecticide.exposure = 1,
                                                                     maximum.female.insecticide.exposure = 1,
                                                                     nsim = 1,
                                                                     intercept = 0,
                                                                     conversion.factor = 0,
                                                                     current.insecticide.efficacy.part.1 = 1,
                                                                     current.insecticide.efficacy.part.2 = 1,
                                                                     currently.deployed.mixture.part.1 = 1,
                                                                     currently.deployed.mixture.part.2 = 2,
                                                                     sim.array = sim.array,
                                                                     current.generation = 6,
                                                                     half.population.bioassay.survival.resistance = 900), 0)
})

test_that("no exposure all survive", {
  expect_equal(calculate_insecticide_population_suppression_mixtures(minimum.female.insecticide.exposure = 0,
                                                                     maximum.female.insecticide.exposure = 0,
                                                                     nsim = 1,
                                                                     intercept = 1,
                                                                     conversion.factor = 1,
                                                                     current.insecticide.efficacy.part.1 = 1,
                                                                     current.insecticide.efficacy.part.2 = 1,
                                                                     currently.deployed.mixture.part.1 = 1,
                                                                     currently.deployed.mixture.part.2 = 2,
                                                                     sim.array = sim.array,
                                                                     current.generation = 6,
                                                                     half.population.bioassay.survival.resistance = 900), 1)
})

test_that("no insecticide efficacy all survive", {
  expect_equal(calculate_insecticide_population_suppression_mixtures(minimum.female.insecticide.exposure = 1,
                                                                     maximum.female.insecticide.exposure = 1,
                                                                     nsim = 1,
                                                                     intercept = 1,
                                                                     conversion.factor = 1,
                                                                     current.insecticide.efficacy.part.1 = 0,
                                                                     current.insecticide.efficacy.part.2 = 0,
                                                                     currently.deployed.mixture.part.1 = 1,
                                                                     currently.deployed.mixture.part.2 = 2,
                                                                     sim.array = sim.array,
                                                                     current.generation = 6,
                                                                     half.population.bioassay.survival.resistance = 900), 1)
})


