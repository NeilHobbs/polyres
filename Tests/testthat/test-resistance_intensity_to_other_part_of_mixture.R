mixture.id = rep(1, times = 10)#first row, first column
mixture.part.1 = rep(1, times = 10)#first row, second column
mixture.part.2 = rep(2, times = 10)#first row, third column
deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)

sim.array = create_starting_array(n.insecticides = 2,
                                  maximum.generations = 20)

sim.array["treatment", 1, 9] = 200
sim.array["treatment", 2, 9] = 100


test_that("returns other resistance value", {
  expect_equal(resistance_intensity_to_other_part_of_mixture(deployed.mixture = deployed.mixture,
                                                             generation = 10,
                                                             insecticide = 1,
                                                             sim.array = sim.array), 100)
  expect_equal(resistance_intensity_to_other_part_of_mixture(deployed.mixture = deployed.mixture,
                                                             generation = 10,
                                                             insecticide = 2,
                                                             sim.array = sim.array), 200)
})
