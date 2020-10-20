

sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 1)

sim.array["treatment" ,1, 1] = 100
sim.array["treatment" ,2, 1] = 200
sim.array["treatment" ,3, 1] = 300

test_that("insecticide should be returned", {
  expect_equal(check_insecticide_return (insecticide = 1,
                                         current.generation = 1,
                                         return.threshold = 200,
                                         simulation.array = sim.array), TRUE)
})

test_that("insecticide should be returned as equal", {
  expect_equal(check_insecticide_return (insecticide = 2,
                                         current.generation = 1,
                                         return.threshold = 200,
                                         simulation.array = sim.array), TRUE)
})

test_that("insecticide should remain withdrawn", {
  expect_equal(check_insecticide_return (insecticide = 3,
                                         current.generation = 1,
                                         return.threshold = 200,
                                         simulation.array = sim.array), FALSE)
})