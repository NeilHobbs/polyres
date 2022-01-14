test_that("lengths match", {
  expect_equal(allow_unique_fitness_costs(resistance.cost = c(0.1, 0.2, 0.3, 0.4),
                                          number.of.insecticides = 4), c(0.1, 0.2, 0.3, 0.4))
})


test_that("length is 1", {
  expect_equal(allow_unique_fitness_costs(resistance.cost = 0.1,
                                          number.of.insecticides = 4), c(0.1, 0.1, 0.1, 0.1))
})

test_that("lengths differ", {
  expect_error(allow_unique_fitness_costs(resistance.cost = c(0.1, 0.2, 0.3),
                                          number.of.insecticides = 4), "length of resistance.cost must be 1 or number.of.insecticides")
})