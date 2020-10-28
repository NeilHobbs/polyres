sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 10)

sim.array["treatment", 1, 5] = 150
sim.array["treatment", 2, 5] = 100
sim.array["treatment", 3, 5] = 80

available.vector = c(1,2,3)
withdrawn.vector = c()


test_that("insecticides are removed from withdrawn vector", {
  expect_equal(withdrawal_of_insecticides_from_arsenal(number.of.insecticides = 3,
                                                       current.generation = 5,
                                                       withdrawal.threshold = 100,
                                                       simulation.array = sim.array,
                                                       available.vector = available.vector,
                                                       withdrawn.vector = withdrawn.vector)[[1]], c(3))
})

test_that("insecticides are moved to withdrawn vector", {
  expect_equal(withdrawal_of_insecticides_from_arsenal(number.of.insecticides = 3,
                                                       current.generation = 5,
                                                       withdrawal.threshold = 100,
                                                       simulation.array = sim.array,
                                                       available.vector = available.vector,
                                                       withdrawn.vector = withdrawn.vector)[[2]], c(1,2))
})