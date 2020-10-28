sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 10)

sim.array["treatment", 1, 1] = 200
sim.array["treatment", 2, 1] = 200
sim.array["treatment", 3, 1] = 200

available.vector = c(1,2,3)
withdrawn.vector = c()

test_that("gives the correct withdraw insecticide",{
  expect_equal(return_and_withdrawal_of_insecticides(number.of.insecticides = 3,
                                                     current.generation = 1,
                                                     withdrawal.threshold = 100,
                                                     return.threshold = 50,
                                                     simulation.array = sim.array,
                                                     available.vector = available.vector,
                                                     withdrawn.vector = withdrawn.vector)[[2]], c(1,2,3))})


return_and_withdrawal_of_insecticides(number.of.insecticides = 3,
                                      current.generation = 1,
                                      withdrawal.threshold = 100,
                                      return.threshold = 50,
                                      simulation.array = sim.array,
                                      available.vector = available.vector,
                                      withdrawn.vector = withdrawn.vector)[[1]]

test_that("gives correct available insecticide",{
  
  expect_equal(length(return_and_withdrawal_of_insecticides(number.of.insecticides = 3,
                                                     current.generation = 1,
                                                     withdrawal.threshold = 100,
                                                     return.threshold = 50,
                                                     simulation.array = sim.array,
                                                     available.vector = available.vector,
                                                     withdrawn.vector = withdrawn.vector)[[1]]), 0)})