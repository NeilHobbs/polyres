sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 10)

sim.array["treatment", 1, 5] = 100
sim.array["treatment", 2, 5] = 100
sim.array["treatment", 3, 5] = 100

available.vector = c()
withdrawn.vector = c(1,2,3)

test_that("adds insecticides from withdrawn to available", {
  expect_equal(return_of_insecticides_to_arsenal(number.of.insecticides = 3,
                                                 current.generation = 5,
                                                 return.threshold = 150,
                                                 simulation.array = sim.array,
                                                 available.vector = available.vector,
                                                 withdrawn.vector = withdrawn.vector)[[1]], c(1,2,3))
})

test_that("removes insecticide from withdrawn",{
  
  expect_equal(return_of_insecticides_to_arsenal(number.of.insecticides = 3,
                                                 current.generation = 5,
                                                 return.threshold = 150,
                                                 simulation.array = sim.array,
                                                 available.vector = available.vector,
                                                 withdrawn.vector = withdrawn.vector)[[2]], c(numeric(0)))
  
  
})
