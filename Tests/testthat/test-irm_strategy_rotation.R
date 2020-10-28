test.values = c(10, 20, 30)
check.values = c(110, 120, 130)


sim.array = create_starting_array(n.insecticides = 3,
                                   maximum.generations = 200)
sim.array["treatment", 1, 100] = 200
sim.array["treatment", 2, 100] = 200
sim.array["treatment", 3, 100] = 20
available.vector = c(1,3)
withdrawn.vector = c(2)
deployed.insecticide = rep(1, times = 100)

for(i in 1:length(test.values)){
test_that("deployment vector is extended by correct amount", {
  expect_equal(length(irm_strategy_rotation(number.of.insecticides = 3,
                                     current.generation = 100,
                                     withdrawal.threshold = 100,
                                     return.threshold = 50,
                                     simulation.array = sim.array,
                                     available.vector = available.vector,
                                     withdrawn.vector = withdrawn.vector,
                                     current.insecticide = 1,
                                     deployment.frequency = test.values[i],
                                     deployment.vector = deployed.insecticide)[[3]]), check.values[i]
)})}

#test for NA deployment

sim.array["treatment", 1, 50] = 200
sim.array["treatment", 2, 50] = 200
sim.array["treatment", 3, 50] = 200
available.vector = c(1,3)
withdrawn.vector = c(2)
deployed.insecticide = rep(1, times = 100)



test_that("NA is produced when no  insecticides available",{
  expect_identical(is.na((irm_strategy_rotation(number.of.insecticides = 3,
                                      current.generation = 50,
                                      withdrawal.threshold = 100,
                                      return.threshold = 50,
                                      simulation.array = sim.array,
                                      available.vector = available.vector,
                                      withdrawn.vector = withdrawn.vector,
                                      current.insecticide = 1,
                                      deployment.frequency = 10,
                                      deployment.vector = deployed.insecticide)[[3]][101])), TRUE)
})

sim.array["treatment", 1, 200] = 20
sim.array["treatment", 2, 200] = 20
sim.array["treatment", 3, 200] = 100
available.vector = c(1)
withdrawn.vector = c(2,3)
deployed.insecticide = rep(1, times = 100)

test_that("available vector is returned",{
  
  expect_equal(irm_strategy_rotation(number.of.insecticides = 3,
                                     current.generation = 200,
                                     withdrawal.threshold = 100,
                                     return.threshold = 50,
                                     simulation.array = sim.array,
                                     available.vector = available.vector,
                                     withdrawn.vector = withdrawn.vector,
                                     current.insecticide = 1,
                                     deployment.frequency = 10,
                                     deployment.vector = deployed.insecticide)[[1]], c(1, 2))
  
})


test_that("withdrawn vector is returned", {
  expect_equal(irm_strategy_rotation(number.of.insecticides = 3,
                                     current.generation = 200,
                                     withdrawal.threshold = 100,
                                     return.threshold = 50,
                                     simulation.array = sim.array,
                                     available.vector = available.vector,
                                     withdrawn.vector = withdrawn.vector,
                                     current.insecticide = 1,
                                     deployment.frequency = 10,
                                     deployment.vector = deployed.insecticide)[[2]], 3)
})


sim.array["treatment", 1, 150] = 20
sim.array["treatment", 2, 150] = 200
sim.array["treatment", 3, 150] = 200
available.vector = c(1)
withdrawn.vector = c(2,3)
deployed.insecticide = rep(1, times = 100)

test_that("cannot redeploy the current insecticide", {
  expect_equal(is.na(irm_strategy_rotation(number.of.insecticides = 3,
                                     current.generation = 150,
                                     withdrawal.threshold = 100,
                                     return.threshold = 50,
                                     simulation.array = sim.array,
                                     available.vector = available.vector,
                                     withdrawn.vector = withdrawn.vector,
                                     current.insecticide = 1,
                                     deployment.frequency = 10,
                                     deployment.vector = deployed.insecticide)[[3]][101]), TRUE)
})







 
