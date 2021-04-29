mixture.id = rep(1, times = 10)#first row, first column
mixture.part.1 = rep(1, times = 10)#first row, second column
mixture.part.2 = rep(2, times = 10)#first row, third column
deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)

sim.array = create_starting_array(n.insecticides = 4,
                                  maximum.generations = 200)
sim.array["treatment", 1, 100] = 200
sim.array["treatment", 2, 100] = 200
sim.array["treatment", 3, 100] = 20
sim.array["treatment", 4, 100] = 20

available.vector = c(1, 2, 3)
withdrawn.vector = c(4)

test_that("deploys for correct duration", {
  expect_equal(nrow(irm_strategy_sequence_mixture(number.of.insecticides = 4,
                                             current.generation = 100,
                                             withdrawal.threshold = 100,
                                             return.threshold = 100,
                                             simulation.array = sim.array,
                                             available.vector = available.vector,
                                             withdrawn.vector = withdrawn.vector,
                                             mixture.df = make_mixture_sequential_discrete(4),
                                             current.mixture = 1,
                                             deployment.frequency = 20,
                                             deployment.df = deployed.mixture)[[4]]), 30)
  
  expect_equal(nrow(irm_strategy_sequence_mixture(number.of.insecticides = 4,
                                             current.generation = 100,
                                             withdrawal.threshold = 100,
                                             return.threshold = 100,
                                             simulation.array = sim.array,
                                             available.vector = available.vector,
                                             withdrawn.vector = withdrawn.vector,
                                             mixture.df = make_mixture_sequential_discrete(4),
                                             current.mixture = 1,
                                             deployment.frequency = 10,
                                             deployment.df = deployed.mixture)[[4]]), 20)
})
