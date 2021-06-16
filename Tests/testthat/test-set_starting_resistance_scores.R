test_that("sets unique scores", {
  expect_equal(c(set_starting_resistance_scores(sim.array = create_starting_array(n.insecticides = 3,
                                                                                  maximum.generations = 10),
                                                starting.refugia.resistance.score = c(1, 3, 7),
                                                starting.intervention.resistance.score = c(5, 2, 8),
                                                number.of.insecticides = 3)[, , generation = 1]), c(1,5,3,2,7,8))
})


test_that("NAs for next generation", {
  expect_equivalent(is.na(c((set_starting_resistance_scores(sim.array = create_starting_array(n.insecticides = 2,
                                                                                         maximum.generations = 3),
                                                       starting.refugia.resistance.score = c(1, 3),
                                                       starting.intervention.resistance.score = c(5, 2),
                                                       number.of.insecticides = 2)[ , , 2]))), rep(TRUE, 4))
})