test_that("cannot be negative", {
  expect_error(calculate_current_insecticide_concentration(applied.concentration = -1,
                                                           instantaneous.decay.rate = 0.05,
                                                           generations.since.deployment = 5), 
               "Error: Insecticide concentration cannot be negative")
})
