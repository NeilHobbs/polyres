test_that("calculates properly", {
  expect_equal(calculate_initial_efficacy_from_insecticide_concentration_mixtures(mixture.dose = 50,
                                                                                  recommended.insecticide.dose = 100), 0.5)
})


test_that("cannot have negative efficacy", {
  expect_error(calculate_initial_efficacy_from_insecticide_concentration_mixtures(mixture.dose = -50,
                                                                                  recommended.insecticide.dose = 100), 
               "Error: Insecticide efficacy cannot be negative")
})
