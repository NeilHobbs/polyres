test_that("calculates efficacy correctly", {
  expect_equal(calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose = 100,
                                                                         recommended.insecticide.dose = 100), 1)
})

test_that("calculates efficacy correctly", {
  expect_error(calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose = -100,
                                                                         recommended.insecticide.dose = 100), 
               "Error: Insecticide efficacy cannot be negative")
})
