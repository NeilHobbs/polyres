test_that("no decay occurs", {
  expect_equal(calculate_current_insecticide_efficacy(generations.since.deployment = 7,
                                                     threshold.generations = 10,
                                                     initial.insecticide.efficacy = 1,
                                                     base.efficacy.decay.rate = 0,
                                                     rapid.decay.rate = 0), 1)
})




test_that("initial.efficacy < 0", {
  expect_error(calculate_current_insecticide_efficacy(generations.since.deployment = 7,
                                                      threshold.generations = 10,
                                                      initial.insecticide.efficacy = -10,
                                                      base.efficacy.decay.rate = 0,
                                                      rapid.decay.rate = 0), "initial.insecticide.efficacy cannot be less than 0")
})


test_that("base.efficacy.decay.rate < 0", {
  expect_warning(calculate_current_insecticide_efficacy(generations.since.deployment = 7,
                                                      threshold.generations = 10,
                                                      initial.insecticide.efficacy = 1,
                                                      base.efficacy.decay.rate = -0.1,
                                                      rapid.decay.rate = 0), "base.efficacy.decay.rate is less than 0. Insecticide efficacy increases over time.")
})

test_that("base.efficacy.decay.rate < 0", {
  expect_warning(calculate_current_insecticide_efficacy(generations.since.deployment = 7,
                                                        threshold.generations = 10,
                                                        initial.insecticide.efficacy = 1,
                                                        base.efficacy.decay.rate = 0,
                                                        rapid.decay.rate = -0.1), "rapid.decay.rate is less than 0. Insecticide efficacy increases over time.")
})






