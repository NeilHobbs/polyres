test_that("is length of the deployment vector", {
  expect_equal(length(create_insecticide_efficacy_vector(applied.insecticide.dose = 100,
                                                  recommended.insecticide.dose = 100,
                                                  threshold.generations = 10,
                                                  base.efficacy.decay.rate = 0.01,
                                                  rapid.decay.rate = 0.2,
                                                  deployment.frequency = 20)), 20)
})


test_that("no decay on application generation", {
  expect_equal(create_insecticide_efficacy_vector(applied.insecticide.dose = 100,
                                                         recommended.insecticide.dose = 100,
                                                         threshold.generations = 10,
                                                         base.efficacy.decay.rate = 0.2,
                                                         rapid.decay.rate = 0.2,
                                                         deployment.frequency = 1), 1)
})


test_that("no decay occurs", {
  expect_equal(create_insecticide_efficacy_vector(applied.insecticide.dose = 100,
                                                  recommended.insecticide.dose = 100,
                                                  threshold.generations = 10,
                                                  base.efficacy.decay.rate = 0,
                                                  rapid.decay.rate = 0,
                                                  deployment.frequency = 10), rep(1, 10))
})