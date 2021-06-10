test_that("never negative", {
  expect_equal(convert_bioassay_survival_to_field_insecticide_decay(conversion.factor = 0.2,
                                                                    bioassay.survival = 0.8,
                                                                    intercept = - 1000,
                                                                    current.insecticide.efficacy = 1), 0)
})


test_that("never greater than 1", {
  expect_equal(convert_bioassay_survival_to_field_insecticide_decay(conversion.factor = 0.2,
                                                                    bioassay.survival = 0.8,
                                                                    intercept = 1000,
                                                                    current.insecticide.efficacy = 1), 1)
})


test_that("no insecticide full survival", {
  expect_equal(convert_bioassay_survival_to_field_insecticide_decay(conversion.factor = 0.48,
                                                                    bioassay.survival = 0.05,
                                                                    intercept = 0.15,
                                                                    current.insecticide.efficacy = 0), 1)
})