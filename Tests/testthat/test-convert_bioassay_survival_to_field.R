test_that("cannot fall below 0", {
  expect_equal(convert_bioassay_survival_to_field(bioassay.survival = 0.1,
                                                  conversion.factor = 2,
                                                  intercept = -1), 0)
})


test_that("cannot rise above 1", {
  expect_equal(convert_bioassay_survival_to_field(bioassay.survival = 0.9,
                                                  conversion.factor = 2,
                                                  intercept = 1), 1)
})


test_that("calculates correctly", {
  expect_equal(convert_bioassay_survival_to_field(bioassay.survival = 0.5,
                                                  conversion.factor = 1,
                                                  intercept = 0), 0.5)
  expect_equal(convert_bioassay_survival_to_field(bioassay.survival = 0.5,
                                                  conversion.factor = 0.5,
                                                  intercept = 0.1), 0.35)
  expect_equal(convert_bioassay_survival_to_field(bioassay.survival = 0.1,
                                                  conversion.factor = 1,
                                                  intercept = 0.1), 0.2)
})