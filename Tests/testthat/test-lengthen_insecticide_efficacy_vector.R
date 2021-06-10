test_that("lengthening works", {
  expect_equal(length(lengthen_insecticide_efficacy_vector(previous.efficacy.vector = rep(1, 10),
                                                    new.efficacy.vector = rep(2, 17))), 27)
})
