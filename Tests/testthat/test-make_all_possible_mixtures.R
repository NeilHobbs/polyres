test_that("multiplication works", {
  expect_equal(nrow(make_all_possible_mixtures(number.of.insecticides = 3)), 3)
})
