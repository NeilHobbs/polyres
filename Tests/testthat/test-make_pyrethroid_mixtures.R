
test_that("gives correct number of mixtures", {
  expect_equal(nrow(make_pyrethroid_mixtures(number.of.insecticides = 7)), 6)
})



test_that("second column is only 1s", {
  expect_equal(make_pyrethroid_mixtures(number.of.insecticides = 7)[, 2], 
               c(1, 1, 1, 1, 1, 1))
})
