
test_that("gives error when number.of.insecticides is odd", {
  expect_error(make_mixture_sequential_discrete(number.of.insecticides = 17), 
               "the number.of.insecticides must be even")
})




test_that("gives error when number.of.insecticides is odd", {
  expect_equal(make_mixture_sequential_discrete(number.of.insecticides = 10)[ , 1], 
               c(1, 2, 3, 4, 5))
})

test_that("gives error when number.of.insecticides is odd", {
  expect_equal(make_mixture_sequential_discrete(number.of.insecticides = 10)[ , 2], 
               c(1, 3, 5, 7, 9))
})

test_that("gives error when number.of.insecticides is odd", {
  expect_equal(make_mixture_sequential_discrete(number.of.insecticides = 10)[ , 3], 
               c(2, 4, 6, 8, 10))
})