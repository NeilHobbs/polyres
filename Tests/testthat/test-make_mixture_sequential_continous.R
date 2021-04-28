test_that("gives correct number of mixtures", {
  expect_equal(nrow(make_mixture_sequential_continous(number.of.insecticides = 12)), 12)
})

test_that("gives correct part 1", {
  expect_equal(make_mixture_sequential_continous(number.of.insecticides = 5)[,2], 
               c(1, 2, 3, 4, 5))
})

test_that("gives correct part 2", {
  expect_equal(make_mixture_sequential_continous(number.of.insecticides = 5)[,3], 
               c(2, 3, 4, 5, 1))
})

test_that("gives correct mixture id", {
  expect_equal(make_mixture_sequential_continous(number.of.insecticides = 5)[,2], 
               c(1, 2, 3, 4, 5))
})
