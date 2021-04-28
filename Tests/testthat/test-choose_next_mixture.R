test_that("returns to the beginning", {
  expect_equal(choose_next_mixture(previous.mixture = 3,
                                   total.mixtures = 3,
                                   available.mixtures = c(1, 2))
               , 1)
})


test_that("goes to next mixture", {
  expect_equal(choose_next_mixture(previous.mixture = 2,
                                   total.mixtures = 3,
                                   available.mixtures = c(1, 3))
               , 3)
})
