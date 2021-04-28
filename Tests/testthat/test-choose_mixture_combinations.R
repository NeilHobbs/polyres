test_that("returns the correct number of mixtures", {
  expect_equal(nrow(choose_mixture_combinations(number.of.mixtures = 5,
                                           potential.mixtures = make_all_possible_mixtures(number.of.insecticides = 5))), 5)
})


test_that("returns the correct number of mixtures", {
  expect_equal(choose_mixture_combinations(number.of.mixtures = 5,
                                                potential.mixtures = make_all_possible_mixtures(number.of.insecticides = 5))[ , 1], 
               c(1, 2, 3, 4, 5))
})
