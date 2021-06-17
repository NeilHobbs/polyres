test_that("withdraws all the mixtures", {
  expect_equal(nrow(find_available_mixtures(mixture.df = make_mixture_sequential_discrete(4),
                                       withdrawn.insecticides = c(1, 3))), 0)
})


test_that("returns only the available mixture", {
  expect_equal(find_available_mixtures(mixture.df = make_mixture_sequential_discrete(4),
                                       withdrawn.insecticides = c(1))[1 , 1], 2
  )
    expect_equal(find_available_mixtures(mixture.df = make_mixture_sequential_discrete(4),
                                       withdrawn.insecticides = c(1))[1 , 2], 3
  )
  expect_equal(find_available_mixtures(mixture.df = make_mixture_sequential_discrete(4),
                                       withdrawn.insecticides = c(1))[1 , 3], 4
  )
})


test_that("returns only the available mixture", {
  expect_equal(find_available_mixtures(mixture.df = make_mixture_sequential_discrete(4),
                                       withdrawn.insecticides = c(1))[1 , 1], 2
  )
  expect_equal(find_available_mixtures(mixture.df = make_mixture_sequential_discrete(4),
                                       withdrawn.insecticides = c(1))[1 , 2], 3
  )
  expect_equal(find_available_mixtures(mixture.df = make_mixture_sequential_discrete(4),
                                       withdrawn.insecticides = c(1))[1 , 3], 4
  )
})