test_that("error message for mix.sequential.discrete when odd", {
  expect_error(select_mixing_stategy(mixture.strategy = "mix.sequential.discrete",
                                     number.of.insecticides = 3), "the number.of.insecticides must be even")
})


