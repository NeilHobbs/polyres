mixture.id = rep(1, times = 10)#first row, first column
mixture.part.1 = rep(1, times = 10)#first row, second column
mixture.part.2 = rep(2, times = 10)#first row, third column

#The dataframe that holds the mixture deployment information
deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)


test_that("deployment frequency works", {
  expect_equal(nrow(deploy_mixture(candidate.mixture.id = 2,
                                   mixture.df = make_mixture_sequential_continous(4),
                                   deployment.df = deployed.mixture ,
                                   deployment.frequency = 5)), 15)
  expect_equal(nrow(deploy_mixture(candidate.mixture.id = 2,
                                   mixture.df = make_mixture_sequential_continous(4),
                                   deployment.df = deployed.mixture ,
                                   deployment.frequency = 10)), 20)
  expect_equal(nrow(deploy_mixture(candidate.mixture.id = 2,
                                   mixture.df = make_mixture_sequential_continous(4),
                                   deployment.df = deployed.mixture ,
                                   deployment.frequency = 20)), 30)
})


test_that("second mixture is deployed", {
  expect_equal(deploy_mixture(candidate.mixture.id = 2,
                                   mixture.df = make_mixture_sequential_continous(4),
                                   deployment.df = deployed.mixture ,
                                   deployment.frequency = 10)[, 1],
               c(rep(1, 10), rep(2, 10)))

})





