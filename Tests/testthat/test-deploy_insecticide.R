A = deploy_insecticide(insecticide.to.deploy = 1,
                         deployment.frequency = 10,
                         deployment.vector = rep(2, times = 10))




test_that("nothing is deployed after the final deployment", {
  expect_equal(is.na(A[21]), TRUE)
})

test_that("deploys for the correct length", {
  expect_equal(length(A), 20)
})

test_that("correct value is deployed",{
 expect_equal(A[14], 1) 
})