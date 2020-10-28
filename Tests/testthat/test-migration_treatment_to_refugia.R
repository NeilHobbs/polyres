test_that("returns vector of correct length", {
  expect_equal(length(migration_treatment_to_refugia(nsim = 1000, 
                                                     min.intervention.coverage = 0.1, 
                                                     max.intervention.coverage = 0.9, 
                                                     min.dispersal.rate = 0.1,
                                                     max.dispersal.rate = 0.9)), 1000)
})

test.values = c(-0.1, 1.1)

for(i in 1:length(test.values))
  test_that("min.intervention.coverage out of range", {
    
    expect_error(migration_treatment_to_refugia(nsim = 1000,
                                                min.intervention.coverage = test.values[i],
                                                max.intervention.coverage = 0.9,
                                                min.dispersal.rate = 0.1,
                                                max.dispersal.rate = 0.9), "min.intervention.coverage must be between 0 and 1")
    
  })

for(i in 1:length(test.values))
  test_that("max.intervention.coverage out of range", {
    
    expect_error(migration_treatment_to_refugia(nsim = 1000,
                                                min.intervention.coverage = 0.1,
                                                max.intervention.coverage = test.values[i],
                                                min.dispersal.rate = 0.1,
                                                max.dispersal.rate = 0.9), "max.intervention.coverage must be between 0 and 1")
    
  })


for(i in 1:length(test.values))
  test_that("min.intervention.coverage > max.intervention.coverage", {
    
    expect_error(migration_treatment_to_refugia(nsim = 1000,
                                                min.intervention.coverage = 0.9,
                                                max.intervention.coverage = 0.1,
                                                min.dispersal.rate = 0.1,
                                                max.dispersal.rate = 0.9), "min.intervention.coverage is greater than max.intervention.coverage")
    
  })


for(i in 1:length(test.values))
  test_that("min.dispersal.rate out of range", {
    
    expect_error(migration_treatment_to_refugia(nsim = 1000,
                                                min.intervention.coverage = 0.1,
                                                max.intervention.coverage = 0.9,
                                                min.dispersal.rate = test.values[i],
                                                max.dispersal.rate = 0.9), "min.dispersal.rate must be between 0 and 1")
    
  })

for(i in 1:length(test.values))
  test_that("max.dispersal.rate out of range", {
    
    expect_error(migration_treatment_to_refugia(nsim = 1000,
                                                min.intervention.coverage = 0.1,
                                                max.intervention.coverage = 0.9,
                                                min.dispersal.rate = 0.1,
                                                max.dispersal.rate = test.values[i]), "max.dispersal.rate must be between 0 and 1")
    
  })


for(i in 1:length(test.values))
  test_that("min.dispersal.rate > max.dispersal.rate", {
    
    expect_error(migration_treatment_to_refugia(nsim = 1000,
                                                min.intervention.coverage = 0.1,
                                                max.intervention.coverage = 0.9,
                                                min.dispersal.rate = 0.9,
                                                max.dispersal.rate = 0.1), "min.dispersal.rate is greater than max.dispersal.rate")
    
  })