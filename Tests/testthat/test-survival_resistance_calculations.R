
test.values = c(200, 500, 900, 2000)

for(i in 1:length(test.values)){
test_that("50% survival is true", {
  expect_equal(bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1, #must be set to 1 to work properly
                                                          michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                          half.population.bioassay.survival.resistance = test.values[i], 
                                                          bioassay.survival = 0.5, 
                                                          estimate.precision = 0.00001, 
                                                          sd.population.resistance = 0,
                                                          nsim = 1000,
                                                          minimum.resistance.value = 0, 
                                                          maximum.resistance.value = 25000), test.values[i])
})}


test.values = c(-1, 1.1, 2)

for(i in 1:length(test.values)){
test_that("error when MM value wrong",{
  
  expect_error(bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1, #must be set to 1 to work properly
                                                          michaelis.menten.slope = test.values[i], #must be set to 1 to work properly
                                                          half.population.bioassay.survival.resistance = 900, 
                                                          bioassay.survival = 0.1, 
                                                          estimate.precision = 0.01, 
                                                          sd.population.resistance = 10,
                                                          nsim = 1000,
                                                          minimum.resistance.value = 0, 
                                                          maximum.resistance.value = 25000), "michaelis.menten.slope must equal 1",
                                                          fixed=TRUE)
})}

for(i in 1:length(test.values)){
test_that("error when max survival value wrong",{
  
  expect_error(bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = test.values[i], #must be set to 1 to work properly
                                               michaelis.menten.slope = 1, #must be set to 1 to work properly
                                               half.population.bioassay.survival.resistance = 900, 
                                               bioassay.survival = 0.1, 
                                               estimate.precision = 0.01, 
                                               sd.population.resistance = 10,
                                               nsim = 1000,
                                               minimum.resistance.value = 0, 
                                               maximum.resistance.value = 25000), "maximum.bioassay.survival.proportion must equal 1.",
                                               fixed=TRUE)
})
}

for(i in 1:length(test.values)){
  test_that("error when max survival value wrong",{
    
    expect_error(bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1, #must be set to 1 to work properly
                                                 michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                 half.population.bioassay.survival.resistance = 900, 
                                                 bioassay.survival = test.values[i], 
                                                 estimate.precision = 0.01, 
                                                 sd.population.resistance = 10,
                                                 nsim = 1000,
                                                 minimum.resistance.value = 0, 
                                                 maximum.resistance.value = 25000), "Bioassay survival must be between 0 and 1.",
                 fixed=TRUE)
  })
}







