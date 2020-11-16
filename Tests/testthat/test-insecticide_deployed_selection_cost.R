test_that("returns correct vector length", {
  expect_equal(length(insecticide_deployed_selection_cost (
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.05, 
    maximum.insecticide.resistance.heritability = 0.30,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9,
    resistance.cost = 0.1,
    initial.resistance.intensity = 0)), 1000)
})


#Test the error and warning messages.
test.values = c(-1, 1.1)


#Heritability
for(i in 1:length(test.values)){
test_that("heritability min error", {
  
  expect_error(insecticide_deployed_selection_cost (
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = test.values[i], 
    maximum.insecticide.resistance.heritability = 0.30,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9,
    resistance.cost = 0.1,
    initial.resistance.intensity = 0), "minimum.insecticide.resistance.heritability must be between 0 and 1")
  
  
})}

for(i in 1:length(test.values)){
  test_that("heritability max error", {
    
    expect_error(insecticide_deployed_selection_cost (
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = test.values[i],
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9,
      resistance.cost = 0.1,
      initial.resistance.intensity = 0), "maximum.insecticide.resistance.heritability must be between 0 and 1")
  })}


test_that("heritability min>max error", {
    
    expect_error(insecticide_deployed_selection_cost (
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.3, 
      maximum.insecticide.resistance.heritability = 0.1,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9,
      resistance.cost = 0.1,
      initial.resistance.intensity = 0), "minimum.insecticide.resistance.heritability is greater than maximum.insecticide.resistance.heritability")
})


#male.insecticide.exposure
for(i in 1:length(test.values)){
  test_that("male.exposure min error", {
    
    expect_error(insecticide_deployed_selection_cost (
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.30,
      minimum.male.insecticide.exposure = test.values[i],
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9,
      resistance.cost = 0.1,
      initial.resistance.intensity = 0), "minimum.male.insecticide.exposure must be between 0 and 1")
    
    
  })}

for(i in 1:length(test.values)){
  test_that("male.exposure max error", {
    
    expect_error(insecticide_deployed_selection_cost (
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.3,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = test.values[i], 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9,
      resistance.cost = 0.1,
      initial.resistance.intensity = 0), "maximum.male.insecticide.exposure must be between 0 and 1")
  })}


test_that("male.exposure min>max error", {
  
  expect_error(insecticide_deployed_selection_cost (
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.1, 
    maximum.insecticide.resistance.heritability = 0.4,
    minimum.male.insecticide.exposure = 0.5,
    maximum.male.insecticide.exposure = 0.4, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9,
    resistance.cost = 0.1,
    initial.resistance.intensity = 0), "minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")
})

#female.insecticide.exposure
for(i in 1:length(test.values)){
  test_that("female.exposure min error", {
    
    expect_error(insecticide_deployed_selection_cost (
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.30,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = test.values[i], 
      maximum.female.insecticide.exposure = 0.9,
      resistance.cost = 0.1,
      initial.resistance.intensity = 0), "minimum.female.insecticide.exposure must be between 0 and 1")
    
  })}

for(i in 1:length(test.values)){
  test_that("female.exposure max error", {
    
    expect_error(insecticide_deployed_selection_cost (
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.3,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 0.4, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = test.values[i],
      resistance.cost = 0.1,
      initial.resistance.intensity = 0), "maximum.female.insecticide.exposure must be between 0 and 1")
  })}


test_that("female.exposure min>max error", {
  
  expect_error(insecticide_deployed_selection_cost (
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.2, 
    maximum.insecticide.resistance.heritability = 0.4,
    minimum.male.insecticide.exposure = 0.1,
    maximum.male.insecticide.exposure = 0.5, 
    minimum.female.insecticide.exposure = 0.9, 
    maximum.female.insecticide.exposure = 0.4,
    resistance.cost = 0.1,
    initial.resistance.intensity = 0), "minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")
})