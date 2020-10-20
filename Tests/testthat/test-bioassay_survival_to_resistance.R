#These tests confirm that the conversion from bioassay survival to resistance intensity work correctly:

test_that("errror for incorrect max survival"){
  
  expect_error(bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 2,
                                               michaelis.menten.slope = 1, #must be set to 1 to work properly
                                               half.population.bioassay.survival.resistance = 900, 
                                               bioassay.survival = 0.5, 
                                               estimate.precision = 0.00001, 
                                               sd.population.resistance = 0,
                                               nsim = 1000,
                                               minimum.resistance.value = 10, 
                                               maximum.resistance.value = 25000), "maximum.bioassay.survival.proportion must equal 1.")
}