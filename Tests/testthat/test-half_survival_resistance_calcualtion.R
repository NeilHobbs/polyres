test.values = c(100, 500, 900, 1000, 5000)

for(i in 1:length(test.values)){
test_that("multiplication works", {
  expect_equal(calculate_half_population_survival(desired.resistance = test.values[i],
                                                  desired.survival.proportion = 0.5,
                                                  maximum.bioassay.survival.proportion = 1,
                                                  michaelis.menten.slope = 1, 
                                                  estimate.precision = 0.0000001, 
                                                  sd.population.resistance = 0,
                                                  nsim = 1000,
                                                  minimum.resistance.value = 0, 
                                                  maximum.resistance.value = 25000), test.values[i])
})
}