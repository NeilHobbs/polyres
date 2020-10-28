test.values = c(1, 50, 100, 1000, 5000)

for(i in 1:length(test.values)){
  test_that("returns 50% survival",{
    expect_equal(resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                 mean.population.resistance = test.values[i],
                                                 michaelis.menten.slope = 1, 
                                                 half.population.bioassay.survival.resistance = test.values[i],
                                                 sd.population.resistance = 0, 
                                                 nsim = 1), 0.5)})}
