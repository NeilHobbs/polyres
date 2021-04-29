








test_that("when survival set to be one, gives same response as when not in mixture", {
  expect_equal(insecticide_deployed_selection_cost_mixture(exposure.scaling.factor = 10,
                                                           nsim = 1, 
                                                           minimum.insecticide.resistance.heritability = 0.30, 
                                                           maximum.insecticide.resistance.heritability = 0.30,
                                                           minimum.male.insecticide.exposure = 1,
                                                           maximum.male.insecticide.exposure = 1, 
                                                           minimum.female.insecticide.exposure = 0.9, 
                                                           maximum.female.insecticide.exposure = 0.9,
                                                           resistance.cost = 0.1,
                                                           initial.resistance.intensity = 0,
                                                           intensity.to.other.mixture.part = 900,
                                                           half.population.bioassay.survival.resistance = 900,
                                                           conversion.factor = 1,
                                                           intercept = 1), insecticide_deployed_selection_cost(exposure.scaling.factor = 10,
                                                                                                               nsim = 1, 
                                                                                                               minimum.insecticide.resistance.heritability = 0.30, 
                                                                                                               maximum.insecticide.resistance.heritability = 0.30,
                                                                                                               minimum.male.insecticide.exposure = 1,
                                                                                                               maximum.male.insecticide.exposure = 1, 
                                                                                                               minimum.female.insecticide.exposure = 0.9, 
                                                                                                               maximum.female.insecticide.exposure = 0.9,
                                                                                                               resistance.cost = 0.1,
                                                                                                               initial.resistance.intensity = 0))
})

test_that("halves response when 50% mixture survival", {
  expect_equal(insecticide_deployed_selection_cost_mixture(exposure.scaling.factor = 10,
                                                           nsim = 1, 
                                                           minimum.insecticide.resistance.heritability = 0.30, 
                                                           maximum.insecticide.resistance.heritability = 0.30,
                                                           minimum.male.insecticide.exposure = 1,
                                                           maximum.male.insecticide.exposure = 1, 
                                                           minimum.female.insecticide.exposure = 0.9, 
                                                           maximum.female.insecticide.exposure = 0.9,
                                                           resistance.cost = 0.1,
                                                           initial.resistance.intensity = 0,
                                                           intensity.to.other.mixture.part = 900,
                                                           half.population.bioassay.survival.resistance = 900,
                                                           conversion.factor = 1,
                                                           intercept = 0), insecticide_deployed_selection_cost(exposure.scaling.factor = 10,
                                                                                                               nsim = 1, 
                                                                                                               minimum.insecticide.resistance.heritability = 0.30, 
                                                                                                               maximum.insecticide.resistance.heritability = 0.30,
                                                                                                               minimum.male.insecticide.exposure = 1,
                                                                                                               maximum.male.insecticide.exposure = 1, 
                                                                                                               minimum.female.insecticide.exposure = 0.9, 
                                                                                                               maximum.female.insecticide.exposure = 0.9,
                                                                                                               resistance.cost = 0.1,
                                                                                                               initial.resistance.intensity = 0)/2)
})