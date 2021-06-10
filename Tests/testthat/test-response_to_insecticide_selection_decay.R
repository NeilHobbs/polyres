test_that("when efficacy is 1, same as response_to_insecticide_selection function", {
  expect_equal(response_to_insecticide_selection_decay(exposure.scaling.factor = 10,
                                                       nsim = 1, 
                                                       minimum.insecticide.resistance.heritability = 0.2, 
                                                       maximum.insecticide.resistance.heritability = 0.2,
                                                       minimum.male.insecticide.exposure = 1,
                                                       maximum.male.insecticide.exposure = 1, 
                                                       minimum.female.insecticide.exposure = 1, 
                                                       maximum.female.insecticide.exposure = 1,
                                                       current.insecticide.efficacy = 1),
               response_to_insecticide_selection(exposure.scaling.factor = 10,
                                                 nsim = 1, 
                                                 minimum.insecticide.resistance.heritability = 0.2, 
                                                 maximum.insecticide.resistance.heritability = 0.2,
                                                 minimum.male.insecticide.exposure = 1,
                                                 maximum.male.insecticide.exposure = 1, 
                                                 minimum.female.insecticide.exposure = 1, 
                                                 maximum.female.insecticide.exposure = 1))
})

test_that("when efficacy is 0 no response", {
  expect_equal(response_to_insecticide_selection_decay(exposure.scaling.factor = 10,
                                                       nsim = 1, 
                                                       minimum.insecticide.resistance.heritability = 0.2, 
                                                       maximum.insecticide.resistance.heritability = 0.2,
                                                       minimum.male.insecticide.exposure = 1,
                                                       maximum.male.insecticide.exposure = 1, 
                                                       minimum.female.insecticide.exposure = 1, 
                                                       maximum.female.insecticide.exposure = 1,
                                                       current.insecticide.efficacy = 0), 0)
})