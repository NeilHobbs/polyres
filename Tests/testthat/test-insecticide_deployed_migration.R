test_that("no change", {
  expect_equal(insecticide_deployed_migration(exposure.scaling.factor = 1,
                                              nsim = 1, 
                                              minimum.insecticide.resistance.heritability = 1, 
                                              maximum.insecticide.resistance.heritability = 1,
                                              minimum.male.insecticide.exposure = 1,
                                              maximum.male.insecticide.exposure = 1, 
                                              minimum.female.insecticide.exposure = 1, 
                                              maximum.female.insecticide.exposure = 1,
                                              resistance.cost = 0,
                                              initial.resistance.intensity = 1,
                                              min.intervention.coverage = 0, 
                                              max.intervention.coverage = 0, 
                                              initial.refugia.resistance = 1,
                                              min.dispersal.rate = 1,
                                              max.dispersal.rate = 1), 1)
})
