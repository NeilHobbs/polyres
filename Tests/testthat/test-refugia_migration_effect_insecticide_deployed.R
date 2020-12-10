test_that("no change when coverage is 1", {
  
  expect_equal(refugia_migration_effect_insecticide_deployed(initial.refugia.resistance = 50,
                                                             initial.resistance.intensity =50,
                                                             resistance.cost = 0,
                                                             exposure.scaling.factor = 10,
                                                             nsim = 1, 
                                                             minimum.insecticide.resistance.heritability = 0.30, 
                                                             maximum.insecticide.resistance.heritability = 0.30,
                                                             minimum.male.insecticide.exposure = 1,
                                                             maximum.male.insecticide.exposure = 1, 
                                                             minimum.female.insecticide.exposure = 1, 
                                                             maximum.female.insecticide.exposure = 1,
                                                             min.intervention.coverage = 1, 
                                                             max.intervention.coverage = 1, 
                                                             min.dispersal.rate = 0,
                                                             max.dispersal.rate = 0), 50)
  
  
})

test_that("no change when no female exposure", {
  
  expect_equal(refugia_migration_effect_insecticide_deployed(initial.refugia.resistance = 50,
                                                             initial.resistance.intensity =50,
                                                             resistance.cost = 0,
                                                             exposure.scaling.factor = 10,
                                                             nsim = 1, 
                                                             minimum.insecticide.resistance.heritability = 0.30, 
                                                             maximum.insecticide.resistance.heritability = 0.30,
                                                             minimum.male.insecticide.exposure = 1,
                                                             maximum.male.insecticide.exposure = 1, 
                                                             minimum.female.insecticide.exposure = 0, 
                                                             maximum.female.insecticide.exposure = 0,
                                                             min.intervention.coverage = 1, 
                                                             max.intervention.coverage = 1, 
                                                             min.dispersal.rate = 1,
                                                             max.dispersal.rate = 1), 50)
  
  
})