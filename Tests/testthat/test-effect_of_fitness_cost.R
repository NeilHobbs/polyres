test_that("resistance cost gives negative outcome", {
  expect_equal(effect_of_fitness_cost(resistance.cost = 1,
                                      exposure.scaling.factor = 1,
                                      nsim = 1, 
                                      minimum.insecticide.resistance.hertitability = 1, 
                                      maximum.insecticide.resistance.hertitability = 1,
                                      minimum.male.insecticide.exposure = 1,
                                      maximum.male.insecticide.exposure = 1, 
                                      minimum.female.insecticide.exposure = 1, 
                                      maximum.female.insecticide.exposure = 1), -1)
})

test_that("exposure scaling factor has effect", {
  expect_equal(effect_of_fitness_cost(resistance.cost = 1,
                                      exposure.scaling.factor = 10,
                                      nsim = 1, 
                                      minimum.insecticide.resistance.hertitability = 1, 
                                      maximum.insecticide.resistance.hertitability = 1,
                                      minimum.male.insecticide.exposure = 1,
                                      maximum.male.insecticide.exposure = 1, 
                                      minimum.female.insecticide.exposure = 1, 
                                      maximum.female.insecticide.exposure = 1), -10)
})