temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0)



test_that("No impact when there is no cross selection", {
  expect_equal(insecticide_not_deployed_indirect_cross_selection(exposure.scaling.factor = 10,
                                                                 nsim = 1000, 
                                                                 minimum.insecticide.resistance.heritability = 0.30, 
                                                                 maximum.insecticide.resistance.heritability = 0.30,
                                                                 minimum.male.insecticide.exposure = 0.5,
                                                                 maximum.male.insecticide.exposure = 0.5, 
                                                                 minimum.female.insecticide.exposure = 0.7, 
                                                                 maximum.female.insecticide.exposure = 0.7,
                                                                 resistance.cost = 0,
                                                                 initial.resistance.intensity = 30,
                                                                 cross.selection.matrix = temp.matrix,
                                                                 currently.deployed.insecticide = 2,
                                                                 currently.tracked.insecticide = 2), 
               insecticide_not_deployed_selection_cost(initial.resistance.intensity = 30,
                 resistance.cost = 0,
                 exposure.scaling.factor = 10,
                 nsim = 1000, 
                 minimum.insecticide.resistance.heritability = 0.30, 
                 maximum.insecticide.resistance.heritability = 0.30,
                 minimum.male.insecticide.exposure = 0.5,
                 maximum.male.insecticide.exposure = 0.5, 
                 minimum.female.insecticide.exposure = 0.7, 
                 maximum.female.insecticide.exposure = 0.7))
               
})


test_that("No impact when there is no cross selection, but there is resistance.cost", {
  expect_equal(insecticide_not_deployed_indirect_cross_selection(exposure.scaling.factor = 10,
                                                                 nsim = 1000, 
                                                                 minimum.insecticide.resistance.heritability = 0.30, 
                                                                 maximum.insecticide.resistance.heritability = 0.30,
                                                                 minimum.male.insecticide.exposure = 0.5,
                                                                 maximum.male.insecticide.exposure = 0.5, 
                                                                 minimum.female.insecticide.exposure = 0.7, 
                                                                 maximum.female.insecticide.exposure = 0.7,
                                                                 resistance.cost = 0.1,
                                                                 initial.resistance.intensity = 30,
                                                                 cross.selection.matrix = temp.matrix,
                                                                 currently.deployed.insecticide = 2,
                                                                 currently.tracked.insecticide = 2), 
               insecticide_not_deployed_selection_cost(initial.resistance.intensity = 30,
                                                       resistance.cost = 0.1,
                                                       exposure.scaling.factor = 10,
                                                       nsim = 1000, 
                                                       minimum.insecticide.resistance.heritability = 0.30, 
                                                       maximum.insecticide.resistance.heritability = 0.30,
                                                       minimum.male.insecticide.exposure = 0.5,
                                                       maximum.male.insecticide.exposure = 0.5, 
                                                       minimum.female.insecticide.exposure = 0.7, 
                                                       maximum.female.insecticide.exposure = 0.7))
})