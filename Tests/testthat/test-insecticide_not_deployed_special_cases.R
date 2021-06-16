test_that("no cross selection no change", {
  expect_equal(insecticide_not_deployed_special_cases(exposure.scaling.factor = 10,
                                                      nsim = 1, 
                                                      minimum.insecticide.resistance.heritability = 1, 
                                                      maximum.insecticide.resistance.heritability = 1,
                                                      minimum.male.insecticide.exposure = 1,
                                                      maximum.male.insecticide.exposure = 1, 
                                                      minimum.female.insecticide.exposure = 1, 
                                                      maximum.female.insecticide.exposure = 1,
                                                      resistance.cost = 0,
                                                      initial.resistance.intensity = 0,
                                                      cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                           min.cross.selection = 0,
                                                                                                           max.cross.selection = 0),
                                                      currently.deployed.insecticide =2,
                                                      currently.tracked.insecticide = 1,
                                                      current.insecticide.efficacy = 1), 0)
})

test_that("cross selection occurs", {
  expect_equal(insecticide_not_deployed_special_cases(exposure.scaling.factor = 10,
                                                      nsim = 1, 
                                                      minimum.insecticide.resistance.heritability = 1, 
                                                      maximum.insecticide.resistance.heritability = 1,
                                                      minimum.male.insecticide.exposure = 1,
                                                      maximum.male.insecticide.exposure = 1, 
                                                      minimum.female.insecticide.exposure = 1, 
                                                      maximum.female.insecticide.exposure = 1,
                                                      resistance.cost = 0,
                                                      initial.resistance.intensity = 0,
                                                      cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                           min.cross.selection = 1,
                                                                                                           max.cross.selection = 1),
                                                      currently.deployed.insecticide =2,
                                                      currently.tracked.insecticide = 1,
                                                      current.insecticide.efficacy = 1), 10)
})



test_that("resistance cannot fall below 0", {
  expect_equal(insecticide_not_deployed_special_cases(exposure.scaling.factor = 10,
                                                      nsim = 1, 
                                                      minimum.insecticide.resistance.heritability = 1, 
                                                      maximum.insecticide.resistance.heritability = 1,
                                                      minimum.male.insecticide.exposure = 1,
                                                      maximum.male.insecticide.exposure = 1, 
                                                      minimum.female.insecticide.exposure = 1, 
                                                      maximum.female.insecticide.exposure = 1,
                                                      resistance.cost = 10,
                                                      initial.resistance.intensity = 0,
                                                      cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                           min.cross.selection = 1,
                                                                                                           max.cross.selection = 1),
                                                      currently.deployed.insecticide =2,
                                                      currently.tracked.insecticide = 1,
                                                      current.insecticide.efficacy = 1), 0)
})




