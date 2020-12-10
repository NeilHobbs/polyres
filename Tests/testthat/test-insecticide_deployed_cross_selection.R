temp.matrix_0 = make_cross_selection_matrix(number.of.insecticides = 3,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0)

test_that("cross selection does not occur when zero", {
  expect_equal(insecticide_deployed_cross_selection(currently.deployed.insecticide = 1, 
                                                    number.of.insecticides = 3,
                                                    cross.selection.matrix = temp.matrix_0, 
                                                    exposure.scaling.factor = 10,
                                                    nsim = 1, 
                                                    minimum.insecticide.resistance.heritability = 1, 
                                                    maximum.insecticide.resistance.heritability = 1,
                                                    minimum.male.insecticide.exposure = 1,
                                                    maximum.male.insecticide.exposure = 1, 
                                                    minimum.female.insecticide.exposure = 1, 
                                                    maximum.female.insecticide.exposure = 1,
                                                    resistance.cost = 0), 0)
})
