test_that("when cross selection is 1, identical to insecticide being deployed", {
  expect_equal(insecticide_deployed_cross_selection_decay(currently.deployed.insecticide = 1, 
                                           number.of.insecticides = 2,
                                           cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                min.cross.selection = 1,
                                                                                                max.cross.selection = 1),
                                           exposure.scaling.factor = 10,
                                           nsim = 1, 
                                           minimum.insecticide.resistance.heritability = 1, 
                                           maximum.insecticide.resistance.heritability = 1,
                                           minimum.male.insecticide.exposure = 1,
                                           maximum.male.insecticide.exposure = 1, 
                                           minimum.female.insecticide.exposure = 1, 
                                           maximum.female.insecticide.exposure = 1,
                                           resistance.cost = 0,
                                           current.insecticide.efficacy = 1), insecticide_deployed_selection_cost(exposure.scaling.factor = 10,
                                    nsim = 1, 
                                    minimum.insecticide.resistance.heritability = 1, 
                                    maximum.insecticide.resistance.heritability = 1,
                                    minimum.male.insecticide.exposure = 1,
                                    maximum.male.insecticide.exposure = 1, 
                                    minimum.female.insecticide.exposure = 1, 
                                    maximum.female.insecticide.exposure = 1,
                                    resistance.cost = 0))
})







