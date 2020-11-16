test_that("resistance is uninheritable", {
  expect_equal(response_to_insecticide_selection (exposure.scaling.factor = 10,
                                                  nsim = 1, 
                                                  minimum.insecticide.resistance.heritability = 0, 
                                                  maximum.insecticide.resistance.heritability = 0,
                                                  minimum.male.insecticide.exposure = 1,
                                                  maximum.male.insecticide.exposure = 1, 
                                                  minimum.female.insecticide.exposure = 1, 
                                                  maximum.female.insecticide.exposure = 1), 0)
})

test_that("no female insecticide exposure", {
  expect_equal(response_to_insecticide_selection (exposure.scaling.factor = 10,
                                                  nsim = 1, 
                                                  minimum.insecticide.resistance.heritability = 1, 
                                                  maximum.insecticide.resistance.heritability = 1,
                                                  minimum.male.insecticide.exposure = 1,
                                                  maximum.male.insecticide.exposure = 1, 
                                                  minimum.female.insecticide.exposure = 0, 
                                                  maximum.female.insecticide.exposure = 0), 0)
})
