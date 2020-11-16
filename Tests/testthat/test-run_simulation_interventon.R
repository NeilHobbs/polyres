rot_1 = run_simulation_intervention(number.of.insecticides = 1,
                                exposure.scaling.factor = 10,
                                nsim = 1,
                                minimum.insecticide.resistance.heritability = 0.30,
                                maximum.insecticide.resistance.heritability = 0.30,
                                minimum.male.insecticide.exposure = 1,
                                maximum.male.insecticide.exposure = 1,
                                minimum.female.insecticide.exposure = 0.9,
                                maximum.female.insecticide.exposure = 0.9,
                                resistance.cost = 0,
                                starting.treatment.site.intensity = 0,
                                starting.refugia.intensity = 0,
                                min.intervention.coverage = 0.9,
                                max.intervention.coverage = 0.9,
                                min.dispersal.rate = 0.9,
                                max.dispersal.rate = 0.9,
                                maximum.generations = 20,
                                irm.strategy = "rotation", 
                                half.population.bioassay.survival.resistance = 900,
                                withdrawal.threshold.value = 0.1,
                                return.threshold.value = 0.05, 
                                deployment.frequency = 10, 
                                maximum.resistance.value = 25000)

test_that("rotations stop being tracked after a single deployment",{
  
  expect_equal(is.na(rot_1[[1]]["treatment", 1, 11]), TRUE)

})

test_that("rotations stop being deployed after single deployment",{
  
  expect_equal(is.na(rot_1[[2]][11]), TRUE)
})


#Test deployment.frequency
  #

rot_3 = run_simulation_intervention(number.of.insecticides = 3,
                                    exposure.scaling.factor = 10,
                                    nsim = 1,
                                    minimum.insecticide.resistance.heritability = 0.30,
                                    maximum.insecticide.resistance.heritability = 0.30,
                                    minimum.male.insecticide.exposure = 1,
                                    maximum.male.insecticide.exposure = 1,
                                    minimum.female.insecticide.exposure = 0.9,
                                    maximum.female.insecticide.exposure = 0.9,
                                    resistance.cost = 0,
                                    starting.treatment.site.intensity = 0,
                                    starting.refugia.intensity = 0,
                                    min.intervention.coverage = 0.9,
                                    max.intervention.coverage = 0.9,
                                    min.dispersal.rate = 0.9,
                                    max.dispersal.rate = 0.9,
                                    maximum.generations = 20,
                                    irm.strategy = "rotation", 
                                    half.population.bioassay.survival.resistance = 900,
                                    withdrawal.threshold.value = 0.1,
                                    return.threshold.value = 0.05, 
                                    deployment.frequency = 5, 
                                    maximum.resistance.value = 25000)

expected_insecticides = c(rep(1, 5), rep(2, 5), rep(3,5), rep(1, 5))

test_that("insecticides in rotation are changed each deployment",{
  
  expect_equal(c(rot_3[[2]]), expected_insecticides)
  
})


#No increase when certain parameter values are zeroes.
#No