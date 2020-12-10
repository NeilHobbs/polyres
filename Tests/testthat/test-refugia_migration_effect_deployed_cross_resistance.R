
temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0)

coverage.vector = seq(from = 0,
                      to = 1,
                      by = 0.1)

for(i in 1:11){
  
  test_that("No difference when cross selection is zero, coverage", {
    expect_equal(refugia_migration_effect_insecticide_deployed(initial.refugia.resistance = 10,
                                                                   initial.resistance.intensity = 30,
                                                                   resistance.cost = 0,
                                                                   exposure.scaling.factor = 10,
                                                                   nsim = 1, 
                                                                   minimum.insecticide.resistance.heritability = 0.30, 
                                                                   maximum.insecticide.resistance.heritability = 0.30,
                                                                   minimum.male.insecticide.exposure = 0.4,
                                                                   maximum.male.insecticide.exposure = 0.4, 
                                                                   minimum.female.insecticide.exposure = 0.7, 
                                                                   maximum.female.insecticide.exposure = 0.7,
                                                                   min.intervention.coverage = coverage.vector[i], 
                                                                   max.intervention.coverage = coverage.vector[i], 
                                                                   min.dispersal.rate = 0.5,
                                                                   max.dispersal.rate = 0.5),
                 refugia_migration_effect_deployed_cross_resistance(exposure.scaling.factor = 10,
                                                                        nsim = 1, 
                                                                        minimum.insecticide.resistance.heritability = 0.30, 
                                                                        maximum.insecticide.resistance.heritability = 0.30,
                                                                        minimum.male.insecticide.exposure = 0.4,
                                                                        maximum.male.insecticide.exposure = 0.4, 
                                                                        minimum.female.insecticide.exposure = 0.7, 
                                                                        maximum.female.insecticide.exposure = 0.7,
                                                                        min.intervention.coverage = coverage.vector[i], 
                                                                        max.intervention.coverage = coverage.vector[i], 
                                                                        min.dispersal.rate = 0.5,
                                                                        max.dispersal.rate = 0.5,
                                                                        resistance.cost = 0,
                                                                        initial.resistance.intensity = 30,
                                                                        cross.selection.matrix = temp.matrix,
                                                                        currently.deployed.insecticide = 2,
                                                                        currently.tracked.insecticide = 1,
                                                                        initial.refugia.resistance = 10,
                                                                        number.of.insecticides = 3))
  })  
}


dispersal.rate.vector = seq(from = 0,
                            to = 1,
                            by = 0.1)

for(i in 1:11){
  
  test_that("No difference when cross selection is zero, dispersal", {
    expect_equal(refugia_migration_effect_insecticide_deployed(exposure.scaling.factor = 10,
                                                                   nsim = 1, 
                                                                   minimum.insecticide.resistance.heritability = 0.30, 
                                                                   maximum.insecticide.resistance.heritability = 0.30,
                                                                   minimum.male.insecticide.exposure = 0.4,
                                                                   maximum.male.insecticide.exposure = 0.4, 
                                                                   minimum.female.insecticide.exposure = 0.7, 
                                                                   maximum.female.insecticide.exposure = 0.7,
                                                                   min.intervention.coverage = 0.5, 
                                                                   max.intervention.coverage = 0.5, 
                                                                   min.dispersal.rate = dispersal.rate.vector[i],
                                                                   max.dispersal.rate = dispersal.rate.vector[i],
                                                                   resistance.cost = 0,
                                                                   initial.resistance.intensity = 30,
                                                                   initial.refugia.resistance = 10),
                 refugia_migration_effect_deployed_cross_resistance(exposure.scaling.factor = 10,
                                                                        nsim = 1, 
                                                                        minimum.insecticide.resistance.heritability = 0.30, 
                                                                        maximum.insecticide.resistance.heritability = 0.30,
                                                                        minimum.male.insecticide.exposure = 0.4,
                                                                        maximum.male.insecticide.exposure = 0.4, 
                                                                        minimum.female.insecticide.exposure = 0.7, 
                                                                        maximum.female.insecticide.exposure = 0.7,
                                                                        min.intervention.coverage = 0.5, 
                                                                        max.intervention.coverage = 0.5, 
                                                                        min.dispersal.rate = dispersal.rate.vector[i],
                                                                        max.dispersal.rate = dispersal.rate.vector[i],
                                                                        resistance.cost = 0,
                                                                        initial.resistance.intensity = 30,
                                                                        cross.selection.matrix = temp.matrix,
                                                                        currently.deployed.insecticide = 2,
                                                                        currently.tracked.insecticide = 1,
                                                                        initial.refugia.resistance = 10,
                                                                        number.of.insecticides = 3))
  })  
}





