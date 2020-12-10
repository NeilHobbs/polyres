
temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0)

coverage.vector = seq(from = 0,
                      to = 1,
                      by = 0.1)

for(i in 1:11){
  
  test_that("No difference when cross selection is zero, coverage", {
    expect_equal(insecticide_deployed_migration(exposure.scaling.factor = 10,
                                                nsim = 1000, 
                                                minimum.insecticide.resistance.heritability = 0.30,
                                                maximum.insecticide.resistance.heritability = 0.30,
                                                minimum.male.insecticide.exposure = 0.4,
                                                maximum.male.insecticide.exposure = 0.4, 
                                                minimum.female.insecticide.exposure = 0.7, 
                                                maximum.female.insecticide.exposure = 0.7,
                                                resistance.cost = 0,
                                                initial.resistance.intensity = 30,
                                                min.intervention.coverage = coverage.vector[i], 
                                                max.intervention.coverage = coverage.vector[i], 
                                                initial.refugia.resistance = 10,
                                                min.dispersal.rate = 0.5,
                                                max.dispersal.rate = 0.5),
                 insecticide_deployed_migration_cross_resistance(resistance.cost = 0,
                                                                 exposure.scaling.factor = 10,
                                                                 nsim = 1000, 
                                                                 minimum.insecticide.resistance.heritability = 0.30, 
                                                                 maximum.insecticide.resistance.heritability = 0.30,
                                                                 minimum.male.insecticide.exposure = 0.4,
                                                                 maximum.male.insecticide.exposure = 0.4, 
                                                                 minimum.female.insecticide.exposure = 0.7, 
                                                                 maximum.female.insecticide.exposure = 0.7,
                                                                 min.dispersal.rate = 0.5,
                                                                 max.dispersal.rate = 0.5,
                                                                 min.intervention.coverage = coverage.vector[i], 
                                                                 max.intervention.coverage = coverage.vector[i],
                                                                 number.of.insecticides = 3,
                                                                 cross.selection.matrix = temp.matrix,
                                                                 initial.resistance.intensity = 30,
                                                                 initial.refugia.resistance = 10,
                                                                 currently.deployed.insecticide = 1))
  })  
}


dispersal.rate.vector = seq(from = 0,
                            to = 1,
                            by = 0.1)

for(i in 1:11){
  
  test_that("No difference when cross selection is zero, dispersal", {
    expect_equal(insecticide_deployed_migration(exposure.scaling.factor = 10,
                                                nsim = 1000, 
                                                minimum.insecticide.resistance.heritability = 0.30,
                                                maximum.insecticide.resistance.heritability = 0.30,
                                                minimum.male.insecticide.exposure = 0.4,
                                                maximum.male.insecticide.exposure = 0.4, 
                                                minimum.female.insecticide.exposure = 0.7, 
                                                maximum.female.insecticide.exposure = 0.7,
                                                resistance.cost = 0,
                                                initial.resistance.intensity = 30,
                                                min.intervention.coverage = 0.5, 
                                                max.intervention.coverage = 0.5, 
                                                initial.refugia.resistance = 10,
                                                min.dispersal.rate = dispersal.rate.vector[i],
                                                max.dispersal.rate = dispersal.rate.vector[i]),
                 insecticide_deployed_migration_cross_resistance(resistance.cost = 0,
                                                                 exposure.scaling.factor = 10,
                                                                 nsim = 1000, 
                                                                 minimum.insecticide.resistance.heritability = 0.30, 
                                                                 maximum.insecticide.resistance.heritability = 0.30,
                                                                 minimum.male.insecticide.exposure = 0.4,
                                                                 maximum.male.insecticide.exposure = 0.4, 
                                                                 minimum.female.insecticide.exposure = 0.7, 
                                                                 maximum.female.insecticide.exposure = 0.7,
                                                                 min.dispersal.rate = dispersal.rate.vector[i],
                                                                 max.dispersal.rate = dispersal.rate.vector[i],
                                                                 min.intervention.coverage = 0.5, 
                                                                 max.intervention.coverage = 0.5,
                                                                 number.of.insecticides = 3,
                                                                 cross.selection.matrix = temp.matrix,
                                                                 initial.resistance.intensity = 30,
                                                                 initial.refugia.resistance = 10,
                                                                 currently.deployed.insecticide = 1))
  })  
}




