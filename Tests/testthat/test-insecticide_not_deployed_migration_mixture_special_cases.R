test_that("FALSE == cross_selection function", {
  
  sim.array.1 = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 5)

cs.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
                                        min.cross.selection = 0,
                                        max.cross.selection = 0)

sim.array.1["refugia", 1, 4] = 10
sim.array.1["refugia", 2, 4] = 10
sim.array.1["treatment", 1, 4] = 10
sim.array.1["treatment", 2, 4] = 10
sim.array.1["refugia", 3, 4] = 8
sim.array.1["treatment", 3, 4] = 10

mixture.id = rep(1, 5)
mixture.part.1 = rep(1, 5)
mixture.part.2 = rep(2, 5)

deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)
  
  
  
  expect_equal(insecticide_not_deployed_migration_mixture_special_cases(nsim = 1,
                                                                        min.intervention.coverage = 0.7,
                                                                        max.intervention.coverage = 0.7,
                                                                        min.dispersal.rate = 0,
                                                                        max.dispersal.rate = 0,
                                                                        initial.refugia.resistance = 8,
                                                                        resistance.cost = 0,
                                                                        exposure.scaling.factor = 10,
                                                                        minimum.insecticide.resistance.heritability = 1, 
                                                                        maximum.insecticide.resistance.heritability = 1,
                                                                        minimum.male.insecticide.exposure = 1,
                                                                        maximum.male.insecticide.exposure = 1, 
                                                                        minimum.female.insecticide.exposure = 1, 
                                                                        maximum.female.insecticide.exposure = 1,
                                                                        initial.resistance.intensity = 10,
                                                                        cross.selection.matrix = cs.matrix,
                                                                        currently.tracked.insecticide = 3,
                                                                        conversion.factor = 0.48,
                                                                        intercept = 0.15,
                                                                        mixture.part.1 = 1,
                                                                        efficacy.mixture.part.1 = 1,
                                                                        mixture.part.2 = 2,
                                                                        efficacy.mixture.part.2 = 1,
                                                                        current.generation = 5,
                                                                        sim.array = sim.array.1,
                                                                        deployed.mixture = deployed.mixture,
                                                                        half.population.bioassay.survival.resistance = 900,
                                                                        insecticide.suppression = "FALSE"), 
               insecticide_not_deployed_migration_mixture_cross_selection(nsim = 1,
                                                                          min.intervention.coverage = 0.7,
                                                                          max.intervention.coverage = 0.7,
                                                                          min.dispersal.rate = 0,
                                                                          max.dispersal.rate = 0,
                                                                          initial.refugia.resistance = 8,
                                                                          resistance.cost = 0,
                                                                          exposure.scaling.factor = 10,
                                                                          minimum.insecticide.resistance.heritability = 1, 
                                                                          maximum.insecticide.resistance.heritability = 1,
                                                                          minimum.male.insecticide.exposure = 1,
                                                                          maximum.male.insecticide.exposure = 1, 
                                                                          minimum.female.insecticide.exposure = 1, 
                                                                          maximum.female.insecticide.exposure = 1,
                                                                          initial.resistance.intensity = 10,
                                                                          cross.selection.matrix = cs.matrix,
                                                                          deployed.mixture = deployed.mixture,
                                                                          sim.array = sim.array.1,
                                                                          deployed.mixture.1 = 1,
                                                                          deployed.mixture.2 = 2,
                                                                          currently.tracked.insecticide = 3,
                                                                          conversion.factor = 0.48,
                                                                          intercept = 0.15,
                                                                          generation = 5,
                                                                          half.population.bioassay.survival.resistance = 900))
})
