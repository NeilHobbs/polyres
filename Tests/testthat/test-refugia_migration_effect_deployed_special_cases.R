sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 10)

sim.array["treatment", 1, 4] = 0
sim.array["refugia", 1, 4] = 100
sim.array["treatment", 2, 4] = 0

test_that("no migration, no fitness costs", {
  expect_equal(refugia_migration_effect_deployed_special_cases(exposure.scaling.factor = 10,
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
                                                               currently.deployed.insecticide = 1,
                                                               currently.tracked.insecticide  = 1,
                                                               initial.refugia.resistance = 100,
                                                               number.of.insecticides = 2,
                                                               min.intervention.coverage = 1, 
                                                               max.intervention.coverage = 1, 
                                                               min.dispersal.rate = 0,
                                                               max.dispersal.rate = 0,
                                                               intercept = 0,
                                                               conversion.factor = 0,
                                                               current.insecticide.efficacy = 1,
                                                               sim.array = sim.array,
                                                               current.generation = 5,
                                                               half.population.bioassay.survival.resistance = 900,
                                                               insecticide.suppression = TRUE), 100)
})

test_that("complete exchange", {
  expect_equal(refugia_migration_effect_deployed_special_cases(exposure.scaling.factor = 10,
                                                               nsim = 1, 
                                                               minimum.insecticide.resistance.heritability = 1, 
                                                               maximum.insecticide.resistance.heritability = 1,
                                                               minimum.male.insecticide.exposure = 1,
                                                               maximum.male.insecticide.exposure = 1, 
                                                               minimum.female.insecticide.exposure = 1, 
                                                               maximum.female.insecticide.exposure = 1,
                                                               resistance.cost = 0,
                                                               initial.resistance.intensity = 50,
                                                               cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                    min.cross.selection = 0,
                                                                                                                    max.cross.selection = 0),
                                                               currently.deployed.insecticide = 1,
                                                               currently.tracked.insecticide  = 1,
                                                               initial.refugia.resistance = 0,
                                                               number.of.insecticides = 2,
                                                               min.intervention.coverage = 1, 
                                                               max.intervention.coverage = 1, 
                                                               min.dispersal.rate = 1,
                                                               max.dispersal.rate = 1,
                                                               intercept = 0,
                                                               conversion.factor = 0,
                                                               current.insecticide.efficacy = 0,
                                                               sim.array = sim.array,
                                                               current.generation = 5,
                                                               half.population.bioassay.survival.resistance = 900,
                                                               insecticide.suppression = TRUE), 50)
})


test_that("full efficacy same as original function", {
  expect_equal(refugia_migration_effect_deployed_special_cases(exposure.scaling.factor = 10,
                                                               nsim = 1, 
                                                               minimum.insecticide.resistance.heritability = 0.2, 
                                                               maximum.insecticide.resistance.heritability = 0.2,
                                                               minimum.male.insecticide.exposure = 0.5,
                                                               maximum.male.insecticide.exposure = 0.5, 
                                                               minimum.female.insecticide.exposure = 0.5, 
                                                               maximum.female.insecticide.exposure = 0.5,
                                                               resistance.cost = 0.1,
                                                               initial.resistance.intensity = 50,
                                                               cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                    min.cross.selection = 0,
                                                                                                                    max.cross.selection = 0),
                                                               currently.deployed.insecticide = 1,
                                                               currently.tracked.insecticide  = 1,
                                                               initial.refugia.resistance = 0,
                                                               number.of.insecticides = 2,
                                                               min.intervention.coverage = 0.5, 
                                                               max.intervention.coverage = 0.5, 
                                                               min.dispersal.rate = 0.5,
                                                               max.dispersal.rate = 0.5,
                                                               intercept = 1,
                                                               conversion.factor = 0,
                                                               current.insecticide.efficacy = 0,
                                                               sim.array = sim.array,
                                                               current.generation = 5,
                                                               half.population.bioassay.survival.resistance = 900,
                                                               insecticide.suppression = FALSE), 
               refugia_migration_effect_insecticide_not_deployed(initial.refugia.resistance = 0,
                                                                 initial.resistance.intensity = 50,
                                                                 resistance.cost = 0.1,
                                                                 exposure.scaling.factor = 10,
                                                                 nsim = 1, 
                                                                 minimum.insecticide.resistance.heritability = 0.2, 
                                                                 maximum.insecticide.resistance.heritability = 0.2,
                                                                 minimum.male.insecticide.exposure = 0.5,
                                                                 maximum.male.insecticide.exposure = 0.5, 
                                                                 minimum.female.insecticide.exposure = 0.5, 
                                                                 maximum.female.insecticide.exposure = 0.5,
                                                                 min.intervention.coverage = 0.5, 
                                                                 max.intervention.coverage = 0.5, 
                                                                 min.dispersal.rate = 0.5,
                                                                 max.dispersal.rate = 0.5))
})









