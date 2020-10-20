temp.array = run_simulation_intervention(number.of.insecticides = 1,
                                      exposure.scaling.factor = 10,
                                      nsim = 1,
                                      minimum.insecticide.resistance.hertitability = 0.3,
                                      maximum.insecticide.resistance.hertitability = 0.3,
                                      minimum.male.insecticide.exposure = 1,
                                      maximum.male.insecticide.exposure = 1,
                                      minimum.female.insecticide.exposure = 1,
                                      maximum.female.insecticide.exposure = 1,
                                      resistance.cost = 0,
                                      starting.treatment.site.intensity = 0,
                                      starting.refugia.intensity = 0,
                                      min.intervention.coverage = 1,
                                      max.intervention.coverage = 1,
                                      min.dispersal.rate = 0.1,
                                      max.dispersal.rate = 0.9,
                                      maximum.generations = 100,
                                      irm.strategy = "rotation",
                                      half.population.bioassay.survival.resistance = 900,
                                      withdrawal.threshold.value = 0.1, 
                                      return.threshold.value = 0.05,
                                      deployment.frequency = 10,
                                      maximum.resistance.value = 25000)

temp.df = get_simulation_dataframe(simulation.array = temp.array,
                                   maximum.generations = 100,
                                   number.of.insecticides = 1)

test_that("only collects while insecticides are deployed", {
  expect_equal(nrow(temp.df), 20)
})
