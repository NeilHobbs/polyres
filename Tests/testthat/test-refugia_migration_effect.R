test.values = c(0, 50, 100, 1000)

for(i in 1:length(test.values)){
test_that("there is no dispersal", {
  expect_equal(refugia_migration_effect(
    initial.refugia.resistance = test.values[i],
    initial.resistance.intensity = 5000,
    resistance.cost = 0,
    exposure.scaling.factor = 10,
    nsim = 1, 
    minimum.insecticide.resistance.heritability = 0.05, 
    maximum.insecticide.resistance.heritability = 0.30,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9,
    min.intervention.coverage = 1, 
    max.intervention.coverage = 1, 
    min.dispersal.rate = 0,
    max.dispersal.rate = 0), test.values[i])
})}


