test.values = c(1, 50, 100, 1000)

for(i in 1:length(test.values)){
test_that("multiplication works", {
  expect_equal(refugia_selection_costs(initial.refugia.resistance = test.values[i],
                                       resistance.cost =0, 
                                       exposure.scaling.factor = 10,
                                       nsim = 1, 
                                       minimum.insecticide.resistance.heritability = 0.3, 
                                       maximum.insecticide.resistance.heritability = 0.3,
                                       minimum.male.insecticide.exposure = 0.9,
                                       maximum.male.insecticide.exposure = 0.9, 
                                       minimum.female.insecticide.exposure = 0.9, 
                                       maximum.female.insecticide.exposure = 0.9), test.values[i])
})}
