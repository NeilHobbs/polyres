cost.values = seq(0, 1, by = 0.1)
no.efficacy = c()
no.insecticide=c()

for(i in 1:11){
  no.efficacy[i]  =insecticide_deployed_special_cases(currently.deployed.insecticide = 1,
                                                    number.of.insecticides = 2,
                                                    cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                         min.cross.selection = 0,
                                                                                                         max.cross.selection = 0),
                                                    exposure.scaling.factor = 10,
                                                    nsim = 1, 
                                                    minimum.insecticide.resistance.heritability = 1, 
                                                    maximum.insecticide.resistance.heritability = 1,
                                                    minimum.male.insecticide.exposure = 1,
                                                    maximum.male.insecticide.exposure = 1, 
                                                    minimum.female.insecticide.exposure = 1, 
                                                    maximum.female.insecticide.exposure = 1,
                                                    resistance.cost = cost.values[i],
                                                    initial.resistance.intensity = 100,
                                                    current.insecticide.efficacy = 0)
  
 no.insecticide[i] = insecticide_not_deployed_selection_cost(initial.resistance.intensity = 100,
                                          resistance.cost = cost.values[i],
                                          exposure.scaling.factor = 10,
                                          nsim = 1, 
                                          minimum.insecticide.resistance.heritability = 1, 
                                          maximum.insecticide.resistance.heritability = 1,
                                          minimum.male.insecticide.exposure = 1,
                                          maximum.male.insecticide.exposure = 1, 
                                          minimum.female.insecticide.exposure = 1, 
                                          maximum.female.insecticide.exposure = 1)
}

test_that("when efficacy is 0, works as if insecticide not deployed", {
  expect_equal(no.efficacy, no.insecticide)
})
  

full.efficacy = c()
insecticide.present=c()

for(i in 1:11){
  full.efficacy[i]  =insecticide_deployed_special_cases(currently.deployed.insecticide = 1,
                                                      number.of.insecticides = 2,
                                                      cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                           min.cross.selection = 0,
                                                                                                           max.cross.selection = 0),
                                                      exposure.scaling.factor = 10,
                                                      nsim = 1, 
                                                      minimum.insecticide.resistance.heritability = 1, 
                                                      maximum.insecticide.resistance.heritability = 1,
                                                      minimum.male.insecticide.exposure = 1,
                                                      maximum.male.insecticide.exposure = 1, 
                                                      minimum.female.insecticide.exposure = 1, 
                                                      maximum.female.insecticide.exposure = 1,
                                                      resistance.cost = cost.values[i],
                                                      initial.resistance.intensity = 100,
                                                      current.insecticide.efficacy = 1)
  
  insecticide.present[i] = insecticide_deployed_selection_cost(initial.resistance.intensity = 100,
                                                              resistance.cost = cost.values[i],
                                                              exposure.scaling.factor = 10,
                                                              nsim = 1, 
                                                              minimum.insecticide.resistance.heritability = 1, 
                                                              maximum.insecticide.resistance.heritability = 1,
                                                              minimum.male.insecticide.exposure = 1,
                                                              maximum.male.insecticide.exposure = 1, 
                                                              minimum.female.insecticide.exposure = 1, 
                                                              maximum.female.insecticide.exposure = 1)
}



test_that("when efficacy is 1, works as if insecticide classically deployed", {
  expect_equal(full.efficacy, insecticide.present)
})









