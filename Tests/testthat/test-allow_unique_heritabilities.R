test_that("function works with 1 heritability, multiple insecticides", {
  expect_equal(allow_unique_heritabilities(minimum.insecticide.resistance.heritability = c(2),
                                           maximum.insecticide.resistance.heritability = c(2),
                                           number.of.insecticides = 7), rep(2, 7))
})


test_that("function works with 3 heritability, 3 insecticides", {
  expect_equal(allow_unique_heritabilities(minimum.insecticide.resistance.heritability = c(2, 5, 7),
                                           maximum.insecticide.resistance.heritability = c(2, 5, 7),
                                           number.of.insecticides = 3), c(2, 5, 7))
})


test_that("errors work", {
  expect_error(allow_unique_heritabilities(minimum.insecticide.resistance.heritability = c(2, 5),
                                           maximum.insecticide.resistance.heritability = c(2, 5, 7),
                                           number.of.insecticides = 3), "length of the insecticide.resistance.heritability vectors must be 1 or equal to the number of insecticides in the simulation and
               minimum.insecticide.resistance.heritability must equal maximum.insecticide.resistance.heritability")
  
  expect_error(allow_unique_heritabilities(minimum.insecticide.resistance.heritability = c(2, 5, 7),
                                           maximum.insecticide.resistance.heritability = c(2, 5),
                                           number.of.insecticides = 3), "length of the insecticide.resistance.heritability vectors must be 1 or equal to the number of insecticides in the simulation and
               minimum.insecticide.resistance.heritability must equal maximum.insecticide.resistance.heritability")
  
  expect_error(allow_unique_heritabilities(minimum.insecticide.resistance.heritability = c(2, 5, 7),
                                           maximum.insecticide.resistance.heritability = c(2, 5, 7),
                                           number.of.insecticides = 7), "length of the insecticide.resistance.heritability vectors must be 1 or equal to the number of insecticides in the simulation and
               minimum.insecticide.resistance.heritability must equal maximum.insecticide.resistance.heritability")
})
