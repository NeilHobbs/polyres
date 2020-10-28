test_that("returns vector of correct length", {
  expect_equal(length(insecticide_not_deployed_selection_cost(
    initial.resistance.intensity = 100,
    resistance.cost = 0.2,
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.hertitability = 0.1, 
    maximum.insecticide.resistance.hertitability = 0.3,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9)), 1000)})


test_that("cannot return values less than zero",{
  expect_equal(insecticide_not_deployed_selection_cost(
    initial.resistance.intensity = -100, #of course there should never be a negative input
    resistance.cost = 0.2,
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.hertitability = 0.1, 
    maximum.insecticide.resistance.hertitability = 0.3,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9), rep(0, 1000))})


#Test for the correct error messages.

test.values = c(-0.1, 1.1)
#Heritability
for(i in 1:length(test.values)){
  test_that("min heritability in range",{
    expect_error(insecticide_not_deployed_selection_cost(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.hertitability = test.values[i], 
      maximum.insecticide.resistance.hertitability = 0.3,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9), "minimum.insecticide.resistance.hertitability must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max heritability in range",{
    expect_error(insecticide_not_deployed_selection_cost(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.hertitability = 0.1, 
      maximum.insecticide.resistance.hertitability = test.values[i],
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9), "maximum.insecticide.resistance.hertitability must be between 0 and 1")})}

test_that("heritability min > max",{
  expect_error(insecticide_not_deployed_selection_cost(
    initial.resistance.intensity = 10,
    resistance.cost = 0.2,
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.hertitability = 0.3, 
    maximum.insecticide.resistance.hertitability = 0.2,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9), "minimum.insecticide.resistance.hertitability is greater than maximum.insecticide.resistance.hertitability")})

#male.insecticide.exposure
for(i in 1:length(test.values)){
  test_that("min male exposure in range",{
    expect_error(insecticide_not_deployed_selection_cost(
      initial.resistance.intensity = 10,
      resistance.cost = 0.2,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.hertitability = 0.2, 
      maximum.insecticide.resistance.hertitability = 0.3,
      minimum.male.insecticide.exposure = test.values[i],
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9), "minimum.male.insecticide.exposure must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max male exposure in range",{
    expect_error(insecticide_not_deployed_selection_cost(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.hertitability = 0.1, 
      maximum.insecticide.resistance.hertitability = 0.2,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = test.values[i], 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9), "maximum.male.insecticide.exposure must be between 0 and 1")})}

test_that("male exposure min>max",{
  expect_error(insecticide_not_deployed_selection_cost(
    initial.resistance.intensity = 10, 
    resistance.cost = 0.2,
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.hertitability = 0.1, 
    maximum.insecticide.resistance.hertitability = 0.2,
    minimum.male.insecticide.exposure = 0.9,
    maximum.male.insecticide.exposure = 0.3, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9), "minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")})

#female.insecticide.exposure
for(i in 1:length(test.values)){
  test_that("min female exposure in range",{
    expect_error(insecticide_not_deployed_selection_cost(
      initial.resistance.intensity = 10,
      resistance.cost = 0.2,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.hertitability = 0.2, 
      maximum.insecticide.resistance.hertitability = 0.3,
      minimum.male.insecticide.exposure = 0.1,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = test.values[i], 
      maximum.female.insecticide.exposure = 0.9), "minimum.female.insecticide.exposure must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max female exposure in range",{
    expect_error(insecticide_not_deployed_selection_cost(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.hertitability = 0.1, 
      maximum.insecticide.resistance.hertitability = 0.2,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 0.9, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = test.values[i]), "maximum.female.insecticide.exposure must be between 0 and 1")})}

test_that("female exposure min>max",{
  expect_error(insecticide_not_deployed_selection_cost(
    initial.resistance.intensity = 10, 
    resistance.cost = 0.2,
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.hertitability = 0.1, 
    maximum.insecticide.resistance.hertitability = 0.2,
    minimum.male.insecticide.exposure = 0.2,
    maximum.male.insecticide.exposure = 0.3, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.2), "minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")})

