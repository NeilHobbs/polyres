test_that("multiplication works", {
  expect_equal(length(insecticide_not_deployed_migration(
                         initial.resistance.intensity = 100,
                         resistance.cost = 0.2,
                         initial.refugia.resistance = 100,
                         exposure.scaling.factor = 10,
                         nsim = 1000, 
                         minimum.insecticide.resistance.heritability = 0.1, 
                         maximum.insecticide.resistance.heritability = 0.3,
                         minimum.male.insecticide.exposure = 0,
                         maximum.male.insecticide.exposure = 1, 
                         minimum.female.insecticide.exposure = 0.4, 
                         maximum.female.insecticide.exposure = 0.9, 
                         min.intervention.coverage = 0.1, 
                         max.intervention.coverage = 0.9, 
                         min.dispersal.rate = 0.1,
                         max.dispersal.rate = 0.9)), 1000)})


test_that("cannot return values less than zero",{
  expect_equal(insecticide_not_deployed_migration(
    initial.resistance.intensity = -100, #of course there should never be a negative input
    resistance.cost = 0.2,
    initial.refugia.resistance = -100, #of course there should never be a negative input
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.1, 
    maximum.insecticide.resistance.heritability = 0.3,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9, 
    min.intervention.coverage = 0.1, 
    max.intervention.coverage = 0.9, 
    min.dispersal.rate = 0.1,
    max.dispersal.rate = 0.9), rep(0, 1000))})


#Test for the correct error messages.

test.values = c(-0.1, 1.1)
  #Heritability
for(i in 1:length(test.values)){
test_that("min heritability in range",{
  expect_error(insecticide_not_deployed_migration(
    initial.resistance.intensity = 10, 
    resistance.cost = 0.2,
    initial.refugia.resistance = 10, 
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = test.values[i], 
    maximum.insecticide.resistance.heritability = 0.3,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9, 
    min.intervention.coverage = 0.1, 
    max.intervention.coverage = 0.9, 
    min.dispersal.rate = 0.1,
    max.dispersal.rate = 0.9), "minimum.insecticide.resistance.heritability must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max heritability in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      initial.refugia.resistance = 10, 
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = test.values[i],
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "maximum.insecticide.resistance.heritability must be between 0 and 1")})}

test_that("heritability min > max",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10,
      resistance.cost = 0.2,
      initial.refugia.resistance = 10, 
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.3, 
      maximum.insecticide.resistance.heritability = 0.2,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "minimum.insecticide.resistance.heritability is greater than maximum.insecticide.resistance.heritability")})

  #male.insecticide.exposure
for(i in 1:length(test.values)){
  test_that("min male exposure in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10,
      resistance.cost = 0.2,
      initial.refugia.resistance = 10,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.2, 
      maximum.insecticide.resistance.heritability = 0.3,
      minimum.male.insecticide.exposure = test.values[i],
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "minimum.male.insecticide.exposure must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max male exposure in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      initial.refugia.resistance = 10, 
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.2,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = test.values[i], 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.9, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "maximum.male.insecticide.exposure must be between 0 and 1")})}

test_that("male exposure min>max",{
  expect_error(insecticide_not_deployed_migration(
    initial.resistance.intensity = 10, 
    resistance.cost = 0.2,
    initial.refugia.resistance = 10, 
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.1, 
    maximum.insecticide.resistance.heritability = 0.2,
    minimum.male.insecticide.exposure = 0.9,
    maximum.male.insecticide.exposure = 0.3, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9, 
    min.intervention.coverage = 0.1, 
    max.intervention.coverage = 0.9, 
    min.dispersal.rate = 0.1,
    max.dispersal.rate = 0.9), "minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")})

  #female.insecticide.exposure
for(i in 1:length(test.values)){
  test_that("min female exposure in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10,
      resistance.cost = 0.2,
      initial.refugia.resistance = 10,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.2, 
      maximum.insecticide.resistance.heritability = 0.3,
      minimum.male.insecticide.exposure = 0.1,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = test.values[i], 
      maximum.female.insecticide.exposure = 0.9, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "minimum.female.insecticide.exposure must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max female exposure in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      initial.refugia.resistance = 10, 
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.2,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 0.9, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = test.values[i], 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "maximum.female.insecticide.exposure must be between 0 and 1")})}

test_that("female exposure min>max",{
  expect_error(insecticide_not_deployed_migration(
    initial.resistance.intensity = 10, 
    resistance.cost = 0.2,
    initial.refugia.resistance = 10, 
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.1, 
    maximum.insecticide.resistance.heritability = 0.2,
    minimum.male.insecticide.exposure = 0.2,
    maximum.male.insecticide.exposure = 0.3, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.2, 
    min.intervention.coverage = 0.1, 
    max.intervention.coverage = 0.9, 
    min.dispersal.rate = 0.1,
    max.dispersal.rate = 0.9), "minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")})
 
 #intervention.coverage
for(i in 1:length(test.values)){
  test_that("min intervention coverage in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10,
      resistance.cost = 0.2,
      initial.refugia.resistance = 10,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.2, 
      maximum.insecticide.resistance.heritability = 0.3,
      minimum.male.insecticide.exposure = 0.1,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.1, 
      maximum.female.insecticide.exposure = 0.9, 
      min.intervention.coverage = test.values[i], 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "min.intervention.coverage must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max intervention coverage in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      initial.refugia.resistance = 10, 
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.2,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 0.9, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.5, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = test.values[i], 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = 0.9), "max.intervention.coverage must be between 0 and 1")})}

test_that("intervention coverage min>max",{
  expect_error(insecticide_not_deployed_migration(
    initial.resistance.intensity = 10, 
    resistance.cost = 0.2,
    initial.refugia.resistance = 10, 
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.1, 
    maximum.insecticide.resistance.heritability = 0.2,
    minimum.male.insecticide.exposure = 0.2,
    maximum.male.insecticide.exposure = 0.3, 
    minimum.female.insecticide.exposure = 0.1, 
    maximum.female.insecticide.exposure = 0.2, 
    min.intervention.coverage = 0.9, 
    max.intervention.coverage = 0.2, 
    min.dispersal.rate = 0.1,
    max.dispersal.rate = 0.9), "min.intervention.coverage is greater than max.intervention.coverage")})

#dispersal.rate
for(i in 1:length(test.values)){
  test_that("min dispersal rate in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10,
      resistance.cost = 0.2,
      initial.refugia.resistance = 10,
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.2, 
      maximum.insecticide.resistance.heritability = 0.3,
      minimum.male.insecticide.exposure = 0.1,
      maximum.male.insecticide.exposure = 1, 
      minimum.female.insecticide.exposure = 0.1, 
      maximum.female.insecticide.exposure = 0.9, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = test.values[i],
      max.dispersal.rate = 0.9), "min.dispersal.rate must be between 0 and 1")})}

for(i in 1:length(test.values)){
  test_that("max dispersal rate in range",{
    expect_error(insecticide_not_deployed_migration(
      initial.resistance.intensity = 10, 
      resistance.cost = 0.2,
      initial.refugia.resistance = 10, 
      exposure.scaling.factor = 10,
      nsim = 1000, 
      minimum.insecticide.resistance.heritability = 0.1, 
      maximum.insecticide.resistance.heritability = 0.2,
      minimum.male.insecticide.exposure = 0,
      maximum.male.insecticide.exposure = 0.9, 
      minimum.female.insecticide.exposure = 0.4, 
      maximum.female.insecticide.exposure = 0.5, 
      min.intervention.coverage = 0.1, 
      max.intervention.coverage = 0.9, 
      min.dispersal.rate = 0.1,
      max.dispersal.rate = test.values[i]), "max.dispersal.rate must be between 0 and 1")})}

test_that("dispersal rate min>max",{
  expect_error(insecticide_not_deployed_migration(
    initial.resistance.intensity = 10, 
    resistance.cost = 0.2,
    initial.refugia.resistance = 10, 
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.heritability = 0.1, 
    maximum.insecticide.resistance.heritability = 0.2,
    minimum.male.insecticide.exposure = 0.2,
    maximum.male.insecticide.exposure = 0.3, 
    minimum.female.insecticide.exposure = 0.1, 
    maximum.female.insecticide.exposure = 0.2, 
    min.intervention.coverage = 0.1, 
    max.intervention.coverage = 0.2, 
    min.dispersal.rate = 0.8,
    max.dispersal.rate = 0.3), "min.dispersal.rate is greater than max.dispersal.rate")})