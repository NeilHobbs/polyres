#'@title Create a dataframe to hold the insecticide parameters
#'
#'@description Insecticides have unique properties, this function allows the user to put in unique insecticidal and
#'dosaging parameters for each insecticide.
#'
#'@param number.of.insecticides = The number of insecticides in the simulation.
#'@param applied.insecticide.dose = The dosage the actual insecticide is deployed at.
#'@param recommended.insecticide.dose = The recommended manufacture's dosage for the insecticide.
#'@param threshold.generation = The threshold generation at which the insecticide begins to rapidly decay.
#'@param base.efficacy.decay.rate = The base efficacy decay rate when the insecticide is deployed.
#'@param rapid.decay.rate = The decay rate of an insecticide after it has exceeded its threshold.generation.

create_insecticide_parameters_dataframe = function(number.of.insecticides,
                                                   applied.insecticide.dose,
                                                   recommended.insecticide.dose,
                                                   threshold.generation,
                                                   base.efficacy.decay.rate,
                                                   rapid.decay.rate){


  if(length(applied.insecticide.dose) == number.of.insecticides){
    applied.insecticide.doses = applied.insecticide.dose}else{if(
      length(applied.insecticide.dose) == 1){
      applied.insecticide.doses = rep(applied.insecticide.dose, times=number.of.insecticides)}else{
        stop("applied.insecticide.dose must either be a vector with length equal to the number.of.insecticides or a single value")}}



  if(length(recommended.insecticide.dose) == number.of.insecticides){
    recommended.insecticide.doses = recommended.insecticide.dose}else{if(length(recommended.insecticide.dose) == 1){
      recommended.insecticide.doses = rep(recommended.insecticide.dose, times=number.of.insecticides)}else{
        stop("recommended.insecticide.dose must either be a vector with length equal to the number.of.insecticides or a single value")
      }}


  if(length(threshold.generation) == number.of.insecticides){
    threshold.generations = threshold.generation}else{if(length(threshold.generation) == 1){
      threshold.generations = rep(threshold.generation, times=number.of.insecticides)}else{
        stop("threshold.generation must either be a vector with length equal to the number.of.insecticides or a single value")}}


  if(length(base.efficacy.decay.rate) == number.of.insecticides){
    base.efficacy.decay.rates = base.efficacy.decay.rate}else{if(length(base.efficacy.decay.rate) == 1){
      base.efficacy.decay.rates = rep(base.efficacy.decay.rate, times=number.of.insecticides)}else{
        stop("base.efficacy.decay.rate must either be a vector with length equal to the number.of.insecticides or a single value")
      }}

  if(length(rapid.decay.rate) == number.of.insecticides){
    rapid.decay.rates = rapid.decay.rate}else{if(length(rapid.decay.rate) == 1){
      rapid.decay.rates = rep(rapid.decay.rate, times=number.of.insecticides)}else{
        stop("rapid.decay.rate must either be a vector with length equal to the number.of.insecticides or a single value")
      }}

  insecticides = seq(from = 1, to = number.of.insecticides, by = 1)


  insecticidal.parameters.dataframe = data.frame(insecticides,
                                                 applied.insecticide.doses,
                                                 recommended.insecticide.doses,
                                                 threshold.generations,
                                                 base.efficacy.decay.rates,
                                                 rapid.decay.rates)

  return(insecticidal.parameters.dataframe)
}

# create_insecticide_parameters_dataframe(number.of.insecticides = 4,
#                                         applied.insecticide.dose = c(1, 3, 4, 8),
#                                         recommended.insecticide.dose = 1,
#                                         threshold.generation = 1,
#                                         base.efficacy.decay.rate = 4,
#                                         rapid.decay.rate = 5)






