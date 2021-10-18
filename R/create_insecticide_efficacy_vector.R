#'@title Create a vector containing the efficacy of the insecticide
#'
#'@description Creates a vector of the efficacy of the insecticide for a single deployment interval.
#'
#'@param applied.insecticide.dose = The amount of insecticide applied
#'@param recommended.insecticide.dose = The label single insecticide dosage of the insecticide as recommended by the manufacturers.
#'@param threhshold.generations = The number of generations after which there is an expected rapid decline in insecticide efficacy
#'@param base.efficacy.decay.rate = The decay rate of the insecticide under standard conditions
#'@param rapid.decay.rate = The decay rate once the threshold.generations time has been exceeded/
#'@param deployment.frequency = The frequency at which insecticide deployments/applications occur.


create_insecticide_efficacy_vector = function(applied.insecticide.dose,
                                              recommended.insecticide.dose,
                                              threshold.generations,
                                              base.efficacy.decay.rate,
                                              rapid.decay.rate,
                                              deployment.frequency){

  start.efficacy = calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose,
                                                                             recommended.insecticide.dose)
  gens.vector = seq(1, deployment.frequency, by = 1)
  efficacy.vector = c()

  for(g in 1:deployment.frequency){


    efficacy.vector[g] = calculate_current_insecticide_efficacy(generations.since.deployment = gens.vector[g],
                                                              threshold.generations = threshold.generations,
                                                              initial.insecticide.efficacy = start.efficacy,
                                                              base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                              rapid.decay.rate = rapid.decay.rate)
  }
  efficacy.vector.updated = c(start.efficacy, efficacy.vector)

  return(efficacy.vector.updated[1:deployment.frequency])

}
