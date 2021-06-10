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
