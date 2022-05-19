

decision_on_insecticide_1_only_standard = function(number.of.insecticides,
                                          current.generation,
                                          withdrawal.threshold,
                                          return.threshold,
                                          simulation.array,
                                          available.vector,
                                          withdrawn.vector,
                                          mixture.df,
                                          current.mixture,
                                          deployment.frequency,
                                          deployment.df){
  
  
  #Step 1: confirm which insecticides are/are not available for deployment [same as with single insecticide deployment]
  list.available.withdrawn =  return_and_withdrawal_of_insecticides(number.of.insecticides = number.of.insecticides,
                                                                    current.generation = current.generation,
                                                                    withdrawal.threshold = withdrawal.threshold,
                                                                    return.threshold = return.threshold,
                                                                    simulation.array = simulation.array,
                                                                    available.vector = available.vector,
                                                                    withdrawn.vector = withdrawn.vector)
  
  #note this section does not actually matter:::
  #get insecticides as vectors
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]
  
  available.mixtures.df = find_available_mixtures(mixture.df = mixture.df,
                                                  withdrawn.insecticides = unavailable.to.deploy)
  
  
  
  
  
  if(simulation.array["treatment", 1, current.generation] >= withdrawal.threshold){
    deployment.df.updated = deploy_mixture(candidate.mixture.id = NA,
                                                      mixture.df = mixture.df,
                                                      deployment.df = deployment.df,
                                                      deployment.frequency = 1)
    
  }
  
  if(simulation.array["treatment", 1, current.generation] < withdrawal.threshold){
    deployment.df.updated = deploy_mixture(candidate.mixture.id = 1,
                                                      mixture.df = mixture.df,
                                                      deployment.df = deployment.df,
                                                      deployment.frequency = deployment.frequency)
  }
  
  
  
  return(list(available.mixtures.df, available.vector, withdrawn.vector, deployment.df.updated))  
}


