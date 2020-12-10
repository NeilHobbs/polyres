

irm_strategy_rotation_mixture = function(number.of.insecticides,
                                         current.generation,
                                         withdrawal.threshold,
                                         return.threshold,
                                         simulation.array,
                                         available.vector,
                                         withdrawn.vector,
                                         mixture.df,
                                         current.mixture){
  
  
  #Step 1: confirm which insecticides are/are not available for deployment [same as with single insecticide deployment]
  list.available.withdrawn =  return_and_withdrawal_of_insecticides(number.of.insecticides,
                                                                    current.generation,
                                                                    withdrawal.threshold,
                                                                    return.threshold,
                                                                    simulation.array,
                                                                    available.vector,
                                                                    withdrawn.vector,
                                                                    mixture.df,
                                                                    current.mixture)
  
  #get insecticides as vectors
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]
  
  #Step 2: Find which mixtures are available:
  
  available.mixtures.df = find_available_mixtures(mixture.df = mixture.df,
                                                  withdrawn.insecticides = unavailable.to.deploy)
  
  #break if no insecticides are available for deployment
  if(nrow(available.mixtures.df)==0){deployment.df.updated = deploy_mixture(candidate.mixture.id = NA,
                                                                                mixture.df = mixture.df,
                                                                                deployment.df = deployment.df,
                                                                                freq.deployment = 1)}
  else{
    candidate.mixture = choose_next_mixture(previous.mixture = current.mixture,
                                                total.mixtures = nrow(mixture.df),
                                                available.mixtures = available.mixtures.df)}
  
  #if the candidate insecticide is the currently deployed insecticide set deployed to NA to stop the simulation
  if(candidate.mixture == current.mixture){
    deployment.df.updated = deploy_mixture(candidate.mixture.id = NA,
                                           mixture.df = mixture.df,
                                           deployment.df = deployment.df,
                                           freq.deployment = 1)}
  #otherwise the candidate insecticide can be deployed.
  else{deployment.df.updated = deploy_mixture(candidate.mixture.id = candidate.mixture,
                                              mixture.df = mixture.df,
                                              deployment.df = deployment.df,
                                              freq.deployment = freq.deployment)}

return(list(available.mixtures.df, available.vector, withdrawn.vector, deployment.df.updated))  
}
  
  
  