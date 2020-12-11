#' @title Use mixtures with rotations
#' 
#' @description This function implements mixtures as a rotation, meaning that the same mixture cannot be deployed 
#' in the next deployment. It should be noted that rotations with mixtures does all for one of the insecticides to
#' be deployed again; providing the second insecticide in the mixture is different. For example a mixture containing
#' insecticides 1&2 could be rotated to a mixture containing either insecticide 1 or insecticide 2 again. 
#' 
#' @param number.of.insecticides = The total number of insecticides that are in the simulation
#' @param current.generation = The current generation in the simulation
#' @param withdrawal.threshold = The resistance intensity that gives the prerequisit bioassay survival for withdrawal.
#' @param return.threshold = The resistance intensity that gives the prerequistit bioassay survival for insecticide return
#' @param simulation.array = The simulation array that holds the simulation data
#' @param available.vector = A vector containing the previously available insecticides
#' @param withdrawn.vector = A vector containing the previously withdrawn insecticides
#' @param mixture.df = A dataframe containing the information on all the insecticidal mixtures that are in the simulation.
#' @param current.mixture = The mixture.id of the last mixture deployed.
#' @param deployment.frequency = The frequency at which insecticide deployment decisions are made.

irm_strategy_rotation_mixture = function(number.of.insecticides,
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
  
  #get insecticides as vectors
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]
  
  #Step 2: Find which mixtures are available:
  
  available.mixtures.df = find_available_mixtures(mixture.df = mixture.df,
                                                  withdrawn.insecticides = unavailable.to.deploy)
  
  #break if no mixtures are available for deployment
  if(nrow(available.mixtures.df)==0){deployment.df.updated = deploy_mixture(candidate.mixture.id = NA,
                                                                                mixture.df = mixture.df,
                                                                                deployment.df = deployment.df,
                                                                            deployment.frequency = 1)}
  else{
    candidate.mixture = choose_next_mixture(previous.mixture = current.mixture,
                                                total.mixtures = nrow(mixture.df),
                                                available.mixtures = available.mixtures.df)}
  
  #if the candidate insecticide is the currently deployed insecticide set deployed to NA to stop the simulation
  if(candidate.mixture == current.mixture){
    deployment.df.updated = deploy_mixture(candidate.mixture.id = NA,
                                           mixture.df = mixture.df,
                                           deployment.df = deployment.df,
                                           deployment.frequency = 1)}
  #otherwise the candidate insecticide can be deployed.
  else{deployment.df.updated = deploy_mixture(candidate.mixture.id = candidate.mixture,
                                              mixture.df = mixture.df,
                                              deployment.df = deployment.df,
                                              deployment.frequency = deployment.frequency)}

return(list(available.mixtures.df, available.vector, withdrawn.vector, deployment.df.updated))  
}
  
  
  