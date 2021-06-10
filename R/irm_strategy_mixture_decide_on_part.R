#' @title Use mixtures as a sequence
#' 
#' @description This function implements mixtures as a sequence. This means that the mixture will be continued to
#' be deployed until one or both of the insecticides in the mixture meets the withdrawal threshold. 
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

irm_strategy_mixture_decide_on_part = function(number.of.insecticides,
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
  list.available.withdrawn =  return_and_withdrawal_of_insecticides(number.of.insecticides,
                                                                    current.generation,
                                                                    withdrawal.threshold,
                                                                    return.threshold,
                                                                    simulation.array,
                                                                    available.vector,
                                                                    withdrawn.vector)
  
  #get insecticides as vectors
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]
  
  #Return insecticide 1 back to the depoyment vector
  available.to.deploy = c(1, available.to.deploy)
  
  #Step 2: Find which mixtures are available:
  available.mixtures.df = find_available_mixtures(mixture.df = mixture.df,
                                                  withdrawn.insecticides = unavailable.to.deploy)
  
  #break if no mixtures are available for deployment
  if(nrow(available.mixtures.df)==0){deployment.df.updated = deploy_mixture(candidate.mixture.id = NA,
                                                                            mixture.df = mixture.df,
                                                                            deployment.df = deployment.df,
                                                                            deployment.frequency = 1)}
  else{
    
    #if the previous insecticide is still available deploy
    if(current.mixture %in% available.mixtures.df$mixture.id){deployment.df.updated = deploy_mixture(candidate.mixture.id = current.mixture,
                                                                                                     mixture.df = mixture.df,
                                                                                                     deployment.df = deployment.df,
                                                                                                     deployment.frequency = deployment.frequency)}
    #otherwise move on to the next insecticide
    else{deployment.df.updated = deploy_mixture(candidate.mixture.id = choose_next_mixture(previous.mixture = current.mixture,
                                                                                           total.mixtures = nrow(mixture.df),
                                                                                           available.mixtures = available.mixtures.df),
                                                mixture.df = mixture.df,
                                                deployment.df = deployment.df,
                                                deployment.frequency = deployment.frequency
                                                
    )}
  }
  return(list(available.mixtures.df, available.vector, withdrawn.vector, deployment.df.updated)) 
}

# sim.array = create_starting_array(n.insecticides = 3,
#                                   maximum.generations = 200)
# 
# sim.array["treatment", 1, 100] = 200
# sim.array["treatment", 2, 100] = 200
# sim.array["treatment", 3, 100] = 20
# available.vector = c(1,3)
# withdrawn.vector = c(2)
# deployed.mixture = rep(1, times = 100)
# 
# 
# irm_strategy_sequence_mixture(number.of.insecticides = 3,
#                       current.generation = 100,
#                       withdrawal.threshold = 100,
#                       return.threshold = 50,
#                       simulation.array = sim.array,
#                       available.vector = available.vector,
#                       withdrawn.vector = withdrawn.vector,
#                       current.insecticide = 1,
#                       deployment.frequency = 10,
#                       deployment.vector = deployed.insecticide,
#                       mixture.df,
#                       current.mixture)

