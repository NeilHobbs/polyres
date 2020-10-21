#'@title Implement the deployment of the sequence insecticide resistance management strategy.
#'
#' @description Implementation of the sequence IRM strategy allows for an insecticide to be continously used
#' until that insecticide reaches the threshold for withdrawal. After this the next insecticide is deployed. 
#' This function returns a list of the updated vectors of the available.vector[[1]], withdrawn.vector[[2]]
#' and the deployed.vector[[3]]

#' @param number.of.insecticides = The total number of insecticides included in the simulation.
#' @param current.generation = The generation where the simulation is up to.
#' @param withdrawal.threshold = The resistance intensity that leads to an insecticide being withdrawn
#' @param return.threshold = The resistance intensity that allows an insecticide to be returned to deployment
#' @param simulation.array = The array which holds the simulation results
#' @param available.vector = A vector containing the insecticides which can be used for deployment
#' @param withdrawn.vector = A vector the insecticides which are withdrawn from deployment
#' @param current.insecticide = The insecticide that is currently in deployment
#' @param deployment.frequency = The number of mosquito generations between each insecticide deployment check
#' @param deployment.vector = A vector containing the sequence of insecticides that have been deployed.
#' 
#' @return list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated)

irm_strategy_sequence = function(number.of.insecticides,
                                 current.generation,
                                 withdrawal.threshold,
                                 return.threshold,
                                 simulation.array,
                                 available.vector,
                                 withdrawn.vector,
                                 current.insecticide,
                                 deployment.frequency,
                                 deployment.vector){

  #Step 1: confirm which insecticides are/are not available for deployment
   list.available.withdrawn =  return_and_withdrawal_of_insecticides(number.of.insecticides,
                                                                   current.generation,
                                                                   withdrawal.threshold,
                                                                   return.threshold,
                                                                   simulation.array,
                                                                   available.vector,
                                                                   withdrawn.vector)
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]

  #break if no insecticides are available for deployment
  if(length(available.to.deploy)==0){deployment.vector.updated = deploy_insecticide(insecticide.to.deploy = NA,
                                                                                     deployment.frequency = 1,
                                                                                    deployment.vector = deployment.vector)}
  else{

  #if the previous insecticide is still available deploy
   if(current.insecticide %in% available.to.deploy){deployment.vector.updated = deploy_insecticide(insecticide.to.deploy = current.insecticide,
                                                                       deployment.frequency = deployment.frequency,
                                                                       deployment.vector = deployment.vector)}
    #otherwise move on to the next insecticide
  else{deployment.vector.updated = deploy_insecticide(insecticide.to.deploy = choose_next_insecticide(
                                                          previous.insecticide = current.insecticide,
                                                          available.insecticides = available.to.deploy,
                                                          number.of.insecticides = number.of.insecticides),
                           deployment.frequency = deployment.frequency,
                           deployment.vector = deployment.vector)}
  }
  return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated))
}
#' 
#' sim.array = create_starting_array(n.insecticides = 3,
#'                                   maximum.generations = 200)
#' 
#' sim.array["treatment", 1, 100] = 200
#' sim.array["treatment", 2, 100] = 200
#' sim.array["treatment", 3, 100] = 20
#' available.vector = c(1,3)
#' withdrawn.vector = c(2)
#' deployed.insecticide = rep(1, times = 100)
#' 
#' 
#' irm_strategy_sequence(number.of.insecticides = 3,
#'                       current.generation = 100,
#'                       withdrawal.threshold = 100,
#'                       return.threshold = 50,
#'                       simulation.array = sim.array,
#'                       available.vector = available.vector,
#'                       withdrawn.vector = withdrawn.vector,
#'                       current.insecticide = 1,
#'                       deployment.frequency = 10,
#'                       deployment.vector = deployed.insecticide)
#' 
#' 
#' 
#' 
#' 
