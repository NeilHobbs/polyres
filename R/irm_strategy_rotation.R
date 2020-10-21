#'@title Implement the rotation insectidicide resistance management strategy.
#'
#'@description The implementation of the rotation strategy means that a different insecticide must be deployed
#'for each deployment interval. For example if insecticide 1 was deployed in the previous deployment it could not
#'be deployed in the next deployment. A list of three updated vectors is returned:
#'available.vector[[1]], withdrawn.vector[[2]] and deployed.vector[[3]]
#'
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

irm_strategy_rotation = function(number.of.insecticides,
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
    candidate.insecticide = choose_next_insecticide(previous.insecticide = current.insecticide,
                                                    available.insecticides = available.to.deploy,
                                                    number.of.insecticides = number.of.insecticides)
    
    
    #if the candidate insecticide is the currently deployed insecticide set deployed to NA to stop the simulation
    if(candidate.insecticide == current.insecticide){
      deployment.vector.updated = deploy_insecticide(insecticide.to.deploy = NA,
                                                     deployment.frequency = 1,
                                                     deployment.vector = deployment.vector)}
    #otherwise the candidate insecticide can be deployed.
    else{deployment.vector.updated = deploy_insecticide(insecticide.to.deploy = candidate.insecticide,
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
