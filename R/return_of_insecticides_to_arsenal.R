#'@title Returns the insecticides that have been withdrawn back to being available for deployment if they meet the return.threshold criteria
#'
#'@description This function returns all the insecticides that meet the return criteria back to being available to deploy. Returning an updated
#'list of two vectors: available.vector[[1]] which can be used for deployment. And withdrawn.vector[[2]] which cannot be used for deployment.
#'It is important to not that this function does not withdrawn the insecticides [This is achieved through the withdraw_insecticide_from_arsenal and
#'withdrawal_of_insecticides_from_arsenal pipeline, which should therefore be called too using the wrapper function return_and_withdrawal_of_insecticides]
#'
#' @param number.of.insecticides = the number of insecticides in the simulation
#' @param current.generation = the generation in the simulation
#' @param return.threshold = the resistance intensity for return to the arsenal (this is the intensity score, not the proportion surviving)
#' @param simulation.array = the array that holds the simulation data
#' @param available.vector = a vector containing the insecticides available for deployment
#' @param withdraw.vector = a vector containing the insecticdes withdrawn from deployment

return_of_insecticides_to_arsenal = function(number.of.insecticides,
                               current.generation,
                               return.threshold,
                               simulation.array,
                               available.vector,
                               withdrawn.vector){

  #Make a vector that contains all the insecticides in the arsenal (both available and withdrawn).
  all.insecticides = seq(1, number.of.insecticides, by = 1)

  #Then for each of these insecticides, determine if that can be returned back to the deployable armory
  for(i in 1:length(all.insecticides)){

    A = if(check_insecticide_return(insecticide = all.insecticides[i],
                                    current.generation = current.generation,
                                    return.threshold = return.threshold,
                                    simulation.array = simulation.array)){
      return_insecticide_to_arsenal(withdrawn.vector = withdrawn.vector,
                                    available.vector = available.vector,
                                    insecticide.to.return = all.insecticides[i])
    } else {list(available.vector, withdrawn.vector)}

    available.vector = A[[1]] #The updated available armory.
    withdrawn.vector = A[[2]] #The withdrawn armory.
  }
  return(A) #A list:available.vector = A[[1]] ;  withdrawn.vector = A[[2]] 
}
