#This function returns the insecticides that have been withdrawn back to being available for deployment if they meet the return.threshold criteria

#' @param number.of.insecticides = the number of insecticides in the simulation
#' @param current.generation = the generation in the simulation
#' @param return.threshold = the resistance intensity for return to the arsenal
#' @param simulation.array = the array that holds the simulation data
#' @param available.vector = a vector containing the insecticides available for deployment
#' @param withdraw.vector = a vector containing the insecticdes withdrawn from deployment



return_of_insecticides_to_arsenal = function(number.of.insecticides,
                               current.generation,
                               return.threshold,
                               simulation.array,
                               available.vector,
                               withdrawn.vector){
  
  all.insecticides = seq(1, number.of.insecticides, by = 1)
  
  for(i in 1:length(all.insecticides)){
    
    A = if(check_insecticide_return(insecticide = all.insecticides[i],
                                    current.generation = current.generation,
                                    return.threshold = return.threshold,
                                    simulation.array = simulation.array)){
      return_insecticide_to_arsenal(withdrawn.vector = withdrawn.vector,
                                    available.vector = available.vector,
                                    insecticide.to.return = all.insecticides[i])
    } else {list(available.vector, withdrawn.vector)}
    
    available.vector = A[[1]]
    withdrawn.vector = A[[2]]
  }
  return(A)
}
