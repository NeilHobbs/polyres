#Function to implement withdrawal and return of insecticides. This returns a list of the the
# available[[1]] and withdrawn[[2]] insecticides.

#' @param number.of.insecticides = The total number of insecticides in the simulation
#' @param current.generation = The current generation in the simualation
#' @param withdrawal.threshold = The resistance intensity threshold for withdrawing an insecticide from the arsenal
#' @param return.threshold = The resistance intensity threshold for returning an insecticide to the arsenal
#' @param simulation.array = The array holding the simulation results
#' @param available.vector = A vector containing the previously available insecticides
#' @param withdrawn.vector = A vector containing the previously withdrawn insecticides


return_and_withdrawal_of_insecticides = function(number.of.insecticides,
                                                 current.generation,
                                                 withdrawal.threshold,
                                                 return.threshold,
                                                 simulation.array,
                                                 available.vector,
                                                 withdrawn.vector){
  #first do withdrawal:
  list.available.withdrawn.r1 = withdrawal_of_insecticides_from_arsenal(number.of.insecticides = number.of.insecticides,
                                                                        current.generation = current.generation,
                                                                        withdrawal.threshold = withdrawal.threshold,
                                                                        simulation.array = simulation.array,
                                                                        available.vector = available.vector,
                                                                        withdrawn.vector = withdrawn.vector)
  
  #Convert the lists back to the vectors
  available.vector.r1 = list.available.withdrawn.r1[[1]]
  withdrawn.vector.r1 = list.available.withdrawn.r1[[2]]
  
  #second do return:
  list.available.withdrawn.r2 = return_of_insecticides_to_arsenal(number.of.insecticides = number.of.insecticides,
                                                                  current.generation = current.generation,
                                                                  return.threshold = return.threshold,
                                                                  simulation.array = simulation.array,
                                                                  available.vector = available.vector.r1,
                                                                  withdrawn.vector = withdrawn.vector.r1)
  
  return(list.available.withdrawn.r2)
}