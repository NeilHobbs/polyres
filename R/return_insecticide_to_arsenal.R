#' @title Transfer a single insecticide from being withdrawn to being available
#' 
#'@description A function that returns an insecticide back to the arsenal having been withdrawb. 
#' This function updates the withdrawn and available insecticide vectors for the single insecticide. This is all passed in
#' the simulations to the return_and_withdrawal_of_insecticides function to perform this task on all the insecicides in the
#' simulation.Returns a list: [[1]]available vector and [[2]]withdrawn vector

#' @param withdrawn.vector = A vector containing the insecticides with have been withdrawn from the arsenal.
#' @param available.vector = A vector containing the insecticides which are available for deployment.
#' @param insecticide.to.return = The value of the insecticide to be returned to the arsenal.
#' 
#' @return list(available.vector, withdrawn.vector) : [[1]]available vector and [[2]]withdrawn vector


return_insecticide_to_arsenal = function(withdrawn.vector,
                                         available.vector,
                                         insecticide.to.return){

  withdrawn.vector = withdrawn.vector[!withdrawn.vector %in% insecticide.to.return]
  available.vector = c(available.vector, insecticide.to.return)

  return(list(available.vector, withdrawn.vector)) #[[1]]available vector ; [[2]]withdrawn vector
}
