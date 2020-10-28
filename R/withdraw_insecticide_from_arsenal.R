#'@title Remove an insecticide from the arsenal. This function updates the withdrawn and available insecticide
#' vectors. Returns a list: [[1]]available vector and [[2]]withdrawn vector.

#' @param withdrawn.vector = A vector containing the insecticides with have been withdrawn from the arsenal.
#' @param available.vector = A vector containing the insecticides which are available for deployment.
#' @param insecticide.to.withdraw = The value of the insecticide to be withdrawn


withdraw_insecticide_from_arsenal = function(withdrawn.vector,
                                             available.vector,
                                             insecticide.to.withdraw){

  available.vector.updated = available.vector[!available.vector %in% insecticide.to.withdraw]
  withdrawn.vector.updated = c(withdrawn.vector, insecticide.to.withdraw)

  return(list(available.vector.updated, withdrawn.vector.updated)) #[[1]]available vector ; [[2]]withdrawn vector

}
