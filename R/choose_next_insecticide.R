#A function to choose the next insecticide for deployment.
#Important Note: It is possible for the candidate.insecticide to be the same as the previous.insecticide; which is not allowed for rotations.

#' @param  previous.insecticide = The insecticide that was last deployed.
#' @param  available.insecticides = A vector containing the available insecticides.
#' @param number.of.insecticides = The total number of insecticides (available and withdrawn) that are in the simulation.

choose_next_insecticide = function(previous.insecticide,
                                   available.insecticides,
                                   number.of.insecticides){

  candidate.insecticide = (previous.insecticide + 1) #move to the next insecticide

  `%notin%` = Negate(`%in%`) #make a is not in operator

  while(candidate.insecticide %notin% available.insecticides){
    if(candidate.insecticide > number.of.insecticides){candidate.insecticide = min(available.insecticides)}#go back to the start of the sequence
    else(candidate.insecticide = (candidate.insecticide + 1))#try to go to the next available insecticide
  }

  return(candidate.insecticide)

}