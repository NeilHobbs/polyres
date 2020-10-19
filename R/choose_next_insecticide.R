#'@title Choose the next insecticide for deployment.
#'
#'@description
#'The function selects the next available insecticide for deployment. It does not choose based on which would be the optimal 
#'insecticide (eg. the insecticide with the lowest resistance intensity), but which is next based on the numerical sequence of
#'the insecticides.
#'
#' @param  previous.insecticide = The insecticide that was last deployed.
#' @param  available.insecticides = A vector containing the available insecticides.
#' @param number.of.insecticides = The total number of insecticides (available and withdrawn) that are in the simulation.
#' 
#' @return candidate.insecticide = a single integer which represents the insecticide which is to be deployed.

choose_next_insecticide = function(previous.insecticide,
                                   available.insecticides, #Make sure this function is only called when there are insecticides available.
                                   number.of.insecticides){

  candidate.insecticide = (previous.insecticide + 1) #move to the next insecticide

  `%notin%` = Negate(`%in%`) #make an is not in operator.

  while(candidate.insecticide %notin% available.insecticides){
    if(candidate.insecticide > number.of.insecticides){candidate.insecticide = min(available.insecticides)}#go back to the start of the sequence
    else(candidate.insecticide = (candidate.insecticide + 1))#try to go to the next available insecticide
  }

  return(candidate.insecticide)

}

#It should be noted that providing the next insecticide in the sequence is available it will be the one selected,
  #even if its not the "best choice" insecticide, the insecticide with the lowest resistance intensity score. It would be 
  #worth considering having a system that chooses the "best choice" insecticide to ensure an optimal strategy is deployed,
    #but I think there is no need to do that until there is the ability to allow each insecticide to have different abilities,
      #as otherwise the insecticides should deploy in order anyway.