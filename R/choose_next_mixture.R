#' @title Choose the next mixture.
#' 
#' @description Choose the next available mixture for deployment.
#' 
#' @param previous.mixture = The mixture that was last deployed
#' @param total.mixtures = The total number of mixtures in the simulation
#' @param available.mixtures = The mixtures that are available for deployment (both parts of the mixture are in the armoury)

#With mixtures there is likely to be a scenario where we are using pyrethroids + another AI for all our mixtures. This
#is because all mixtures for public health currently in development are in a pyrethroid plus format.

#Therefore when modelling mixtures (especially with a "pyrethroid plus" mixture strategy), deployment decisions should not be made on the
#pyrethroid (as this is likely to be pre-set to be above the withdrawal threshold).


#mixture.part.2 is what decision is made on:

choose_next_mixture = function(previous.mixture,
                               total.mixtures,
                               available.mixtures){
  #This will choose the mixture ID
    candidate.mixture = (previous.mixture + 1) #move to the next mixture
  
  
  `%notin%` = Negate(`%in%`) #make an "is not in" operator.
  
  while(candidate.mixture %notin% available.mixtures){
    if(candidate.mixture > total.mixtures){candidate.mixture = min(available.mixtures)}#go back to the start of the sequence
    else(candidate.mixture = (candidate.mixture + 1))#try to go to the next available insecticide
  }
  
  return(candidate.mixture) #will return the ID of the candidate mixture 
  
}


