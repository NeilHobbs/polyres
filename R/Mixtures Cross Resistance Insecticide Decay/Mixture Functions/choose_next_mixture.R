choose_next_mixture = function(previous.mixture,
                               total.mixtures,
                               available.mixtures){
  
  candidate.mixture = (previous.mixture + 1) #move to the next mixture
  
  
  `%notin%` = Negate(`%in%`) #make an "is not in" operator.
  
  while(candidate.mixture %notin% available.mixtures){
    if(candidate.mixture > total.mixtures){candidate.mixture = min(available.mixtures)}#go back to the start of the sequence
    else(candidate.mixture = (candidate.mixture + 1))#try to go to the next available insecticide
  }
  
  return(candidate.mixture) #will return the ID of the candidate mixture 
  
}


