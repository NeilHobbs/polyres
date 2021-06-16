#'@title Function to get the insecticide information of insecticide deployed in the mixture but not currently being tracked.

get_other_insecticide_mixture = function(tracked.insecticide,
                                                    deployed.mixture,
                                                    generation){
  
  
  
  if(tracked.insecticide == deployed.mixture$mixture.part.1[generation]){
    insecticide.efficacy = deployed.mixture$mixture.part.2[generation] 
  }
  
  if(tracked.insecticide == deployed.mixture$mixture.part.2[generation]){
    insecticide.efficacy = deployed.mixture$mixture.part.1[generation]
  }
  
  return(insecticide.efficacy)
}



