#'@title Function to get the insecticide information of insecticide deployed in the mixture but not currently being tracked.

get_other_mixture_part_decay_information = function(tracked.insecticide,
                                                    deployed.mixture,
                                                    generation){
  
  
  
    if(tracked.insecticide == deployed.mixture$mixture.part.1[generation]){
    insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation] #efficacy of part 2
  }
  
  if(tracked.insecticide == deployed.mixture$mixture.part.2[generation]){
    insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation]#efficacy of part 1
  }
  
  return(insecticide.efficacy)
}



