get_tracked_insecticide_efficacy = function(tracked.insecticide,
                                        deployed.mixture,
                                        generation){
  
    if(tracked.insecticide == deployed.mixture$mixture.part.1[generation]){
    insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation] #efficacy of part 1
  }
  
  if(tracked.insecticide == deployed.mixture$mixture.part.2[generation]){
    insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation]#efficacy of part 2
  }
  
  return(insecticide.efficacy)
}
