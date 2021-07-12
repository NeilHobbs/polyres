#'@title Get the other part of the mixture that is deployed

get_other_part_of_mixture = function(deployed.mixture,
                                     generation,
                                     insecticide,
                                     sim.array){
  
  #if deployed.mixture.1 is being tracked; get resistance intensity to deployed.mixture.2
  if(insecticide == deployed.mixture$mixture.part.1[generation]){
    other.mixture.part = deployed.mixture$mixture.part.2[generation]
  }
  #if deployed.mixture.2 is being tracked; calculate survival from deployed.mixture.1
  if(insecticide == deployed.mixture$mixture.part.2[generation]){
    other.mixture.part = deployed.mixture$mixture.part.1[generation]
  }
  
  return(other.mixture.part) 
}