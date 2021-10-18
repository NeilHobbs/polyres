#'@title Get the other part of the mixture that is deployed
#'
#'@description Identifies the other part of the mixture that is currently being deployed.
#'
#'@param deploy.mixture = The ID of the currently deployed mixture
#'@param generation = The current generation of the simulation
#'@param insecticide = The insecticide currently being tracked by the model
#'@param sim.array = The array holding the simulation

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