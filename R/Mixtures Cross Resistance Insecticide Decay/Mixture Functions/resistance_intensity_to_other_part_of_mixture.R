#' @title Calculate the bioassay survival to the second part of the mixture.

#' @description The purpose of this function is to find the bioassay survival to the second part of the insecticide mixture.

resistance_intensity_to_other_part_of_mixture = function(deployed.mixture,
                                             generation,
                                             insecticide,
                                             sim.array){
  
  #if deployed.mixture.1 is being tracked; get resistance intensity to deployed.mixture.2
if(insecticide == deployed.mixture$deployed.mixture.1[generation]){
  resistance.intensity.to.other.insecticide = sim.array["treatment", deployed.mixture$deployed.mixture.2[generation - 1], generation -1]
}
  #if deployed.mixture.2 is being tracked; calculate survival from deployed.mixture.1
if(insecticide == deployed.mixture$deployed.mixture.2[generation]){
  resistance.intensity.to.other.insecticide =  sim.array["treatment", deployed.mixture$deployed.mixture.1[generation - 1], generation - 1]
}
                                                                        
  return(resistance.intensity.to.other.insecticide) 
}



