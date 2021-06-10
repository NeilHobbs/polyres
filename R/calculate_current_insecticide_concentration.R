#' @title Calculate the current insecticide concentration in deployment
#'
#' @param applied.concentration = The concentration of the active ingredient that was initially deployed
#' @param instantaneous.decay.rate = The decay rate of the insecticide
#' @param generations.since.deployment = The number of mosquito generations since the insecticide was deployed.

calculate_current_insecticide_concentration = function(applied.concentration,
                                                       instantaneous.decay.rate,
                                                       generations.since.deployment){

  current.concentration = applied.concentration * exp(-instantaneous.decay.rate * generations.since.deployment)

  if(current.concentration < 0)stop("Error: Insecticide concentration cannot be negative")
  
  return(current.concentration)

}
