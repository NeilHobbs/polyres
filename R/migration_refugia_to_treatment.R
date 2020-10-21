#'@title  calculate the migration effect from the refugia to the treatment area.
#'
#'@param min.intervention.coverage Minimum coverage of uniform distribution of insecticide intervention, set to 0.1
#'@param max.intervention.coverage Minimum coverage of uniform distribution of insecticide intervention, set to 0.9
#'@param min.dispersal.rate Minimum disperal proportion of uniform distribution, Set to 0.1
#'@param max.dispersal.rate Maximum dispersal propotion of unifrom distribution, set to 0.9
#'@param nsim Number of interations of the runif function 
#'
#'@return exchange.refugia.to.treatment A vector of length nsim

migration_refugia_to_treatment = function(nsim = 1000, 
                                          min.intervention.coverage = 0.1, 
                                          max.intervention.coverage = 0.9, 
                                          min.dispersal.rate = 0.1,
                                          max.dispersal.rate = 0.9){
  
  #Error messages to prevent incorrect parameter values being used. 
  if(0 > min.intervention.coverage | min.intervention.coverage > 1){stop("min.intervention.coverage must be between 0 and 1")}
  if(0 > max.intervention.coverage | max.intervention.coverage > 1){stop("max.intervention.coverage must be between 0 and 1")}
  if(min.intervention.coverage > max.intervention.coverage){stop("min.intervention.coverage is greater than max.intervention.coverage")}

  if(0 > min.dispersal.rate | min.dispersal.rate > 1){stop("min.dispersal.rate must be between 0 and 1")}
  if(0 > max.dispersal.rate | max.dispersal.rate > 1){stop("max.dispersal.rate must be between 0 and 1")}
  if(min.dispersal.rate > max.dispersal.rate){stop("min.dispersal.rate is greater than max.dispersal.rate")}
  
  exchange.refugia.to.treatment = runif(nsim, min = min.intervention.coverage, max = max.intervention.coverage) * 
                                     runif(nsim, min = min.dispersal.rate, max= max.dispersal.rate)
  
  return(exchange.refugia.to.treatment)
  
}

#This is equation 6B in the MS