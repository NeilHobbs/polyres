#'This function calculates the migration effect from the treatment area to the refugia.
#'
#'@param min.intervention.coverage Minimum coverage of uniform distribution of insecticide intervention, set to 0.1
#'@param max.intervention.coverage Minimum coverage of uniform distribution of insecticide intervention, set to 0.9
#'@param min.dispersal.rate Minimum disperal proportion of uniform distribution, Set to 0.1
#'@param max.dispersal.rate Maximum dispersal propotion of unifrom distribution, set to 0.9
#'@param nsim Number of interations of the runif function 
#'
#'@return exchange.treatment.to.refugia

migration_treatment_to_refugia = function(nsim, 
                                          min.intervention.coverage = 0.1, 
                                          max,intervention.coverage = 0.9, 
                                          min.dispersal.proportion = 0.1,
                                          max.dispersal.proportion = 0.9){
  
  exchange.treatment.to.refugia = (1 - runif(nsim, min = min.intervention.coverage, max = max.intervention.coverage)) * 
                                     runif(nsim, min = min.dispersal.proportion, max= max.dispersal.proportion)
  
  return(exchange.treatment.to.refugia)
}
