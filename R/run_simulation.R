#' This function runs the model in the absence of dispersal, but with selection costs. 
#' @param number.of.insecticides how many insecticides are available in the arsenal. 
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.hertitability = 0.05, 
#' @param maximum.insecticide.resistance.hertitability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9,
#' @param resistance.cost,
#' @param initial.resistance.intensity,
#' @param min.intervention.coverage = 0.1, 
#' @param max.intervention.coverage = 0.9, 
#' @param initial.refugia.resistance,
#' @param min.dispersal.rate = 0.1,
#' @param max.dispersal.rate = 0.9
#' @param irm.strategy To be able to set the resistance management strategy (rotations, sequences)


#And need to figure out how to save into an array!

run_simulation = function(number.of.insecticides = 1,
                                       exposure.scaling.factor = 10,
                                       nsim = 1000, 
                                       minimum.insecticide.resistance.hertitability = 0.05, 
                                       maximum.insecticide.resistance.hertitability = 0.30,
                                       minimum.male.insecticide.exposure = 0,
                                       maximum.male.insecticide.exposure = 1, 
                                       minimum.female.insecticide.exposure = 0.4, 
                                       maximum.female.insecticide.exposure = 0.9,
                                       resistance.cost,
                                       initial.resistance.intensity,
                                       min.intervention.coverage = 0.1, 
                                       max.intervention.coverage = 0.9, 
                                       initial.refugia.resistance,
                                       min.dispersal.rate = 0.1,
                                       max.dispersal.rate = 0.9
                                       #irm.strategy
                                       ){
  
  count.insecticides = set_number_of_insecticides(number.of.insecticides)
  
  # Set up the array for the data:
  #allow for 500 generations (50 years) with rows.: How to store the data into the array?
  #Columns: generation, treatment.site IR, refugia IR, insecticide.deployed. 
  simulation.data = array(c(500, 4, length(count.insecticides)))
  
  
  ##Can be either rotation or sequence
  #strategy = irm.strategy
  
  #set starting intensities to zero.
  track.mean.site.resistance = 0
  
  for(year in 1:50){
    
    #select an insecticide
     for(insecticide in 1:length(count.insecticides)){
      
      #Check if survival/resistance less than the limits of 10% survival. #How to deploy only 1 insecticide at a time??
      if(check_resistance_10(current.resistance.status = track.mean.site.resistance[insecticide]) == TRUE){
        deploy.insecticide = TRUE} else (deploy.insecticide = FALSE)
      
      #can insecticide be used? IR level less than 47 (5% survival) to be re-used. If TRUE can be re-used
      #check_resistance_5(current.resistance.status = track.mean.site.resistance)
      
       ##then run all the generations of a year with each insecticide
      for(generation in 1:10){
        ##Incorporate 
        track.mean.site.resistance[generation + 1] = if(deploy.insecticide == TRUE){ #or [generation] = function[generation - 1]?
                   mean(insecticide_deployed_migration(
              exposure.scaling.factor = exposure.scaling.factor,
              nsim = nsim, 
              minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
              maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
              minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
              maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
              minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
              maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
              resistance.cost = resistance.cost,
              initial.resistance.intensity = track.mean.site.resistance[generation],
              min.intervention.coverage = min.intervention.coverage, 
              max.intervention.coverage = max.intervention.coverage, 
              initial.refugia.resistance = track.mean.refugia.resistance[generation],
              min.dispersal.rate = min.dispersal.rate,
              max.dispersal.rate = max.dispersal.rate))} else(
         
                   mean(insecticide_not_deployed_migration(
             initial.resistance.intensity,
             resistance.cost,
             initial.refugia.resistance = track.mean.site.resistance[generation], #or [generation - 1?, with [generation] as the target]
             exposure.scaling.factor = exposure.scaling.factor,
             nsim = nsim, 
             minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
             maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
             minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
             maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
             minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
             maximum.female.insecticide.exposure = maximum.female.insecticide.exposure, 
             min.intervention.coverage = min.intervention.coverage, 
             max.intervention.coverage = max.intervention.coverage, 
             min.dispersal.rate = min.dispersal.rate,
             max.dispersal.rate = max.dispersal.rate
           )))
        
           track.mean.refugia.resistance[generation + 1] = mean(refugia_migration_effect(
                                            initial.refugia.resistance = track.mean.refugia.resistance[generation],
                                            resistance.cost = resistance.cost,
                                            exposure.scaling.factor = exposure.scaling.factor,
                                            nsim = nsim, 
                                            minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                            maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                            minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                            maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                            minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                            maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                            min.intervention.coverage = min.intervention.coverage, 
                                            max.intervention.coverage = max.intervention.coverage, 
                                            min.dispersal.rate = min.dispersal.rate,
                                            max.dispersal.rate = max.dispersal.rate))
           #end of for(generation) loop
           }
    
    #return the mean population IR each year.
  return(track.mean.site.resistance[year])
  }
}
}


 
  