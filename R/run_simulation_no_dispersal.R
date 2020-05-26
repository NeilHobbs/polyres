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
#' @param resistance.cost cost of having insecticide resistance
#' @param initial.resistance.intensity The initial resistance intensities 
#' @param irm.strategy To be able to set the resistance management strategy (rotations, sequences)


#And need to figure out how to save into an array!

run_simulation_no_dispersal = function(number.of.insecticides = 3,
                                       exposure.scaling.factor = 10,
                                       nsim = 1000, 
                                       minimum.insecticide.resistance.hertitability = 0.05, 
                                       maximum.insecticide.resistance.hertitability = 0.30,
                                       minimum.male.insecticide.exposure = 0,
                                       maximum.male.insecticide.exposure = 1, 
                                       minimum.female.insecticide.exposure = 0.4, 
                                       maximum.female.insecticide.exposure = 0.9,
                                       resistance.cost,
                                       initial.resistance.intensity = 0
                                       #irm.strategy
                                       ){
  
  count.insecticides = set_number_of_insecticides(number.of.insecticides)
  
  # Set up the array for the data:
  name.column = c("generation", "intensity.site", "intensity.refugia", "deployment")
  name.matrix = c("insecticide")
  
  #allow for 500 generations (50 years) with rows
  simulation.data = array(c(500, 4, length(count.insecticides)), dimnames = list(NULL, name.column, name.matrix))
  
  
  ##Can be either rotation or sequence
  #strategy = irm.strategy #rotation or sequence
  
  #fixed intitial year:
  track.mean.site.resistance = initial.resistance.intensity
 
  for(year in 1:50){
    
     for(insecticide in 1:length(count.insecticides)){
      
       #Check if survival/resistance less than the limits
      if(check_resistance_10(current.resistance.status = track.mean.site.resistance) == TRUE){
        deploy.insecticide = TRUE} else (deploy.insecticide = FALSE)
      }
      #can insecticide be used? IR level less than 47 (5% survival) to be re-used. If TRUE can be re-used
      #check_resistance_5(current.resistance.status = track.mean.site.resistance)
      
      #check_resistance ##if less than 10% (IR = 100) can continue to be used. For sequences only??
      #check_resistance_10(current.resistance.status = track.mean.site.resistance)
    
      #How to deploy only 1 insecticide at a time??
      
      for(generation in 1:10){
        track.mean.site.resistance[generation+1] = if(deploy.insecticide == TRUE){
          
          insecticide_deployed_selection_cost(
            exposure.scaling.factor = exposure.scaling.factor,
            nsim = nsim, 
            minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
            maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
            minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
            maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
            minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
            maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
            resistance.cost = resistance.cost,
            initial.resistance.intensity = track.mean.site.resistance[generation])} else(
              
           insecticide_not_deployed_selection_cost(
             initial.resistance.intensity = track.mean.site.resistance[generation],
             resistance.cost = resistance.cost,
             exposure.scaling.factor = exposure.scaling.factor,
             nsim = nsim, 
             minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
             maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
             minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
             maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
             minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
             maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))
        
        #Input a check to prevent track.mean.site.resistance going below zero.
   }
    
    #return the mean population IR each year.
  return(track.mean.site.resistance)
  }
}


 
  