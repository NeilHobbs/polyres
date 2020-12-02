#'@title Add the cross selection between a deployed insecticide and an non-deployed insecticide. 
#'
#'@description This function implements the cross selection part of equation 7Ai in the MS. 
#'
#'@param currently.deployed.insecticide = The insecticide that is currently being deployed in the intervention(treatment) site. Will be deployed.insecticide[generation]
#'@param number.of.insecticides = The number of insecticides in the simulation.
#'@param cross.selection.matrix = A matrix containing the genetic correlation values between each insecticide
#'@param exposure.scaling.factor
#'@param nsim
#'@param minimum.insecticide.resistance.heritability
#'@param maximum.insecticide.resistance.heritability
#'@param minimum.male.insecticide.exposure
#'@param maximum.male.insecticide.exposure
#'@param minimum.female.insecticide.exposure
#'@param maximum.female.insecticide.exposure
#'@param resistance.cost

#This function implements the cross selection part of equation 7Ai in the MS. 
insecticide_deployed_cross_selection = function(currently.deployed.insecticide, #will be deployed.insecticide[generation]
                                                number.of.insecticides,
                                                cross.selection.matrix,
                                                exposure.scaling.factor = 10,
                                                nsim = 1000, 
                                                minimum.insecticide.resistance.heritability = 0.05, 
                                                maximum.insecticide.resistance.heritability = 0.30,
                                                minimum.male.insecticide.exposure = 0,
                                                maximum.male.insecticide.exposure = 1, 
                                                minimum.female.insecticide.exposure = 0.4, 
                                                maximum.female.insecticide.exposure = 0.9,
                                                resistance.cost = 0.1){

    #Warning and Error Messages
    if(0 > minimum.insecticide.resistance.heritability |minimum.insecticide.resistance.heritability > 1){stop("minimum.insecticide.resistance.heritability must be between 0 and 1")}
    if(0 > maximum.insecticide.resistance.heritability |maximum.insecticide.resistance.heritability > 1){stop("maximum.insecticide.resistance.heritability must be between 0 and 1")}
    if(minimum.insecticide.resistance.heritability > maximum.insecticide.resistance.heritability){stop("minimum.insecticide.resistance.heritability is greater than maximum.insecticide.resistance.heritability")}
    
    if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
    if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
    if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}
    
    if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
    if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
    if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}
    
  
     insecticide.vector = seq(1, number.of.insecticides, by = 1)
     
      #remove the currently deployed insectide, must not include the value of deployed.insecticide[generation]
     insecticide.vector.not.deployed = insecticide.vector[-currently.deployed.insecticide]
  
  
  #Is currently the same conditions for all insecticides so keep out of loop for speed of processing
  response.to.selection = mean(response_to_insecticide_selection(exposure.scaling.factor = exposure.scaling.factor,
                                                                     nsim = nsim, 
                                                                     minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                     maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                     minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                     maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                     minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                     maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) + 
      effect_of_fitness_cost(resistance.cost = resistance.cost,#plus sign used as  negative effect is put in effect_of_fitness_cost function 
                             exposure.scaling.factor = exposure.scaling.factor,
                             nsim = nsim, 
                             minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                             maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                             minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                             maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                             minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                             maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))
  
     #empty vector to hold changes as a result of cross selection
  cross.selection.values = c()
  #Loop for all non-deployed insecticides. 
   for(j in insecticide.vector.not.deployed){
  
    cross.selection.values[j] = response.to.selection * cross.selection.matrix[currently.deployed.insecticide, j]
                          #correlation from currently deployed to the "tracked in the current loop". 
    
}

  #Sum of the effects. 
cross.selection.value = sum(cross.selection.values, 
                            na.rm = TRUE) #Issue was that cross.selection.values was having NAs.

  return(cross.selection.value)
}




# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 3,
#                                           min.cross.selection = 1,
#                                           max.cross.selection = 1)
# 
# insecticide_deployed_cross_selection(currently.deployed.insecticide = 1,
#                                      number.of.insecticides = 3,
#                                      cross.selection.matrix = temp.matrix,
#                                      exposure.scaling.factor = 10,
#                                      nsim = 1000, 
#                                      minimum.insecticide.resistance.heritability = 0.5, 
#                                      maximum.insecticide.resistance.heritability = 0.5,
#                                      minimum.male.insecticide.exposure = 0.5,
#                                      maximum.male.insecticide.exposure = 0.5, 
#                                      minimum.female.insecticide.exposure = 0.5, 
#                                      maximum.female.insecticide.exposure = 0.5,
#                                      resistance.cost = 1)


