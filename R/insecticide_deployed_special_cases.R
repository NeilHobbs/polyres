#' @title Incorporate cross selection that occurs from changes to other traits as a result of an insecticide being deployed
#' 
#' @description When insecticide i is deployed there is selection on traits J,K,L etc; which leads to a change in trait I
#' as a result of the genetic correlation between trait J, K, L etc. and trait I.
#' 
#' @param currently.deployed.insecticide = The insecticide that is currently being deployed deployed.insecticide[generation]
#' @param number.of.insecticides = The total number of insecticides available in the simulation
#' @param cross.selection.matrix = A matrix containing the cross selection values between each insecticide and trait. Made from the make_cross_selection_matrix function
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.heritability = 0.05, 
#' @param maximum.insecticide.resistance.heritability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9,
#' @param resistance.cost = The fitness cost associated with having insecticide resistance.
#' @param initial.resistance.intensity = The resistance intensity in the previous generation to the insecticide ["treatment", insecticide, generation - 1]
#' @param current.insecticide.efficacy = The insecticide efficacy of insecticide i at time since deployment τ defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.

insecticide_deployed_special_cases = function(currently.deployed.insecticide,
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
                                              resistance.cost = 0.1,
                                              initial.resistance.intensity,
                                              current.insecticide.efficacy){
  
  
  #This is the response of insecticide i on trait I
  direct.selection.response = mean(response_to_insecticide_selection_decay(exposure.scaling.factor = exposure.scaling.factor,
                                                                           nsim = nsim, 
                                                                           minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                           maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                           minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                           maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                           minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                           maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy)) 
  
  direct.selection.fitness.cost =  mean(effect_of_fitness_cost(resistance.cost = resistance.cost,
                                                          exposure.scaling.factor = exposure.scaling.factor,
                                                          nsim = nsim, 
                                                          minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                          maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                          minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                          maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                          minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                          maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))
  
  #This is the total of the effect of insecticide i on Traits J
  indirect.selection = mean(insecticide_deployed_cross_selection_decay(currently.deployed.insecticide = currently.deployed.insecticide,
                                                                       number.of.insecticides = number.of.insecticides,
                                                                       cross.selection.matrix = cross.selection.matrix,
                                                                       exposure.scaling.factor = exposure.scaling.factor,
                                                                       nsim = nsim, 
                                                                       minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                       maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                       minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                       maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                       minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                       maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                       resistance.cost = resistance.cost,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy))
  
  
  track.resistance.intensity = initial.resistance.intensity + (direct.selection.response + direct.selection.fitness.cost) + indirect.selection
  
  #Resistance intensity is not allowed to drop below zero.
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, yes = 0, no = track.resistance.intensity)
  
  return(track.resistance.intensity)
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# insecticide_deployed_indirect_cross_selection(currently.deployed.insecticide = 2,
#                                                                number.of.insecticides = 2,
#                                                                cross.selection.matrix = temp.matrix,
#                                                                exposure.scaling.factor = 10,
#                                                                nsim = 1,
#                                                                minimum.insecticide.resistance.heritability = 1,
#                                                                maximum.insecticide.resistance.heritability = 1,
#                                                                minimum.male.insecticide.exposure = 1,
#                                                                maximum.male.insecticide.exposure = 1,
#                                                                minimum.female.insecticide.exposure = 1,
#                                                                maximum.female.insecticide.exposure = 1,
#                                                                resistance.cost = 0,
#                                                                initial.resistance.intensity = 0)








