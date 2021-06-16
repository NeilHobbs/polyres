#' @title The tracked insecticide is not deployed in the intervention site. With decay, population suppression and cross selection.
#' 
#' @description 
#' 
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.heritability = 0.05, 
#' @param maximum.insecticide.resistance.heritability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9,
#' @param resistance.cost = 0.1,
#' @param initial.resistance.intensity = 0,
#' @param cross.selection.matrix,
#' @param currently.deployed.insecticide,
#' @param currently.tracked.insecticide,
#' @param current.insecticide.efficacy

#This is equation 8a(i)
insecticide_not_deployed_special_cases = function(exposure.scaling.factor = 10,
                                                  nsim = 1000, 
                                                  minimum.insecticide.resistance.heritability = 0.05, 
                                                  maximum.insecticide.resistance.heritability = 0.30,
                                                  minimum.male.insecticide.exposure = 0,
                                                  maximum.male.insecticide.exposure = 1, 
                                                  minimum.female.insecticide.exposure = 0.4, 
                                                  maximum.female.insecticide.exposure = 0.9,
                                                  resistance.cost = 0.1,
                                                  initial.resistance.intensity = 0,
                                                  cross.selection.matrix,
                                                  currently.deployed.insecticide,
                                                  currently.tracked.insecticide,
                                                  current.insecticide.efficacy){
  
  #The genetic correlation between the trait which gives resistance to the tracked insecticide the trait
  #whih gives resistance to the the deployed insecticide.
  genetic.correlation = cross.selection.matrix[currently.deployed.insecticide, currently.tracked.insecticide]
  
  #Same for all insecticides at the moment
  fitness.costs = effect_of_fitness_cost(resistance.cost = resistance.cost,
                                         exposure.scaling.factor = exposure.scaling.factor,
                                         nsim = nsim, 
                                         minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                         maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                         minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                         maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                         minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                         maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  #Same for all insecticides at the moment
  response.to.selection = response_to_insecticide_selection_decay(exposure.scaling.factor = exposure.scaling.factor,
                                                            nsim = nsim, 
                                                            minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                            maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                            minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                            maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                            minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                            maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                            current.insecticide.efficacy = current.insecticide.efficacy)
  
  #The effect of indirect selection.
  indirect.selection = (response.to.selection + fitness.costs) * genetic.correlation
  
  track.resistance.intensity = initial.resistance.intensity + fitness.costs + indirect.selection
  
  #resistance intensity values cannot be below zero.
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, yes = 0, no = track.resistance.intensity)  
  
  return(track.resistance.intensity)
}


# temp.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
#                                           min.cross.selection = 0,
#                                           max.cross.selection = 0)
# 
# insecticide_not_deployed_indirect_cross_selection(exposure.scaling.factor = 10,
#                                                     nsim = 1,
#                                                     minimum.insecticide.resistance.heritability = 1,
#                                                     maximum.insecticide.resistance.heritability = 1,
#                                                     minimum.male.insecticide.exposure = 1,
#                                                     maximum.male.insecticide.exposure = 1,
#                                                     minimum.female.insecticide.exposure = 1,
#                                                     maximum.female.insecticide.exposure = 1,
#                                                     resistance.cost = 0.5,
#                                                     initial.resistance.intensity = 50,
#                                                     cross.selection.matrix = temp.matrix,
#                                                     currently.deployed.insecticide = 1,
#                                                     currently.tracked.insecticide = 2)
# 
# 
# 
# insecticide_not_deployed_selection_cost(
#    initial.resistance.intensity = 50,
#    resistance.cost = 0.5,
#    exposure.scaling.factor = 10,
#    nsim = 1, 
#    minimum.insecticide.resistance.heritability = 1, 
#    maximum.insecticide.resistance.heritability = 1,
#    minimum.male.insecticide.exposure = 1,
#    maximum.male.insecticide.exposure = 1, 
#    minimum.female.insecticide.exposure = 1, 
#    maximum.female.insecticide.exposure = 1)
