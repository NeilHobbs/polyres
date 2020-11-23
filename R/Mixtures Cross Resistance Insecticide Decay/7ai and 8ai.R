# 
# #7ai
# insecticide_deployed_direct_selection = function(currently.deployed.insecticide,
#                                                  number.of.insecticides = number.of.insecticides,
#                                                  cross.selection.matrix= cross.selection.matrix,
#                                                  exposure.scaling.factor = 10,
#                                                  nsim = 1000, 
#                                                  minimum.insecticide.resistance.heritability = 0.05, 
#                                                  maximum.insecticide.resistance.heritability = 0.30,
#                                                  minimum.male.insecticide.exposure = 0,
#                                                  maximum.male.insecticide.exposure = 1, 
#                                                  minimum.female.insecticide.exposure = 0.4, 
#                                                  maximum.female.insecticide.exposure = 0.9,
#                                                  resistance.cost = 0.1,
#                                                  initial.resistance.intensity = 0,
#                                                  ){
#   
#    insecticide.resistance.intensity =  insecticide_deployed_selection_cost(
#                                                                             exposure.scaling.factor = exposure.scaling.factor,
#                                                                             nsim = nsim, 
#                                                                             minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
#                                                                             maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
#                                                                             minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
#                                                                             maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
#                                                                             minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
#                                                                             maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
#                                                                             resistance.cost = resistance.cost,
#                                                                             initial.resistance.intensity = initial.resistance.intensity
#    ) + #Red section
#           cross_selection_effect_insecticide_deployed(currently.deployed.insecticide,
#                                                       number.of.insecticides = number.of.insecticides,
#                                                       cross.selection.matrix= cross.selection.matrix,
#                                                       exposure.scaling.factor = exposure.scaling.factor,
#                                                       nsim = nsim, 
#                                                       minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
#                                                       maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
#                                                       minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
#                                                       maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
#                                                       minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
#                                                       maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
#                                                       resistance.cost = resistance.cost,
#                                                       initial.resistance.intensity = 0 #Must be set to zero
#                                                       ) #Green section #Coded
#   
#    
#    return(insecticide.resistance.intensity)
# }
# 
# 
# #8ai
# insecticide_not_deployed_direct_and_indirect_selection = function(){
#   
#   insecticide.resistance.intensity = initial.insecticide.intensity + 
#     direct_cross_selection_insecticide_not_deployed()+ #Blue section #Coded
#             insecticide_not_deployed_cross_selection_j_i()+ #Orange section
#               indirect_cross_selection_effect_insecticide_not_deployed() #Purple Section #Coded
#   
#   return(insecticide.resistance.intensity)
#   
# }