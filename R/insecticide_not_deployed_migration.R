#' Equation 8B
#' 


insecticide_not_deployed_migration = function(
                                              initial.resistance.intensity,
                                              resistance.cost,
                                              initial.refugia.resistance,
                                              exposure.scaling.factor = 10,
                                              nsim = 1000, 
                                              minimum.insecticide.resistance.hertitability = 0.05, 
                                              maximum.insecticide.resistance.hertitability = 0.30,
                                              minimum.male.insecticide.exposure = 0,
                                              maximum.male.insecticide.exposure = 1, 
                                              minimum.female.insecticide.exposure = 0.4, 
                                              maximum.female.insecticide.exposure = 0.9, 
                                              min.intervention.coverage = 0.1, 
                                              max.intervention.coverage = 0.9, 
                                              min.dispersal.rate = 0.1,
                                              max.dispersal.rate = 0.9){
  
  
  selection.cost.site = insecticide_not_deployed_selection_cost(
    initial.resistance.intensity = initial.resistance.intensity,
    resistance.cost = resistance.cost,
    exposure.scaling.factor = exposure.scaling.factor,
    nsim = nsim, 
    minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
    maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
    minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
    maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
    minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
    maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
 
  migration = migration_treatment_to_refugia(
                                      nsim = nsim, 
                                      min.intervention.coverage = min.intervention.coverage, 
                                      max.intervention.coverage = max.intervention.coverage, 
                                      min.dispersal.rate = min.dispersal.rate,
                                      max.dispersal.rate = max.dispersal.rate)
  
  selection.cost.refugia = refugia_selection_costs(
                                      initial.refugia.resistance = initial.refugia.resistance,
                                      resistance.cost = resistance.cost,
                                      exposure.scaling.factor = exposure.scaling.factor,
                                      nsim = nsim, 
                                      minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                      maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                      minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                      maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                      minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                      maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  track.resistance.intensity = (selection.cost.site *(1 - migration)) + (selection.cost.refugia * migration)
  
  #Prevent resistance values being lower than 0
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, 0, track.resistance.intensity)
  
  return(track.resistance.intensity)
  
}
