#'Equation 7B
#'


insecticide_deployed_migration = function(exposure.scaling.factor = 10,
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
                                              max.dispersal.rate = 0.9)
{
  
  resistance.intensity.migration = (insecticide_deployed_selection_cost(
                                 exposure.scaling.factor = exposure.scaling.factor,
                                 nsim = nsim, 
                                 minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                 maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                 minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                 maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                 minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                 maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                 resistance.cost = resistance.cost,
                                 initial.resistance.intensity = initial.resistance.intensity) * 
                                   (1 - (migration_treatment_to_refugia(
                                        nsim = nsim, 
                                        min.intervention.coverage = min.intervention.coverage, 
                                        max.intervention.coverage = max.intervention.coverage, 
                                        min.dispersal.rate = min.dispersal.rate,
                                        max.dispersal.rate = max.dispersal.rate)))) +
                                   
                                   (refugia_selection_costs(
                                     initial.refugia.resistance = initial.refugia.resistance,
                                     resistance.cost = resistance.cost,
                                     exposure.scaling.factor = exposure.scaling.factor,
                                     nsim = nsim, 
                                     minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
                                     maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
                                     minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                     maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                     minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                     maximum.female.insecticide.exposure = maximum.female.insecticide.exposure) * 
                                    
                                    (migration_treatment_to_refugia(
                                      nsim = nsim, 
                                      min.intervention.coverage = min.intervention.coverage, 
                                      max.intervention.coverage = max.intervention.coverage, 
                                      min.dispersal.rate = min.dispersal.rate,
                                      max.dispersal.rate = max.dispersal.rate)))
  
  
  ##To prevent resistance intensity being less than 0
  resistance.intensity.migration = ifelse(resistance.intensity.migration < 0, 0, resistance.intensity.migration) 
  
  return(resistance.intensity.migration)
  
}
