#'Equation 7B
#'


insecticide_treated_area_migration = function(exposure.scaling.factor = 10,
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
                                              max,intervention.coverage = 0.9, 
                                              min.dispersal.proportion = 0.1,
                                              max.dispersal.proportion = 0.9)
{
  
  population.resistance = insecticide_treated_area_selection_cost(
    exposure.scaling.factor = 10,
    nsim = 1000, 
    minimum.insecticide.resistance.hertitability = 0.05, 
    maximum.insecticide.resistance.hertitability = 0.30,
    minimum.male.insecticide.exposure = 0,
    maximum.male.insecticide.exposure = 1, 
    minimum.female.insecticide.exposure = 0.4, 
    maximum.female.insecticide.exposure = 0.9,
    resistance.cost,
    initial.resistance.intensity
    
  )*(1 - migration_treatment_to_refugia(nsim, 
                                        min.intervention.coverage = 0.1, 
                                        max,intervention.coverage = 0.9, 
                                        min.dispersal.proportion = 0.1,
                                        max.dispersal.proportion = 0.9)
    
  ))
  
}