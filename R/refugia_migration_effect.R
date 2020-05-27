#' Equation 9B
#' 



refugia_migration_effect = function(
                        initial.refugia.resistance,
                        resistance.cost,
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
  
    track.refugia.resistance = (refugia_selection_costs(
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
                                (1 - 
                                   (migration_refugia_to_treatment(
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
    migration_refugia_to_treatment(
                    nsim = 1000, 
                    min.intervention.coverage = min.intervention.coverage, 
                    max.intervention.coverage = max.intervention.coverage, 
                    min.dispersal.rate = min.dispersal.rate,
                    max.dispersal.rate = max.dispersal.rate))
  
  
  #Prevent resistance intensity going below 0
  track.refugia.resistance = ifelse(track.refugia.resistance < 0, 0, track.refugia.resistance)
  
  return(track.refugia.resistance)
}