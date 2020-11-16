#' @title Effect of migration when the corresponding insecticide is not deployed.
#' 
#' @param initial.resistance.intensity This is the resistance intensity in the treatment site
#' @param resistance.cost,
#' @param initial.refugia.resistance This is the resistance intensity in the refugia
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.heritability = 0.05, 
#' @param maximum.insecticide.resistance.heritability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9, 
#' @param min.intervention.coverage = 0.1, 
#' @param max.intervention.coverage = 0.9, 
#' @param min.dispersal.rate = 0.1,
#' @param max.dispersal.rate = 0.9

insecticide_not_deployed_migration = function(
                                              initial.resistance.intensity,
                                              resistance.cost,
                                              initial.refugia.resistance,
                                              exposure.scaling.factor = 10,
                                              nsim = 1000, 
                                              minimum.insecticide.resistance.heritability = 0.05, 
                                              maximum.insecticide.resistance.heritability = 0.30,
                                              minimum.male.insecticide.exposure = 0,
                                              maximum.male.insecticide.exposure = 1, 
                                              minimum.female.insecticide.exposure = 0.4, 
                                              maximum.female.insecticide.exposure = 0.9, 
                                              min.intervention.coverage = 0.1, 
                                              max.intervention.coverage = 0.9, 
                                              min.dispersal.rate = 0.1,
                                              max.dispersal.rate = 0.9){
  
  #error and warning messages:
  if(0 > minimum.insecticide.resistance.heritability |minimum.insecticide.resistance.heritability > 1){stop("minimum.insecticide.resistance.heritability must be between 0 and 1")}
  if(0 > maximum.insecticide.resistance.heritability |maximum.insecticide.resistance.heritability > 1){stop("maximum.insecticide.resistance.heritability must be between 0 and 1")}
  if(minimum.insecticide.resistance.heritability > maximum.insecticide.resistance.heritability){stop("minimum.insecticide.resistance.heritability is greater than maximum.insecticide.resistance.heritability")}
  
  if(0 > minimum.male.insecticide.exposure | minimum.male.insecticide.exposure > 1){stop("minimum.male.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.male.insecticide.exposure | maximum.male.insecticide.exposure > 1){stop("maximum.male.insecticide.exposure must be between 0 and 1")}
  if(minimum.male.insecticide.exposure > maximum.male.insecticide.exposure){stop("minimum.male.insecticide.exposure is greater than maximum.male.insecticide.exposure")}
  
  if(0 > minimum.female.insecticide.exposure | minimum.female.insecticide.exposure > 1){stop("minimum.female.insecticide.exposure must be between 0 and 1")}
  if(0 > maximum.female.insecticide.exposure | maximum.female.insecticide.exposure > 1){stop("maximum.female.insecticide.exposure must be between 0 and 1")}
  if(minimum.female.insecticide.exposure > maximum.female.insecticide.exposure){stop("minimum.female.insecticide.exposure is greater than maximum.female.insecticide.exposure")}
  
  if(0 > min.intervention.coverage | min.intervention.coverage > 1){stop("min.intervention.coverage must be between 0 and 1")}
  if(0 > max.intervention.coverage | max.intervention.coverage > 1){stop("max.intervention.coverage must be between 0 and 1")}
  if(min.intervention.coverage > max.intervention.coverage){stop("min.intervention.coverage is greater than max.intervention.coverage")}

  if(0 > min.dispersal.rate | min.dispersal.rate > 1){stop("min.dispersal.rate must be between 0 and 1")}
  if(0 > max.dispersal.rate | max.dispersal.rate > 1){stop("max.dispersal.rate must be between 0 and 1")}
  if(min.dispersal.rate > max.dispersal.rate){stop("min.dispersal.rate is greater than max.dispersal.rate")}
  
  
  
  
  #First calculate the fitness cost when the insecticide is not deployed in the treatment site
  selection.cost.site = insecticide_not_deployed_selection_cost(
    initial.resistance.intensity = initial.resistance.intensity,
    resistance.cost = resistance.cost,
    exposure.scaling.factor = exposure.scaling.factor,
    nsim = nsim, 
    minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
    maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
    minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
    maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
    minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
    maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
 
  #Second allow for migration from the treatment site to the refugia
  migration = migration_treatment_to_refugia(
                                      nsim = nsim, 
                                      min.intervention.coverage = min.intervention.coverage, 
                                      max.intervention.coverage = max.intervention.coverage, 
                                      min.dispersal.rate = min.dispersal.rate,
                                      max.dispersal.rate = max.dispersal.rate)
  
  #Third allow for the fitness costs in the refugia
  selection.cost.refugia = refugia_selection_costs(
                                      initial.refugia.resistance = initial.refugia.resistance,
                                      resistance.cost = resistance.cost,
                                      exposure.scaling.factor = exposure.scaling.factor,
                                      nsim = nsim, 
                                      minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                      maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                      minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                      maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                      minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                      maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
  
  track.resistance.intensity = (selection.cost.site *(1 - migration)) + (selection.cost.refugia * migration)
  
  #Prevent resistance values being lower than 0
  track.resistance.intensity = ifelse(track.resistance.intensity < 0, 0, track.resistance.intensity)
  
  return(track.resistance.intensity)#returns a vector of length nsim
  
}

#This is equation 8B in the MS
