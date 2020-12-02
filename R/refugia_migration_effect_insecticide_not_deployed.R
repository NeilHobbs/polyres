#' @title The effect of migration on the resistance intensity in the refugia when the insecticide in the treatment 
#' site is not deployed.
#' 
#' @description This function incorporates the migration of the mosquitoes from the refugia to the treatment site;
#' and from the treatment site to the refugia. This is all conducted after the selection and fitness costs have occurred
#' in both the refugia and the treatment site, which are both called in the function. Note; this function calls the selection
#' of insecticide resistance in the treatment site when the insecticide is not deployed in the treatment site.  
#' 
#' @param initial.refugia.resistance The current IR intensity in the refugia.
#' @param intitial.resistance.intensity The current IR intensty in the treatment site
#' @param resistance.cost The fitness cost of having IR
#' @param exposure.scaling.factor factor converting exposure into selection differential (default is 10)
#' @param nsim number of simulations of the runif functions
#' @param minimum.insecticide.resistance.heritability minimum heritability of IR (default is 0.05) 
#' @param maximum.insecticide.resistance.heritability maximum heritability of IR (default is 0.30)
#' @param minimum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 0)
#' @param maximum.male.insecticide.exposure Proportion of males exposed as a proportion of females (default is 1)
#' @param minimum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.4)
#' @param maximum.female.insecticide.exposure Proportion of females exposed at intervention site (default = 0.9)
#' @param min.intervention.coverage Proportion of the insect population in the treated intervention area (default is 0.1) 
#' @param max.intervention.coverage Proportion of the insect population in the treated intervention area (default is 0.9)  
#' @param min.dispersal.rate Proportion of population exchanged between the treatment and refugia sites per generation (default 0.1)
#' @param max.dispersal.rate Proportion of the population exchanged between the treatment and refugia sites per generation 
#' 
#' @return track.refugia.resistance A vector length nsim of resistance intensity values in the refugia


refugia_migration_effect_insecticide_not_deployed = function(initial.refugia.resistance,
                                                              initial.resistance.intensity,
                                                              resistance.cost,
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
  
  
  refugia.selection.costs = refugia_selection_costs(initial.refugia.resistance = initial.refugia.resistance,
                                                    resistance.cost = resistance.cost,
                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                    nsim = nsim, 
                                                    minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                    maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                    minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                    maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                    minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                    maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
    
    
  treatment.selection.costs =insecticide_not_deployed_selection_cost(initial.resistance.intensity = initial.resistance.intensity,
                                                                       resistance.cost = resistance.cost,
                                                                       exposure.scaling.factor = exposure.scaling.factor,
                                                                       nsim = nsim, 
                                                                       minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                       maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                       minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                       maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                       minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                       maximum.female.insecticide.exposure = maximum.female.insecticide.exposure)
   
    
  migration = migration_refugia_to_treatment(nsim = nsim, 
                                             min.intervention.coverage = min.intervention.coverage, 
                                             max.intervention.coverage = max.intervention.coverage, 
                                             min.dispersal.rate = min.dispersal.rate,
                                             max.dispersal.rate = max.dispersal.rate)
        
  

  track.refugia.resistance = (refugia.selection.costs *(1 - migration)) + (treatment.selection.costs * migration)
    #Prevent resistance intensity going below 0
  track.refugia.resistance = ifelse(track.refugia.resistance < 0, yes = 0, no = track.refugia.resistance)
  
  return(track.refugia.resistance)
  
}

#This is equation 9B in the MS