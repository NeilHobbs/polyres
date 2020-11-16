#' This function runs the model in the absence of dispersal, but with selection costs. 
#' @param number.of.insecticides how many insecticides are available in the arsenal. 
#' @param exposure.scaling.factor = 10,
#' @param nsim = 1000, 
#' @param minimum.insecticide.resistance.heritability = 0.05, 
#' @param maximum.insecticide.resistance.heritability = 0.30,
#' @param minimum.male.insecticide.exposure = 0,
#' @param maximum.male.insecticide.exposure = 1, 
#' @param minimum.female.insecticide.exposure = 0.4, 
#' @param maximum.female.insecticide.exposure = 0.9,
#' @param resistance.cost,
#' @param initial.resistance.intensity,
#' @param min.intervention.coverage = 0.1, 
#' @param max.intervention.coverage = 0.9, 
#' @param initial.refugia.resistance,
#' @param min.dispersal.rate = 0.1,
#' @param max.dispersal.rate = 0.9
#' @param irm.strategy To be able to set the resistance management strategy (rotations, sequences)


#Notes:
#There does not appear to be migration to the refugia from the treatment site, as refugia values remain 0.00000. 


#And need to figure out how to save into an array!

run_simulation_endpoint = function(number.of.insecticides = 2,
                                   exposure.scaling.factor = 10,
                                   nsim = 1000,
                                   minimum.insecticide.resistance.heritability = 0.05,
                                   maximum.insecticide.resistance.heritability = 0.30,
                                   minimum.male.insecticide.exposure = 0,
                                   maximum.male.insecticide.exposure = 1,
                                   minimum.female.insecticide.exposure = 0.4,
                                   maximum.female.insecticide.exposure = 0.9,
                                   resistance.cost,
                                   starting.treatment.site.intensity = 0,
                                   starting.refugia.intensity = 0,
                                   min.intervention.coverage = 0.1,
                                   max.intervention.coverage = 0.9,
                                   min.dispersal.rate = 0.1,
                                   max.dispersal.rate = 0.9,
                                   maximum.generations = 300
                                   #irm.strategy
){
  
  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "treatment"), which hold resistance intensities. 
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  #dim 4: which insecticide is currently deployed
  #refugia is the refugia. treatment is the place where insecticides are the intervention site where insecticides are deployed.
  
  sim.array = create_starting_array(n.insecticides = number.of.insecticides, 
                                    maximum.generations = maximum.generations)
  
  
  #Set starting resistance intensities (fills in whole column with zeroes) - shouldn't be an issue as updated in for loops:
  #refugia site starting resistace intensity
  sim.array['refugia', ,] = starting.refugia.intensity
  
  #treatment site starting resistance intensity (where the insecticide can be deployed)
  sim.array['treatment', ,] = starting.treatment.site.intensity 
  
  #Make a pre-defined treatment strategy; this will have to be changeed
  #change to deployed.insecticide; as 4th dimension in sim.array?
  current.insecticide = c(rep(1, times = 100), rep(2, times = 100), 
                          rep(3, times = 100), rep(1, times=100), rep(2, times = 100)) # total 500. While playing around
  
  ##Define which insecticide resistance management is being used: Can be either rotation or sequence
  #strategy = irm.strategy 
  #put in an error message if irm.strategy is not rotation or sequence. Prevent model from running if that is the case.
  # "Have you specified irm.stategy either rotation or sequence?" 
  
  #start at generation 2, as generation 1 has intensities set.
  for(generation in 2:maximum.generations){
    
    #select an insecticide
    for(insecticide in 1:number.of.insecticides){
      
      #Check if survival/resistance less than the limits of 10% survival. #How to deploy only 1 insecticide at a time??
      #Use the resistance intensity of the previous generation at the treatment site.
      #if(check_resistance_10(current.resistance.status = sim.array['treatment', insecticide, generation - 1]) == TRUE){
      # deploy.insecticide = TRUE} else (deploy.insecticide = FALSE)
      
      
      ##This is just to make the simulation run for checking
      #deploy.insecticide = TRUE
      
      #can insecticide be used? IR level less than 47 (5% survival) to be re-used. If TRUE can be re-used
      #check_resistance_5(current.resistance.status = track.mean.site.resistance)
      
      #Do refugia first, updating each generation for each insecticide
      #calculate the population mean from the previous population mean
      sim.array['refugia', insecticide, generation] = mean(refugia_migration_effect(#this function calls refugia_selection_costs
        initial.refugia.resistance = sim.array['refugia', insecticide, generation - 1],
        initial.resistance.intensity = sim.array['treatment', insecticide, generation - 1],
        resistance.cost = resistance.cost,
        exposure.scaling.factor = exposure.scaling.factor,
        nsim = nsim, 
        minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
        maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
        minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
        maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
        minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
        maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
        min.intervention.coverage = min.intervention.coverage, 
        max.intervention.coverage = max.intervention.coverage, 
        min.dispersal.rate = min.dispersal.rate,
        max.dispersal.rate = max.dispersal.rate)) #end of refugia
      
      #next do treatment site: requires checking if insecticide deployed or not (will be TRUE/FALSE)
      
      
      ##                                                   #ask whether insecticide is the same as deployed insecticide
      sim.array['treatment', insecticide, generation] = if(insecticide == current.insecticide[generation]){#Insecticide is deployed in treatment site
        #calculate population mean from previous population mean when insecticide present
        mean(insecticide_deployed_migration(#this function calls insecticide_deployed_selection_costs
          exposure.scaling.factor = exposure.scaling.factor,
          nsim = nsim,
          minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability,
          maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
          minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
          maximum.male.insecticide.exposure = maximum.male.insecticide.exposure,
          minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
          maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
          resistance.cost = resistance.cost,
          initial.resistance.intensity = sim.array['treatment', insecticide, generation - 1],
          min.intervention.coverage = min.intervention.coverage,
          max.intervention.coverage = max.intervention.coverage,
          initial.refugia.resistance = sim.array['refugia', insecticide, generation - 1],
          min.dispersal.rate = min.dispersal.rate,
          max.dispersal.rate = max.dispersal.rate))} #end of insecticide deployed
      else( #insecticide is not deployed
        #calculate population mean when insecticide not deployed from previous population mean
        mean(insecticide_not_deployed_migration(#this function calls insecticide_not_deployed_selection_costs
          initial.resistance.intensity = sim.array['treatment', insecticide, generation - 1],#use previous generation info in treatment site
          resistance.cost = resistance.cost,
          initial.refugia.resistance = sim.array['refugia', insecticide, generation - 1], # use previous generation info in refugia
          exposure.scaling.factor = exposure.scaling.factor,
          nsim = nsim,
          minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability,
          maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
          minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
          maximum.male.insecticide.exposure = maximum.male.insecticide.exposure,
          minimum.female.insecticide.exposure = minimum.female.insecticide.exposure,
          maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
          min.intervention.coverage = min.intervention.coverage,
          max.intervention.coverage = max.intervention.coverage,
          min.dispersal.rate = min.dispersal.rate,
          max.dispersal.rate = max.dispersal.rate
        )))#end insecticide not deployed
      
    }#end of insecticide loop
    
    #return the mean population IR each year.
    
  }#end of for(generation) loop
  
  #ensure the simulation array is return after running
  #need to develop an quick and easy way to turn array into dataframes for plotting purposes
  
  T500 = mean(sim.array["treatment", , 500])
  T300 = mean(sim.array["treatment", , 300])
  T100 = mean(sim.array["treatment", , 100])
  R500 = mean(sim.array["refugia", , 500])
  R300 = mean(sim.array["refugia", , 300])
  R100 = mean(sim.array["refugia", , 100])
              
  A = list(T500, T300, T300, R500, R300, R100)            
           
  return(A) #This is modified to only return the intensity of the 500th gen in the treatment
  #for all the insecticides (mean)
}



