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
#' @param resistance.cost = default set to 0
#' @param initial.resistance.intensity,
#' @param min.intervention.coverage = 0.1, 
#' @param max.intervention.coverage = 0.9, 
#' @param initial.refugia.resistance,
#' @param min.dispersal.rate = 0.1,
#' @param max.dispersal.rate = 0.9
#' @param irm.strategy To be able to set the resistance management strategy (rotation, sequence)

#This function allows for the prespecification of the deployed order of insecticides. And does not take into account 
  #the need to withdraw/return insecticides. It is a practice function.

run_simulation = function(number.of.insecticides = 2,
                                        exposure.scaling.factor = 10,
                                        nsim = 1000,
                                        minimum.insecticide.resistance.heritability = 0.05,
                                        maximum.insecticide.resistance.heritability = 0.30,
                                        minimum.male.insecticide.exposure = 0,
                                        maximum.male.insecticide.exposure = 1,
                                        minimum.female.insecticide.exposure = 0.4,
                                        maximum.female.insecticide.exposure = 0.9,
                                        resistance.cost = 0,
                                        starting.treatment.site.intensity = 0,
                                        starting.refugia.intensity = 0,
                                        min.intervention.coverage = 0.1,
                                        max.intervention.coverage = 0.9,
                                        min.dispersal.rate = 0.1,
                                        max.dispersal.rate = 0.9,
                                        maximum.generations = 500
                                        #irm.strategy, will be sequence or rotation (plus mixture later on),
                                        #half.population.bioassay.survival.resistance = 900,
                                        #withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                        #return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                        #deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                        #maximum.resistance.value = 25000 #have arbitrarily high just in case
                                        #
                                        ){
      
       #Start by creating an array (calls the array_named function):
    #dimension 1: site = c("refugia", "treatment"), which hold resistance intensities. 
    #Easier to include both, but refugia won't happen if no dispersal
    #dimension 2: insectide to which the resistance intensity corresponds to
    #dimension 3: generation.
    #dim 4: which insecticide is currently deployed. Need to decide if it easier/better to have the deployed insecticide in the 
            #array; or just as a separate vector as is currently used. I think as a separate vector as it is currently working through this method.
    #refugia is the refugia. treatment is the place where insecticides are the intervention site where insecticides are deployed.
   
       sim.array = create_starting_array(n.insecticides = number.of.insecticides, 
                                          maximum.generations = maximum.generations)
    
      
    #Set starting resistance intensities (fills in whole column with zeroes) - shouldn't be an issue as updated in for loops:
                #refugia site starting resistace intensity
      sim.array['refugia', ,] = starting.refugia.intensity
      
                #treatment site starting resistance intensity (where the insecticide can be deployed)
      sim.array['treatment', ,] = starting.treatment.site.intensity 
      
      #Make a pre-defined treatment strategy; this will have to be changed.
      #change to deployed.insecticide; as 4th dimension in sim.array?
      current.insecticide = c(rep(1, times = 100), rep(2, times = 100), 
                              rep(3, times = 100), rep(1, times=100), rep(2, times = 100)) # total 500. While playing around
    
      #this creates a vector for holding deployed insecticide. Deployment always starts with insecticide 1. But as all insecticides 
        #are "the same" in terms of performance this currently does not matter. If it did matter then randomly select an insecticide from 
          #the available armory.
      
      #available.vector = seq(1, number.of.insecticides, by = 1)
      #withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides. 
      
      #deployed.insecticide = rep(1, times = frequency.deployment)
      
    #Set the withdrawal and return thresholds: requires inputting the desired proportion of survival as input parameters. These will require
      #the user to input the half.population.bioassay.survival.resistance; the required thresholds; and maximum.resistance.value [this will be incase,
        #a user decides to use a high Z50 value]. But we should recommend the Z50 to be 900.
      
      #withdrawal.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                              # michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                              # half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
                                                              # bioassay.survival = withdrawal.threshold.value, 
                                                              # estimate.precision = 0.01, 
                                                              # sd.population.resistance = 10,
                                                              # nsim = 1000,
                                                              # minimum.resistance.value = 0, 
                                                              # maximum.resistance.value = maximum.resistance.value)

      #return.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                        # michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                        # half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
                                                        # bioassay.survival = return.threshold.value, 
                                                        # estimate.precision = 0.01, 
                                                        # sd.population.resistance = 10,
                                                        # nsim = 1000,
                                                        # minimum.resistance.value = 0, 
                                                        # maximum.resistance.value = maximum.resistance.value)
      
    #start at generation 2, as generation 1 has intensities set.
   for(generation in 2:maximum.generations){

     #if(generation %% frequency.deployment == 0){
        #if(irm.strategy == rotation){for(check in 1:number.of.insecticides){DO THE ROTATION STRATEGY}}
        #if(irm.strategy == sequence){for(check in 1:number.of.insecticides){DO THE SEQUENCE STRATEGY}
     
    
     #A break point to stop simuation if there is no insecticide deployed
      #if(is.na(current.insecticide[generation])){break}
     
      for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
        
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
                    mean(insecticide_deployed_migration(#note: this function calls insecticide_deployed_selection_costs
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
      return(sim.array)
 }

 
# test_sim = run_simulation(maximum.generations = 50)
# print(test_sim)
 
  