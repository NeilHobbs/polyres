#' @title Run the insecticide resistance management simulation for sequences and rotations.
#' 
#' @description This is the main wrapper function that implements the running of the insecticide resistance management
#' simulations. Currently the simulations allows for the comparison of sequence and rotation strategies. At the moment,
#' each insecticide acts completely independently; such that there is no cross resistance and cross selection. 
#' For the "sequence" irm.strategy a single insecticide is continually deployed until it reaches the threshold for withdrawal,
#' at this point the next insecticide is deployed (at the next deployment opportunity). For the "rotation" irm.strategy, the
#' insecticide is changed at each deployment interval. It is therefore not recommend to compare rotations and sequences when
#' the number.of.insecticides = 1; as this will mean that the rotation strategy lasts a single deployment duration.
#' 
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
#' @param half.population.bioassay.survival.resistance
#' @param withdrawal.threshold.value
#' @param return.threshold.value
#' @param deployment.frequency
#' @param maximum.resistance.value  It is recommended that this be set as approximately 20*half.population.bioassay.survival.resistance
#' 

      #Current Error
# Error in if (is.na(deployed.mixture$mixture.id[generation])) { : 
#     argument is of length zero


run_simulation_intervention_test_mixtures = function(number.of.insecticides = 2,
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
                                                     maximum.generations = 500,
                                                     irm.deployment.strategy = "mixtures", #single, mixtures
                                                     mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
                                                     irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
                                                     half.population.bioassay.survival.resistance = 900,
                                                     withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                     return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                     deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                     maximum.resistance.value = 25000,
                                                     conversion.factor = 0.48,
                                                     intercept = 0.15){
  
  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "treatment"), which hold resistance intensities.
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  
  #array; or just as a separate vector as is currently used. I think as a separate vector as it is currently working through this method.
  #refugia is the refugia. treatment is the place where insecticides are the intervention site where insecticides are deployed.
  
  sim.array = create_starting_array(n.insecticides = number.of.insecticides,
                                    maximum.generations = maximum.generations)
  
  
  #Maybe create a separate function: set_starting_conditions() for the following chunk of code. In doing so;
  #be able to set each insecticide having a unique starting intensity. And would set the insecticide.info
  #and calculating the withdrawal and return thresholds. 
  
  #Set starting resistance intensities (fills in only the first row/generation). The other generations are set to NAs.
  #refugia site starting resistace intensity
  sim.array['refugia', , 1] = starting.refugia.intensity
  
  #treatment site starting resistance intensity (where the insecticide can be deployed)
  sim.array['treatment', , 1] = starting.treatment.site.intensity
  
  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment. 
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.
  
  #make the mixtures: 
    #Note we may decide that all insecticides are mixed to a pyrethroid (eg 1,2 ; 1,3; 1,4); with withdrawal/return decisions
    #only being made on the non-pyrethroid insecticide. 
  if(irm.deployment.strategy == "mixtures"){
    if(mixture.strategy == "mix.sequential.discrete"){
    mixture.df = make_mixture_sequential_discrete(number.of.insecticides = number.of.insecticides)
    }
    if(mixture.strategy == "mix.sequential.continous"){
      mixture.df = make_mixture_sequential_continous(number.of.insecticides = number.of.insecticides)
    }

    if(mixture.strategy == "random.mixtures"){

      mixture.df = choose_mixture_combinations(number.of.mixtures = number.of.insecticides,#make it so there is the same number of mixtures as there is number of insecticides
                                               potential.mixtures = make_all_possible_mixtures(number.of.insecticides = number.of.insecticides))

    }
    if(mixture.strategy == "pyrethroid.plus"){
      mixture.df = make_pyrethroid_mixtures(number.of.insecticides = number.of.insecticides)
    }
    
    mixture.id = rep(mixture.df[1, 1], times = deployment.frequency)#first row, first column
    mixture.part.1 = rep(mixture.df[1, 2], times = deployment.frequency)#first row, second column
    mixture.part.2 = rep(mixture.df[1, 3], times = deployment.frequency)#first row, third column
    
    #The dataframe that holds the mixture deployment information
    deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)
    
  }
  
  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment. 
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.
  
  available.mixtures = mixture.df #at the start all mixtures will be available
  
  mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)
  
  calc.withdrawal.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                              michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                              bioassay.survival = withdrawal.threshold.value,
                                                              estimate.precision = 0.001,
                                                              sd.population.resistance = 0,
                                                              nsim = 1000,
                                                              minimum.resistance.value = 0,
                                                              maximum.resistance.value = maximum.resistance.value)
  
  calc.return.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                          michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          bioassay.survival = return.threshold.value,
                                                          estimate.precision = 0.001,
                                                          sd.population.resistance = 0,
                                                          nsim = 1000,
                                                          minimum.resistance.value = 0,
                                                          maximum.resistance.value = maximum.resistance.value)
  
  
  #Also worth considering turning the for generation and for insecticide loops into functions,
  #as the code is other wise very large and chunky and therefore complicated to edit and adapt.
  #start at generation 2, as generation 1 has intensities set at 0.
  for(generation in 2:maximum.generations){
    
    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.mixture$mixture.id[generation])){break}else{
      
      for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
                    ##                                                   #ask whether insecticide is the same as deployed insecticide
        sim.array['treatment', insecticide, generation] = if(insecticide == deployed.mixture$mixture.part.1[generation] |
                                                             deployed.mixture$mixture.part.2[generation]){#Insecticide is deployed in treatment site
          #calculate population mean from previous population mean when insecticide present
          mean(insecticide_deployed_migration_mixture(#note: this function calls insecticide_deployed_selection_costs_mixture
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
            max.dispersal.rate = max.dispersal.rate,
            intensity.to.other.mixture.part = resistance_intensity_to_other_part_of_mixture(deployed.mixture = deployed.mixture,
                                                                                            generation = generation,
                                                                                            insecticide = insecticide,
                                                                                            sim.array = sim.array),
            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
            conversion.factor = conversion.factor,
            intercept = intercept))} #end of insecticide deployed
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
            max.dispersal.rate = max.dispersal.rate,
            conversion.factor = conversion.factor,
            intercept = intercept
          )))#end insecticide not deployed    
        
      #Do refugia second, updating each generation for each insecticide
        #calculate the population mean from the previous population mean
        sim.array['refugia', insecticide, generation] = if(insecticide == deployed.mixture$mixture.part.1[generation] |
                                                           deployed.mixture$mixture.part.2[generation]){#Insecticide is deployed in treatment site
          #calculate population mean from previous population mean when insecticide present
          mean(refugia_migration_effect_insecticide_deployed_mixture(initial.refugia.resistance = sim.array["refugia", insecticide, generation - 1],
                                                                     initial.resistance.intensity = sim.array["treatment", insecticide, generation - 1],
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
                                                                     max.dispersal.rate = max.dispersal.rate,
                                                                     intensity.to.other.mixture.part = resistance_intensity_to_other_part_of_mixture(deployed.mixture = deployed.mixture,
                                                                                                                                                     generation = generation,
                                                                                                                                                     insecticide = insecticide,
                                                                                                                                                     sim.array = sim.array),
                                                                     half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                     conversion.factor = conversion.factor,
                                                                     intercept = intercept))}
          #calculate population mean when insecticide not deployed from previous population mean
          else(mean(refugia_migration_effect_insecticide_not_deployed(
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
        
       #end of refugia
        
        #next do treatment site: requires checking if insecticide deployed or not (will be TRUE/FALSE)

        
      }}#end of forinsecticide loop
    
    #returns the mean population insecticide resistance each generation.
    
    
    #Which irm.strategy is being used: sequence or rotation
    
    #May be worth making the following chunk of code into its own function as it is a bit chunky
    #at the moment.
    #Update insecticide each time the deployment.frequency is reached:
    if(generation < maximum.generations){
      update.mixture.info = if(generation %% deployment.frequency == 0){
        if(irm.switch.strategy == "rotation"){
          irm_strategy_rotation_mixture(number.of.insecticides = number.of.insecticides,
                                        current.generation = generation,
                                        withdrawal.threshold = calc.withdrawal.threshold,
                                        return.threshold = calc.return.threshold,
                                        simulation.array = sim.array,
                                        available.vector = available.vector,
                                        withdrawn.vector = withdrawn.vector,
                                        mixture.df = mixture.df,
                                        current.mixture = deployed.mixture$mixture.id[generation],
                                        deployment.frequency = deployment.frequency,
                                        deployment.df = deployed.mixture)} else{
              if(irm.switch.strategy == "sequence"){
                irm_strategy_sequence_mixture(number.of.insecticides = number.of.insecticides,
                                              current.generation = generation,
                                              withdrawal.threshold = calc.withdrawal.threshold,
                                              return.threshold = calc.return.threshold,
                                              simulation.array = sim.array,
                                              available.vector = available.vector,
                                              withdrawn.vector = withdrawn.vector,
                                              mixture.df = mixture.df,
                                              current.mixture = deployed.mixture$mixture.id[generation],
                                              deployment.frequency = deployment.frequency,
                                              deployment.df = deployed.mixture)
              }
            } 
        
        # mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)
      }
      if(generation %% deployment.frequency == 0){available.mixtures = update.mixture.info[[1]]}
      if(generation %% deployment.frequency == 0){available.vector = update.mixture.info[[2]]}
      if(generation %% deployment.frequency == 0){withdrawn.vector = update.mixture.info[[3]]}
      if(generation %% deployment.frequency == 0){deployed.mixture = update.mixture.info[[4]]}
    }
    #A break point to stop simuation if there is no insecticide deployed
    #if(is.na(deployed.insecticide[generation])){break}
    
  }#end of for(generation) loop
  
  #ensure the simulation array is return after running
  #need to develop an quick and easy way to turn array into dataframes for plotting purposes
  return(list(sim.array, deployed.mixture))
}


# A = run_simulation_intervention_test_mixtures(number.of.insecticides = 4,
#                                               exposure.scaling.factor = 10,
#                                               nsim = 1,
#                                               minimum.insecticide.resistance.heritability = 1,
#                                               maximum.insecticide.resistance.heritability = 1,
#                                               minimum.male.insecticide.exposure = 1,
#                                               maximum.male.insecticide.exposure = 1,
#                                               minimum.female.insecticide.exposure = 1,
#                                               maximum.female.insecticide.exposure = 1,
#                                               resistance.cost = 0,
#                                               starting.treatment.site.intensity = 10,
#                                               starting.refugia.intensity = 0,
#                                               min.intervention.coverage = 1,
#                                               max.intervention.coverage = 1,
#                                               min.dispersal.rate = 0,
#                                               max.dispersal.rate = 0,
#                                               maximum.generations = 50,
#                                               irm.deployment.strategy = "mixtures", #single, mixtures
#                                               mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
#                                               irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
#                                               half.population.bioassay.survival.resistance = 900,
#                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                               maximum.resistance.value = 25000)
# 
# 
# 
# B = get_simulation_dataframe_mixtures(simulation.array = A,
#                                     number.of.insecticides = 2,
#                                     maximum.generations = 50)
# 

