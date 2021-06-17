#Special cases:
  #1. Insecticide decay.
  #2. Population suppression.
  #3. Cross Selection.


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
#' @param min.cross.selection 
#' @param max.cross.selection
#' 
#' 
#' 
#' Currently this function runs.
#' 
run_simulation_intervention_special_cases = function(number.of.insecticides = 2,
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
                                                      irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                                      half.population.bioassay.survival.resistance = 900,
                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                      deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                      maximum.resistance.value = 25000,
                                                      min.cross.selection = -1,
                                                      max.cross.selection = 1,
                                                     applied.insecticide.dose,
                                                     recommended.insecticide.dose ,
                                                     threshold.generation ,
                                                     base.efficacy.decay.rate,
                                                     rapid.decay.rate,
                                                     intercept,
                                                     conversion.factor,
                                                     insecticide.suppression #TRUE or FALSE
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
  
  
  #Maybe create a separate function: set_starting_conditions() for the following chunk of code. In doing so;
  #be able to set each insecticide having a unique starting intensity. And would set the insecticide.info
  #and calculating the withdrawal and return thresholds. 
  
  #Set starting resistance intensities (fills in only the first row/generation). The other generations are set to NAs.
  #refugia site starting resistace intensity
  #Set the starting resistance scores for the insecticides:
  sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                             starting.refugia.resistance.score = starting.refugia.intensity,
                                             starting.intervention.resistance.score = starting.treatment.site.intensity,
                                             number.of.insecticides = number.of.insecticides)
  
    #Make a dataframe of the insecticide parameters:
  insecticide.parameters.df = create_insecticide_parameters_dataframe(number.of.insecticides = number.of.insecticides,
                                                                      applied.insecticide.dose = applied.insecticide.dose,
                                                                      recommended.insecticide.dose = recommended.insecticide.dose,
                                                                      threshold.generation = threshold.generation,
                                                                      base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                      rapid.decay.rate = rapid.decay.rate)
  
  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment. 
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.
  
  deployed.insecticide = rep(1, times = deployment.frequency)#Always start with insecticide 1. 
  #This is fine as all insecticides have equivalent properties.
  
  
  #The first insecticide deployed is always insecticide 1
  insecticide.efficacy.vector = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
                                                                   recommended.insecticide.dose = insecticide.parameters.df[1,3],
                                                                   threshold.generations = insecticide.parameters.df[1,4],
                                                                   base.efficacy.decay.rate = insecticide.parameters.df[1,5],
                                                                   rapid.decay.rate = insecticide.parameters.df[1,6],
                                                                   deployment.frequency = deployment.frequency)
  
  insecticide.info = list(available.vector, withdrawn.vector, deployed.insecticide, insecticide.efficacy.vector)
  #Set the withdrawal and return thresholds: requires inputting the desired proportion of survival as input parameters. These will require
  #the user to input the half.population.bioassay.survival.resistance; the required thresholds; and maximum.resistance.value [this will be incase,
  #a user decides to use a high Z50 value]. But we should recommend the Z50 to be 900.
  
  
  calc.withdrawal.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                              michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                              bioassay.survival = withdrawal.threshold.value,
                                                              estimate.precision = 0.0001,
                                                              sd.population.resistance = 0,
                                                              nsim = 1000,
                                                              minimum.resistance.value = 0,
                                                              maximum.resistance.value = maximum.resistance.value)
  
  calc.return.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                          michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          bioassay.survival = return.threshold.value,
                                                          estimate.precision = 0.0001,
                                                          sd.population.resistance = 0,
                                                          nsim = 1000,
                                                          minimum.resistance.value = 0,
                                                          maximum.resistance.value = maximum.resistance.value)
  
  
  #create a matrix holding the values of the cross selection coefficients
  genetic.correlation.matrix = make_cross_selection_matrix(number.of.insecticides = number.of.insecticides,
                                                           min.cross.selection = min.cross.selection,
                                                           max.cross.selection = max.cross.selection)
  

  
  
  #Also worth considering turning the for generation and for insecticide loops into functions,
  #as the code is other wise very large and chunky and therefore complicated to edit and adapt.
  #start at generation 2, as generation 1 has intensities set at 0.
  for(generation in 2:maximum.generations){
    
    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.insecticide[generation])){break}else{
      
      for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
        ##                                                   #ask whether insecticide is the same as deployed insecticide
        
        sim.array['treatment', insecticide, generation] = if(insecticide == deployed.insecticide[generation]){#Insecticide is deployed in treatment site
          #calculate population mean from previous population mean when insecticide present
          mean(insecticide_deployed_migration_special_cases(resistance.cost = resistance.cost,
                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                            nsim = nsim, 
                                                            minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                            maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                            minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                            maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                            minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                            maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                            min.dispersal.rate = min.dispersal.rate,
                                                            max.dispersal.rate = max.dispersal.rate,
                                                            min.intervention.coverage = min.intervention.coverage, 
                                                            max.intervention.coverage = max.intervention.coverage,
                                                            number.of.insecticides = number.of.insecticides,
                                                            cross.selection.matrix = genetic.correlation.matrix,
                                                            initial.resistance.intensity = sim.array['treatment', insecticide, generation - 1],
                                                            initial.refugia.resistance = sim.array['refugia', insecticide, generation - 1],
                                                            currently.deployed.insecticide = deployed.insecticide[generation],
                                                            current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                            sim.array= sim.array,
                                                            current.generation = generation,
                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                            intercept = intercept,
                                                            conversion.factor = conversion.factor,
                                                            insecticide.suppression = insecticide.suppression))} #end of insecticide deployed
        else( #insecticide is not deployed
          #calculate population mean when insecticide not deployed from previous population mean
          mean(insecticide_not_deployed_migration_special_cases(resistance.cost = resistance.cost,
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
                                                                currently.tracked.insecticide = insecticide,
                                                                cross.selection.matrix = genetic.correlation.matrix,
                                                                initial.resistance.intensity = sim.array['treatment', insecticide, generation-1],
                                                                initial.refugia.resistance = sim.array['refugia', insecticide, generation - 1],
                                                                currently.deployed.insecticide = deployed.insecticide[generation],
                                                                current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                intercept = intercept,
                                                                conversion.factor = conversion.factor,
                                                                sim.array = sim.array,
                                                                current.generation = generation,
                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                insecticide.suppression = insecticide.suppression)))
        
        #Do refugia second, updating each generation for each insecticide
        #calculate the population mean from the previous population mean
        sim.array['refugia', insecticide, generation] = if(insecticide == deployed.insecticide[generation]){#Insecticide is deployed in treatment site
          #calculate population mean from previous population mean when insecticide present
          mean(refugia_migration_effect_deployed_special_cases(resistance.cost = resistance.cost,
                                                                  exposure.scaling.factor = exposure.scaling.factor,
                                                                  nsim = nsim, 
                                                                  minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                  maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                  minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                  maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                  minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                  maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                  min.dispersal.rate = min.dispersal.rate,
                                                                  max.dispersal.rate = max.dispersal.rate,
                                                                  min.intervention.coverage = min.intervention.coverage, 
                                                                  max.intervention.coverage = max.intervention.coverage,
                                                                  number.of.insecticides = number.of.insecticides,
                                                                  currently.tracked.insecticide = insecticide,
                                                                  cross.selection.matrix = genetic.correlation.matrix,
                                                                  initial.resistance.intensity = sim.array['treatment', insecticide, generation - 1],
                                                                  initial.refugia.resistance = sim.array['refugia', insecticide, generation - 1],
                                                                  currently.deployed.insecticide = deployed.insecticide[generation],
                                                               current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                               intercept = intercept,
                                                               conversion.factor = conversion.factor,
                                                               sim.array = sim.array,
                                                               current.generation = generation,
                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                               insecticide.suppression = insecticide.suppression))} #end of insecticide deployed
        else( #insecticide is not deployed
          #calculate population mean when insecticide not deployed from previous population mean
          mean(refugia_migration_effect_not_deployed_special_cases(resistance.cost = resistance.cost,
                                                                      exposure.scaling.factor = exposure.scaling.factor,
                                                                      nsim = nsim, 
                                                                      minimum.insecticide.resistance.heritability = minimum.insecticide.resistance.heritability, 
                                                                      maximum.insecticide.resistance.heritability = maximum.insecticide.resistance.heritability,
                                                                      minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
                                                                      maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
                                                                      minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
                                                                      maximum.female.insecticide.exposure = maximum.female.insecticide.exposure,
                                                                      min.dispersal.rate = min.dispersal.rate,
                                                                      max.dispersal.rate = max.dispersal.rate,
                                                                      min.intervention.coverage = min.intervention.coverage, 
                                                                      max.intervention.coverage = max.intervention.coverage,
                                                                      currently.tracked.insecticide = insecticide,
                                                                      cross.selection.matrix = genetic.correlation.matrix,
                                                                      initial.resistance.intensity = sim.array['treatment', insecticide, generation - 1],
                                                                      initial.refugia.resistance = sim.array['refugia', insecticide, generation - 1],
                                                                      currently.deployed.insecticide = deployed.insecticide[generation],
                                                                   current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                   intercept = intercept,
                                                                   conversion.factor = conversion.factor,
                                                                   sim.array = sim.array,
                                                                   current.generation = generation,
                                                                   half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                   insecticide.suppression = insecticide.suppression)))#end insecticide not deployed
        
        
        #end of refugia
        
        #next do treatment site: requires checking if insecticide deployed or not (will be TRUE/FALSE)
        
        
      }}#end of forinsecticide loop
    
    #returns the mean population insecticide resistance each generation.
    
    
    #Which irm.strategy is being used: sequence or rotation
    
    #May be worth making the following chunk of code into its own function as it is a bit chunky
    #at the moment.
    #Update insecticide each time the deployment.frequency is reached:
    if(generation < maximum.generations){
      update.insecticide.info = if(generation %% deployment.frequency == 0){
        if(irm.strategy == "rotation"){
          irm_strategy_rotation(
            number.of.insecticides = number.of.insecticides,
            current.generation = generation,
            withdrawal.threshold = calc.withdrawal.threshold,
            return.threshold = calc.return.threshold,
            simulation.array = sim.array,
            available.vector = available.vector,
            withdrawn.vector = withdrawn.vector,
            current.insecticide = deployed.insecticide[generation],
            deployment.frequency = deployment.frequency,
            deployment.vector = deployed.insecticide)} else{
              if(irm.strategy == "sequence"){
                irm_strategy_sequence(
                  number.of.insecticides = number.of.insecticides,
                  current.generation = generation,
                  withdrawal.threshold = calc.withdrawal.threshold,
                  return.threshold = calc.return.threshold,
                  simulation.array = sim.array,
                  available.vector = available.vector,
                  withdrawn.vector = withdrawn.vector,
                  current.insecticide = deployed.insecticide[generation],
                  deployment.frequency = deployment.frequency,
                  deployment.vector = deployed.insecticide) 
              }
            } 
        
        #update.insectide.info[[1]] is the vector of the available insecticides
        #update.insecticide.info[[2]] is the vector of the withdrawn insecticides
        #update.insecticide.info[[3]] is the vector of the whole deployment =c(previous.deployment, new.deployment)
      }
      if(generation %% deployment.frequency == 0){available.vector = update.insecticide.info[[1]]}
      if(generation %% deployment.frequency == 0){withdrawn.vector = update.insecticide.info[[2]]}
      if(generation %% deployment.frequency == 0){deployed.insecticide = update.insecticide.info[[3]]
      to.be.deployed.insecticide = deployed.insecticide[generation+1]
      if(is.na(to.be.deployed.insecticide)){break}
      insecticide.efficacy.vector = c(insecticide.efficacy.vector,
                                      create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[to.be.deployed.insecticide, 2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df[to.be.deployed.insecticide, 3],
                                                                         threshold.generations = insecticide.parameters.df[to.be.deployed.insecticide, 4],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df[to.be.deployed.insecticide, 5],
                                                                         rapid.decay.rate = insecticide.parameters.df[to.be.deployed.insecticide, 6],
                                                                         deployment.frequency = deployment.frequency))
      
      
      }
     
      #Currently always giving the information from insecticide 1:
     
    }
    #A break point to stop simuation if there is no insecticide deployed
    #if(is.na(deployed.insecticide[generation])){break}
    
  }#end of for(generation) loop
  
  #ensure the simulation array is return after running
  #need to develop an quick and easy way to turn array into dataframes for plotting purposes
  return(list(sim.array, deployed.insecticide, insecticide.efficacy.vector))
}


