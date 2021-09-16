#Looking at the impact of allowing population suppression in the intervention site.
#SET 19
#1. load in the required R packages:
library(devtools)
load_all() #for polyres

parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")[1:5000, 2:7]

#First run for sequences 
temp.list.sequence = list()
for(v in 1:nrow(parameter.space)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention_special_cases(number.of.insecticides = 2,
                                                                             exposure.scaling.factor = 10,
                                                                             nsim = 1,
                                                                             minimum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                                             maximum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                                             minimum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                                             maximum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                                             minimum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                                             maximum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                                             resistance.cost = parameter.space$Fitness.Cost[v],
                                                                             starting.treatment.site.intensity = 0,
                                                                             starting.refugia.intensity = 0,
                                                                             min.intervention.coverage = parameter.space$Intervention.Coverage[v],
                                                                             max.intervention.coverage = parameter.space$Intervention.Coverage[v],
                                                                             min.dispersal.rate = parameter.space$Dispersal[v],
                                                                             max.dispersal.rate = parameter.space$Dispersal[v],
                                                                             maximum.generations = 500, #appoximately 50 years
                                                                             irm.strategy = "sequence", 
                                                                             half.population.bioassay.survival.resistance = 900, 
                                                                             withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                             return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                             deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                             maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                             min.cross.selection = 0,
                                                                             max.cross.selection = 0,
                                                                             applied.insecticide.dose = 1,
                                                                             recommended.insecticide.dose = 1,
                                                                             threshold.generation = 10,
                                                                             base.efficacy.decay.rate = 0, #no decay
                                                                             rapid.decay.rate = 0, #no decay
                                                                             intercept = 0.15,
                                                                             conversion.factor = 0.48,
                                                                             insecticide.suppression = TRUE
  ) , 500, 2)
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  deployment.interval = parameter.space$deployment.interval[v]
  strategy = "sequence"
  
  temp_2 = data.frame(simulation.duration, strategy, average.resistance.intensity, peak.resistance)
  
  temp.list.sequence[[v]] = temp_2
}
#Convert to a single dataframe
sequence.df = do.call(rbind, temp.list.sequence)
sequence.df.complete = cbind(sequence.df, parameter.space)


#First run for rotations 
temp.list.rotation = list()
for(v in 1:nrow(parameter.space)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention_special_cases(number.of.insecticides = 2,
                                                                             exposure.scaling.factor = 10,
                                                                             nsim = 1,
                                                                             minimum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                                             maximum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                                             minimum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                                             maximum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                                             minimum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                                             maximum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                                             resistance.cost = parameter.space$Fitness.Cost[v],
                                                                             starting.treatment.site.intensity = 0,
                                                                             starting.refugia.intensity = 0,
                                                                             min.intervention.coverage = parameter.space$Intervention.Coverage[v],
                                                                             max.intervention.coverage = parameter.space$Intervention.Coverage[v],
                                                                             min.dispersal.rate = parameter.space$Dispersal[v],
                                                                             max.dispersal.rate = parameter.space$Dispersal[v],
                                                                             maximum.generations = 500, #appoximately 50 years
                                                                             irm.strategy = "rotation", 
                                                                             half.population.bioassay.survival.resistance = 900, 
                                                                             withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                             return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                             deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                             maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                             min.cross.selection = 0,
                                                                             max.cross.selection = 0,
                                                                             applied.insecticide.dose = 1,
                                                                             recommended.insecticide.dose = 1,
                                                                             threshold.generation = 10,
                                                                             base.efficacy.decay.rate = 0, #no decay
                                                                             rapid.decay.rate = 0, #no decay
                                                                             intercept = 0.15,
                                                                             conversion.factor = 0.48,
                                                                             insecticide.suppression = TRUE
  ) , 500, 2)
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  deployment.interval = parameter.space$deployment.interval[v]
  strategy = "rotation"
  
  temp_2 = data.frame(simulation.duration, strategy, average.resistance.intensity, peak.resistance)
  
  temp.list.rotation[[v]] = temp_2
}

#Convert to a single dataframe
rotation.df = do.call(rbind, temp.list.rotation)
rotation.df.complete = cbind(rotation.df, parameter.space)

seq.rot.df = rbind(sequence.df.complete, rotation.df.complete)
write.csv(seq.rot.df, ".//set.19.rotations.sequences.csv")

#Then do mixtures:
temp.list.mixtures = list()
for(v in 1: nrow(parameter.space)){
  
  temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_mixtures_special_cases(number.of.insecticides = 2,
                                                                                               exposure.scaling.factor = 10,
                                                                                               nsim = 1,
                                                                                      minimum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                                                      maximum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                                                      minimum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                                                      maximum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                                                      minimum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                                                      maximum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                                                      resistance.cost = parameter.space$Fitness.Cost[v],
                                                                                      starting.treatment.site.intensity = 0,
                                                                                      starting.refugia.intensity = 0,
                                                                                      min.intervention.coverage = parameter.space$Intervention.Coverage[v],
                                                                                      max.intervention.coverage = parameter.space$Intervention.Coverage[v],
                                                                                      min.dispersal.rate = parameter.space$Dispersal[v],
                                                                                      max.dispersal.rate = parameter.space$Dispersal[v],
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
                                                                                      intercept = 0.15,
                                                                                      applied.insecticide.dose = 1,
                                                                                      recommended.insecticide.dose = 1 ,
                                                                                      threshold.generation = 10 ,
                                                                                      base.efficacy.decay.rate = 0,
                                                                                      rapid.decay.rate = 0,
                                                                                      insecticide.suppression = TRUE), 
                                            maximum.generations = 500, number.of.insecticides = 2)                                              
  
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  #note, as in mixture will be the same for mixture part 1 and mixture part 2 - only count based on part1.
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = c(2)
  
  average.resistance.intensity.1 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  average.resistance.intensity.2 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  strategy = "mixture"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim)
  
  temp.list.mixtures[[v]] = temp_2
}
  
mixture.df = do.call(rbind, temp.list.mixtures)
mixture.df.1 = cbind(mixture.df, parameter.space)

write.csv(mixture.df.1, ".//set.19.mixtures.csv")

