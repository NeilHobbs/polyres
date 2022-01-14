#Set 33
#Testing the impact of cross selection in the absence of fitness costs but with refugia 
#for sequences, rotations and mixtures:

#read in parameter space dataframe

parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[1:5000, 2:7]

cross.selection = c(rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000), rep(0, 5000), rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000))
parameter.space.df = rbind(parameter.space, parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space)
parameter.space.df$cross.selection = cross.selection
parameter.space.df$Fitness.Cost = 0




#Do the Sequence Runs
temp.list.sequence.cross = list()
for(v in 1: nrow(parameter.space.df)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                               maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                               minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                               maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                               minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                               maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                               resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                               max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                               min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                               max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                               maximum.generations = 500, #appoximately 50 years
                                                                               irm.strategy = "sequence", 
                                                                               half.population.bioassay.survival.resistance = 900, 
                                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                               return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                               maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                               min.cross.selection = parameter.space.df$cross.selection[v],
                                                                               max.cross.selection = parameter.space.df$cross.selection[v]), 
                                   maximum.generations = 500, number.of.insecticides = 2)                                              
  
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = c(2)
  
  dep.freq = c(10)
  
  cross.resistance = parameter.space.df$cross.selection[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "sequence"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
                      cross.resistance)
  
  temp.list.sequence.cross[[v]] = temp_2
}

sequence.df.cross = do.call(rbind, temp.list.sequence.cross)
sequence.df.cross.1 = cbind(sequence.df.cross, parameter.space.df)
write.csv(sequence.df.cross.1, ".//sequence.set.33.csv")

#Do the rotation Runs
temp.list.rotation.cross = list()
for(v in 1: nrow(parameter.space.df)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                               maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                               minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                               maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                               minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                               maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                               resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                               max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                               min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                               max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                               maximum.generations = 500, #appoximately 50 years
                                                                               irm.strategy = "rotation", 
                                                                               half.population.bioassay.survival.resistance = 900, 
                                                                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                               return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                               maximum.resistance.value = 25000, #have arbitrarily high just in case
                                                                               min.cross.selection = parameter.space.df$cross.selection[v],
                                                                               max.cross.selection = parameter.space.df$cross.selection[v]), 
                                   maximum.generations = 500, number.of.insecticides = 2)                                              
  
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = c(2)
  
  dep.freq = c(10)
  
  cross.resistance = parameter.space.df$cross.selection[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "rotation"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
                      cross.resistance)
  
  temp.list.rotation.cross[[v]] = temp_2
}

rotation.df.cross = do.call(rbind, temp.list.rotation.cross)
rotation.df.cross.1 = cbind(rotation.df.cross, parameter.space.df)
write.csv(rotation.df.cross.1, ".//rotation.set.33.csv")


#Mixtures
temp.list.mixtures = list()
for(v in 1:nrow(parameter.space.df)){
  
  temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
                                                                                                      exposure.scaling.factor = 10,
                                                                                                      nsim = 1,
                                                                                                      minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                                      maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                                      minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                      maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                      minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                      maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                      resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                                                      starting.treatment.site.intensity = 50,
                                                                                                      starting.refugia.intensity = 50,
                                                                                                      min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                      max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                      min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                      max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                      maximum.generations = 500,
                                                                                                      irm.deployment.strategy = "mixtures", #single, mixtures
                                                                                                      mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
                                                                                                      irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
                                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                                      return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                                      deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                                      maximum.resistance.value = 25000,
                                                                                                      conversion.factor = 0.48,
                                                                                                      intercept = 0.15,
                                                                                                      min.cross.selection = parameter.space.df$cross.selection[v],
                                                                                                      max.cross.selection = parameter.space.df$cross.selection[v]), 
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
  
  dep.freq = c(10)
  
  cross.selection = parameter.space.df$cross.selection[v]
  
  average.resistance.intensity.1 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  average.resistance.intensity.2 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  strategy = "mixture"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq, cross.selection)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space.df))  
  
  temp.list.mixtures[[v]] = temp_2
}

mixture.df = do.call(rbind, temp.list.mixtures)
mixture.df.1 = cbind(mixture.df, parameter.space.df)

write.csv(mixture.df.1, ".//mixtures.set.33.csv")

