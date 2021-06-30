#Set 14: different starting resistance


parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[1:5000, 2:7]

parameter.space = rbind(parameter.space,
                        parameter.space,
                        parameter.space,
                        parameter.space,
                        parameter.space,
                        parameter.space,
                        parameter.space,
                        parameter.space,
                        parameter.space,
                        parameter.space)

start.resistance = rep(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90), 5000)

parameter.space$start.resistance = start.resistance


temp.list.rotation = list() #create empty list to hold the loop

#First run for rotations [30000 runs]
for(v in 1:nrow(parameter.space)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 2,
                                                               exposure.scaling.factor = 10,
                                                               nsim = 1,
                                                               minimum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                               maximum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                               minimum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                               maximum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                               minimum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                               maximum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                               resistance.cost = parameter.space$Fitness.Cost[v],
                                                               starting.treatment.site.intensity = parameter.space$start.resistance[v],
                                                               starting.refugia.intensity = parameter.space$start.resistance[v],
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
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, 2)
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  start.resist = parameter.space$start.resistance[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  strategy = "rotation"
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, start.resist)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space))  
  
  temp.list.rotation[[v]] = temp_2
}

#Convert to a single dataframe
rotation.df.set14 = do.call(rbind, temp.list.rotation)
rotation.df.set14.complete = cbind(rotation.df.set14, parameter.space)

write.csv(rotation.df.set14.complete, ".//rotation.set14.csv")



###Next do the sequences
temp.list.sequence = list() #create empty list to hold the loop

for(v in 1:nrow(parameter.space)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 2,
                                                               exposure.scaling.factor = 10,
                                                               nsim = 1,
                                                               minimum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                               maximum.insecticide.resistance.heritability = parameter.space$Heritability[v],
                                                               minimum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                               maximum.male.insecticide.exposure = parameter.space$Male.Insecticide.Exposure[v],
                                                               minimum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                               maximum.female.insecticide.exposure = parameter.space$Female.Insecticide.Exposure[v],
                                                               resistance.cost = parameter.space$Fitness.Cost[v],
                                                               starting.treatment.site.intensity = parameter.space$start.resistance[v],
                                                               starting.refugia.intensity = parameter.space$start.resistance[v],
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
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, 2)
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  start.resist = parameter.space$start.resistance[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  strategy = "sequence"
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, start.resist)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space))  
  
  temp.list.sequence[[v]] = temp_2
}

#Convert to a single dataframe
sequence.df.set14 = do.call(rbind, temp.list.sequence)
sequence.df.set14.complete = cbind(sequence.df.set14, parameter.space)

write.csv(sequence.df.set14.complete, ".//sequence.set14.csv")



