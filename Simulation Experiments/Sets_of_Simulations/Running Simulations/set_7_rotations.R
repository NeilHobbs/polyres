#Set 7:rotations::: The return threshold has been increased from 5% to 9%. 

parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[,2:9]

temp.list.rotation = list() #create empty list to hold the loop

#First run for rotations [45000 runs]
for(v in 1:nrow(parameter.space)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = parameter.space$Number.of.Insecticides[v],
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
                                                               return.threshold.value = 0.09, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = parameter.space$Deployment.Interval[v], #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, parameter.space$Number.of.Insecticides[v])
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = parameter.space$Number.of.Insecticides[v]
  
  dep.freq = parameter.space$Deployment.Interval[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  strategy = "rotation"
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)
  
  temp.list.rotation[[v]] = temp_2
}

#Convert to a single dataframe
rotation.df.set7 = do.call(rbind, temp.list.rotation)
rotation.df.set7.complete = cbind(rotation.df.set7, parameter.space)

write.csv(rotation.df.set7.complete, ".//rotation.set7.csv")



