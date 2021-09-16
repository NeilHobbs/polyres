#Looking at the impact of the deployment interval.
#SET 16
#1. load in the required R packages:
library(devtools)
load_all() #for polyres

#Create a small (~1000) lhs data set:
parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")[1:5000, 2:7]

deployment.interval = rep(c(2, 5, 10, 20, 30), 5000)

parameter.space = rbind(parameter.space, parameter.space, parameter.space, parameter.space, parameter.space)
parameter.space$deployment.interval = deployment.interval


#First run for sequences 
temp.list.sequence = list()
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
                                                               deployment.frequency = parameter.space$deployment.interval[v], #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
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
  
  temp_2 = data.frame(simulation.duration, strategy, deployment.interval, average.resistance.intensity, peak.resistance)
  
  temp.list.sequence[[v]] = temp_2
}

#Convert to a single dataframe
sequence.df = do.call(rbind, temp.list.sequence)
sequence.df.complete = cbind(sequence.df, parameter.space)

#then do rotations
temp.list.rotation= list()
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
                                                               deployment.frequency = parameter.space$deployment.interval[v], #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
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
  temp_2 = data.frame(simulation.duration, strategy, deployment.interval, average.resistance.intensity, peak.resistance)
  
  temp.list.rotation[[v]] = temp_2
}

#Convert to a single dataframe
rotation.df = do.call(rbind, temp.list.rotation)
rotation.df.complete = cbind(rotation.df, parameter.space)

dataset.df = rbind(rotation.df.complete, sequence.df.complete)

write.csv(dataset.df, ".\\set.16.sequences.rotations.csv")
