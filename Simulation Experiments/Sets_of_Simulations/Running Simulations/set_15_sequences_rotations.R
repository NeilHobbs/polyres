#Defining the simulation rules:::
#SET 16
#1. load in the required R packages:
library(devtools)
load_all() #for polyres

#Create a small (~1000) lhs data set:
parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")[1:5000, 2:7]


return.threshold = c(rep(0.05, 5000), rep(0.06, 5000), rep(0.07, 5000), rep(0.08, 5000), rep(0.09, 5000), rep(0.099, 5000))
withdrawal.threshold = rep(0.1, 30000)

parameter.space = rbind(parameter.space, parameter.space, parameter.space, parameter.space, parameter.space, parameter.space)
parameter.space$return.threshold = return.threshold
parameter.space$withdrawal.threshold = withdrawal.threshold


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
                                                               return.threshold.value = parameter.space$return.threshold[v], #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, 2)
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  return.thresh = parameter.space$return.threshold[v]
  strategy = "sequence"
  temp_2 = data.frame(simulation.duration, strategy, return.thresh)
  
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
                                                               return.threshold.value = parameter.space$return.threshold[v], #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, 2)
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     dplyr::summarise(mean(resistance.intensity)))
  
  return.thresh = parameter.space$return.threshold[v]
  strategy = "rotation"
  temp_2 = data.frame(simulation.duration, strategy, return.thresh)
  
  temp.list.rotation[[v]] = temp_2
}

#Convert to a single dataframe
rotation.df = do.call(rbind, temp.list.rotation)
rotation.df.complete = cbind(rotation.df, parameter.space)

dataset.df = rbind(rotation.df.complete, sequence.df.complete)

#write.csv(dataset.df, "C:\\Users\\neilp\\OneDrive - LSTM\\PhD Project\\DATASETS\\polyres_1A.csv")

dataset.df = read.csv("C:/Users/neilp/OneDrive - LSTM/PhD Project/DATASETS/polyres_1A.csv")
library(magrittr)
library(ggplot2)

rotation.df.complete = dataset.df%>%
  dplyr::filter(strategy == "rotation")

sequence.df.complete = dataset.df%>%
  dplyr::filter(strategy == "sequence")

rotation.duration = rotation.df.complete$simulation.duration
sequence.duration = sequence.df.complete$simulation.duration
return.threshold = as.character(sequence.df.complete$return.threshold)

ratio.difference.duration = rotation.duration/sequence.duration

threshold.df = data.frame(rotation.duration,
                          sequence.duration,
                          return.threshold,
                          ratio.difference.duration)



ggplot(threshold.df, aes(x=sequence.duration,
                         y=rotation.duration,
                         colour = return.threshold))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1,
              colour = "black",
              size = 2,
              alpha = 0.4)+
  geom_abline(intercept = 0, slope = 1.1,
              colour = "red",
              size = 2,
              alpha = 0.4)+
  geom_abline(intercept = 0, slope = 1.2,
              colour = "blue",
               size =2,
              alpha = 0.4)+
  facet_wrap(~return.threshold)+
  ylab("Rotation Simulation Duration")+
  xlab("Sequence Simulation Duration")+
  theme_classic()

ggplot(threshold.df, aes(x=ratio.difference.duration))+
  geom_histogram(bins = 100)+
  facet_wrap(~return.threshold)+
  theme_classic()


#Need to find a return value that is both operationally realistic, 
  #but also not overly inflating a single control strategy. 
  #8% bioassay survival appears to meet these two criteria. 
