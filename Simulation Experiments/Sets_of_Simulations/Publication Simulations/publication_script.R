#Document for the running of simulations and data analysis used for publication:

#install required packages

#load required packages
library(magrittr)
library(devtools)
load_all()

# ##Step 1: Create a data frame for the parameter space: ##hashed out to prevent re-sampling
# 
# parameter.space.df = data.frame(lhs::randomLHS(20000, 6)) #20,000 random samples of the 6 input parameters.
# 
# 
# #Rename and sample from the uniform distributions
# parameter.space.df = parameter.space.df%>%
#   dplyr::rename(Heritability = X1)%>%
#   dplyr::rename(`Male Insecticide Exposure` = X2)%>%
#   dplyr::rename(`Female Insecticide Exposure` = X3)%>%
#   dplyr::rename(`Fitness Cost` = X4)%>%
#   dplyr::rename(`Intervention Coverage` = X5)%>%
#   dplyr::rename(Dispersal = X6)%>%
#   dplyr::mutate(Heritability = qunif(Heritability, 0.05, 0.3))%>%
#   dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))%>% #
#   dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))%>% #Defaults from Ian
#   dplyr::mutate(`Fitness Cost` = qunif(`Fitness Cost`, 0.01, 0.2))%>%
#   dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.1, 0.9))%>%
#   dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))
# 
# #use cross selection values of -0.5 to 0.5 (by 0.1)
# cross.selection.values = seq(-0.5, 0.5, 0.1)
# cross.selection.values = rep(cross.selection.values, 20000)
# cross.selection.values = sort(cross.selection.values, decreasing = TRUE)
# start.resistance.values = c(rep(0, 220000), rep(50, 220000))
# cross.selection.values = c(cross.selection.values, cross.selection.values)
# 
# parameter.space.df = do.call("rbind", replicate(22, parameter.space.df, simplify = FALSE))
# parameter.space.df$cross.selection.values = cross.selection.values
# parameter.space.df$start.resistance.values = start.resistance.values
# 
# 
# write.csv(parameter.space.df, "paramater.space.df.publication.csv")

#read in the dataset
parameter.space.df = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/paramater.space.df.publication.csv")


#Do the sequence Runs
temp.list.sequence = list()
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
                                                                               starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
                                                                               starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
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
                                                                               min.cross.selection = parameter.space.df$cross.selection.values[v],
                                                                               max.cross.selection = parameter.space.df$cross.selection.values[v]), 
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
  
  strategy = "sequence"
  
  dep.freq = c(10)
  
  cross.selection = parameter.space.df$cross.selection.values[v]
  
  start.resistance = parameter.space.df$start.resistance.values[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "sequence"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
                      cross.selection, start.resistance)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space.df))  
  
  temp.list.sequence[[v]] = temp_2
}

sequence.df = do.call(rbind, temp.list.sequence)
sequence.df.publication = cbind(sequence.df, parameter.space.df)

write.csv(sequence.df.publication, ".//sequence.set.publication.csv")


#Do the Rotation Runs
temp.list.rotation = list()
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
                                                                               starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
                                                                               starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
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
                                                                               min.cross.selection = parameter.space.df$cross.resistance[v],
                                                                               max.cross.selection = parameter.space.df$cross.resistance[v]), 
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
  
  cross.selection = parameter.space.df$cross.selection.values[v]
  
  start.resistance = parameter.space.df$start.resistance.values[v]
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "rotation"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
                      cross.selection, start.resistance)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space.df))  
  
  temp.list.rotation[[v]] = temp_2
}

rotation.df = do.call(rbind, temp.list.rotation)
rotation.df.publication = cbind(rotation.df, parameter.space.df)

write.csv(rotation.df.publication, ".//sequence.set.publication.csv")


#Then do mixtures::
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
                                                                                                      starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
                                                                                                      starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
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
                                                                                                      conversion.factor = 0.48, #point estimates from linear regression
                                                                                                      intercept = 0.15, #point estimates from linear regression
                                                                                                      min.cross.selection = parameter.space.df$cross.selection.values[v],
                                                                                                      max.cross.selection = parameter.space.df$cross.selection.values[v]), 
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
  
  cross.selection = parameter.space.df$cross.selection.values[v]
  
  start.resistance = parameter.space.df$start.resistance.values[v]
  
  average.resistance.intensity.1 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  average.resistance.intensity.2 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  strategy = "mixture"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq, cross.selection, start.resistance)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space.df))  
  
  temp.list.mixtures.set.5[[v]] = temp_2
}

mixture.df = do.call(rbind, temp.list.mixtures)
mixture.df.publication = cbind(mixture.df, parameter.space.df)

write.csv(mixture.df.publication, ".//mixture.df.publication.csv")








