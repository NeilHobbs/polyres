#Set 6: Should new insecticides be deployed alone or in mixtures with insecticides where there is already resistance.

#read in previously used random parameters
parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space.df = parameter.space[1:5000,2:9] #only need the first 5000 rows

#Do the sequence Runs
temp.list.sequence.set.6 = list()
for(v in 1: nrow(parameter.space.df)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = 1,
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
                                                               maximum.generations = 200, #appoximately 50 years
                                                               irm.strategy = "sequence", 
                                                               half.population.bioassay.survival.resistance = 900, 
                                                               withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                               return.threshold.value = 1, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = 200, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000),
                                   maximum.generations = 500, number.of.insecticides = 1)                                              
  
  
  temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
    dplyr::filter(site == "treatment")
  
  simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
  
  exceedance.generations = nrow(temp_treatment%>%
                                  dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
  
  exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
                                           dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                           dplyr::filter(resistance.intensity > 100))
  
  peak.resistance = max(temp_treatment$resistance.intensity)
  
  insecticides.in.sim = c(1)
  
  dep.freq = c(200)
  
  average.resistance.intensity = c(temp_treatment%>%
                                     dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  strategy = "sequence"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space.df))  
  
  temp.list.sequence.set.6[[v]] = temp_2
}

sequence.df.6= do.call(rbind, temp.list.sequence.set.6)
sequence.df.set.6 = cbind(temp.list.sequence.set.6, parameter.space.df)

write.csv(sequence.df.set.6, ".//sequence.set.6.csv")

#0 = 0%
#47 = 5%
# 100 = 10%
# 225 = 20%
# 600 = %40%
# 900 = 50%
# 2100 = 70%
# 8100 = 90%
# 89100 =99%

parameter.space.df = rbind(parameter.space.df, parameter.space.df,parameter.space.df,
                           parameter.space.df,parameter.space.df,parameter.space.df,
                           parameter.space.df,parameter.space.df,parameter.space.df)

start.resistance.1 = c(rep(0, 5000), rep(47, 5000), rep(100, 5000), rep(225, 5000),
                     rep(600, 5000), rep(900, 5000), rep(2100, 5000), rep(8100, 5000),
                    rep(89100, 5000))




temp.list.mixtures.set.6 = list()
for(v in 1:nrow(parameter.space.df)){
  
  temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures(number.of.insecticides = 2,
                                                                                                      exposure.scaling.factor = 10,
                                                                                                      nsim = 1,
                                                                                                      minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                                      maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
                                                                                                      minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                      maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                                      minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                      maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                                      resistance.cost = parameter.space.df$Fitness.Cost[v],
                                                                                                      starting.treatment.site.intensity = c(start.resistance.1[v], 0),
                                                                                                      starting.refugia.intensity = c(start.resistance.1[v], 0),
                                                                                                      min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                      max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                                      min.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                      max.dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                                      maximum.generations = 200,
                                                                                                      irm.deployment.strategy = "mixtures", #single, mixtures
                                                                                                      mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
                                                                                                      irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
                                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                                      withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                                      return.threshold.value = 1, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                                      deployment.frequency = 200, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                                      maximum.resistance.value = 500000000, #set 
                                                                                                      conversion.factor = 0.48,
                                                                                                      intercept = 0.15), 
                                            maximum.generations = 200, number.of.insecticides = 2)                                              
  
  
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
  
  dep.freq = c(200)
  
  
  average.resistance.intensity.1 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  average.resistance.intensity.2 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  strategy = "mixture"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)
  
  svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
                   max.value = nrow(parameter.space.df))  
  
  temp.list.mixtures.set.6[[v]] = temp_2
}

mixture.df.set.6 = do.call(rbind, temp.list.mixtures.set.6)
mixture.df.set.6$part.1.start = start.resistance.1

mixture.df.set.6.1 = cbind(mixture.df.set.6, parameter.space.df)

write.csv(mixture.df.set.6.1, ".//mixtures.set.6.csv")






