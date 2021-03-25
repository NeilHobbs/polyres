#Set 3: Mixtures

#read in previously used random parameters
parameter.space = read.csv("Simulation Experiments/Sets_of_Simulations/Setting up Simulations/parameter.space.csv")
parameter.space = parameter.space[1:5000,2:7] #only need the first 5000 rows

deployment.interval = c(rep(5, 5000), rep(10, 5000), rep(20, 5000))
parameter.space = rbind(parameter.space, parameter.space, parameter.space)


parameter.space.df = cbind(parameter.space, deployment.interval)


#Do the rotation Runs
temp.list.mixtures = list()
for(v in 1: nrow(parameter.space.df)){
  
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
                                                                             starting.treatment.site.intensity = 0,
                                                                             starting.refugia.intensity = 0,
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
                                                                             return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                             deployment.frequency = parameter.space.df$deployment.interval[v], #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                             maximum.resistance.value = 25000,
                                                                             conversion.factor = 0.48,
                                                                             intercept = 0.15), 
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
  
  dep.freq = parameter.space.df$deployment.interval[v]

  average.resistance.intensity.1 = c(temp_treatment%>%
                                     dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
                                     summarise(mean(resistance.intensity)))
  
  average.resistance.intensity.2 = c(temp_treatment%>%
                                       dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
                                       summarise(mean(resistance.intensity)))
  
  strategy = "mixture"
  
  temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations, 
                      exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq)
  
  temp.list.mixtures[[v]] = temp_2
}

mixture.df = do.call(rbind, temp.list.mixtures)
mixture.df.1 = cbind(mixture.df, parameter.space.df)

write.csv(mixture.df.1, ".//mixtures.set.3.csv")

